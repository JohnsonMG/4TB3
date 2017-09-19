"""
Pascal0 Code Generator for ARMv8, Emil Sekerinski, Gabriel Dalimonte, Gavin Johnson, March 2017.
Using delayed code generation for a one-pass compiler. The types of symbol
table entries for expressions, Var, Ref, Const, are extended by two more
types Reg for expression results in a register, and Cond, for short-circuited
Boolean expressions with two branch targets.
"""

import SC  #  used for SC.error
from SC import TIMES, DIV, MOD, AND, PLUS, MINUS, OR, EQ, NE, LT, GT, LE, \
     GE, NOT, mark
from ST import Var, Ref, Const, Type, Proc, StdProc, Int, Bool, Array

# w31's value is context dependent based on the instruction
# FP, SP, and LNK need to be x reg since we're dealing with a 64-bit memory space
ZR = 'wzr'; FP = 'x29'; SP = 'sp'; LNK = 'x30'  # reserved registers

class Reg:
    """
    For integers or booleans stored in a register;
    register can be $0 for constants '0' and 'false'
    """
    def __init__(self, tp, reg):
        self.tp, self.reg = tp, reg

class Cond:
    """
    For a boolean resulting from comparing left and right by cond:
    left, right are either registers or constants, but one has to be a register;
    cond is one of 'EQ', 'NE', 'LT', 'GT', 'LE', 'GE';
    labA, labB are lists of branch targets for when the result is true or false
    if right is $0, then cond 'EQ' and 'NE' can be used for branching depending
    on register left.
    """
    count = 0
    def __init__(self, cond, left, right):
        self.tp, self.cond, self.left, self.right = Bool, cond, left, right
        self.labA = ['.C' + str(Cond.count)]; Cond.count += 1
        self.labB = ['.C' + str(Cond.count)]; Cond.count += 1

class DeferredBlock:
    """
    Builds a deferred block which will allow for deferred cvode generation
    of array operations to have a desitnation register.
    The notion is the concept of a deferred block chain
    For the current use of deferred blocks (for SIMD) bunching
    the deferred assignment generates the loop prologue and epilogue
    Then each func in function chain is called appending the actions
    of the previous. The return from func is the output register
    """
    def __init__(self, tp, func, i, reg, bc, tar):
        self.func = [func]
        self.tp = tp
        self.i = i
        self.reg = reg
        self.bc = bc
        self.tar = tar

# curlev is the current level of nesting of procedures
# regs is the set of available registers for expression evaluation
# asm is a list of triples; each triple consists of three strings
# - a label
# - an instruction, possibly with operands
# - a target (for branch and jump instructions)
# each of them can be the empty string

def obtainReg():
    if len(regs) == 0: mark('out of registers'); return ZR
    else: return regs.pop()

def obtainVectorReg():
    if len(vregs) == 0: mark('out of SIMD registers'); return ZR
    else: return vregs.pop()

def releaseReg(r):
    if r not in (ZR, SP, FP, LNK): (regs if r[0] == 'w' or r[0] == 'x' else vregs).add(r)

def putLab(lab, instr = ''):
    """Emit label lab with optional instruction; lab may be a single
    label or a list of labels"""
    if type(lab) == list:
        for l in lab[:-1]: asm.append((l, '', ''))
        asm.append((lab[-1], instr, ''))
    else: asm.append((lab, instr, ''))

def putInstr(instr, target = ''):
    """Emit an instruction"""
    asm.append(('', instr, target))

def put(op, a, b, *args):
    """Emit instruction op with three operands, a, b, args"""
    c = ''
    for i in args:
        c += ', ' + str(i)
    putInstr(op + ' ' + a + ', ' + str(b) + c)

def putB(op, a, b, c):
    putInstr(op + ' ' + a + ', ' + str(b), str(c))

def putM(op, a, b, c=ZR):
    """Emit load/store instruction at location or register b + offset or register c"""
    if b == ZR:
        b = c
        c = ZR
    b_rep = 'x' + b[1:] if b != SP else b
    if c == ZR:
        putInstr(op + ' ' + a + ', [' + b_rep + ']')
    else:
        if type(c) == int:
            prefix = "#"
            c = str(c)
        else:
            prefix = ''
            c = 'x' + c[1:] if c != SP else c
        putInstr(op + ' ' + a + ', [' + b_rep + ', ' + prefix + c + ']')

def testRange(x):
    """Check if x is suitable for immediate addressing"""
    # This is kept simple. Although AArch64 may support >12 bit immediate through shifting,
    # it is assumed that will not occur. Similarly, an immediate may be generated which is
    # negative by swapping the instruction.
    if x.val >= 0x0FFF or x.val < 0: mark('value too large')

def loadAddressOfLabel(x, r=None):
    s = obtainReg()
    put('adrp', 'x' + s[1:], x)
    put('add', s, s, ':lo12:' + x)
    if r is not None:
        put('add', r, r, s)
        releaseReg(s)
        s = r
    return s
    
def loadItemReg(x, r):
    """Assuming item x is Var, Const, or Reg, loads x into register r"""
    if type(x) == Var:
        if type(x.adr) == str:
            s = loadAddressOfLabel(x.adr)
            putM('ldr', r, x.reg, s); releaseReg(s); releaseReg(x.reg)
        elif type(x.adr) == int:
            putM('ldr', r, x.reg, x.adr)
            #putInstr('ldr ' + r + ', [' + x.reg + ', #'+ str(x.adr) + ']')
            releaseReg(x.reg)
    elif type(x) == Const:
        testRange(x); put('mov', r, '#' + str(x.val))
    elif type(x) == Reg: # move to register r
        put('mov', r, x.reg)
    else: assert False

def loadItem(x):
    """Assuming item x is Var or Const, loads x into a new register and
    returns a new Reg item"""
    if type(x) == Const and x.val == 0: r = ZR # use ZR for "0"
    else: r = obtainReg(); loadItemReg(x, r)
    return Reg(x.tp, r)

def loadBool(x):
    """Assuming x is Var or Const and x has type Bool, loads x into a
    new register and returns a new Cond item"""
    # improve by allowing c.left to be a constant
    if type(x) == Const and x.val == 0: r = ZR # use ZR for "false"
    else: r = obtainReg(); loadItemReg(x, r)
    c = Cond(NE, r, ZR)
    return c

def putOp(cd, x, y, **kwargs):
    """For operation op with mnemonic cd, emit code for x op y, assuming
    x, y are Var, Const, Reg"""
    if type(x) != Reg: x = loadItem(x)
    if x.reg == ZR: x.reg, r = obtainReg(), ZR
    else: r = x.reg # r is source, x.reg is destination
    if type(y) == Const and (('imm' in kwargs and kwargs['imm']) or 'imm' not in kwargs):
        testRange(y); put(cd, r, x.reg, '#' + str(y.val))
    else:
        if type(y) != Reg: y = loadItem(y)
        put(cd, x.reg, r, y.reg); releaseReg(y.reg)
    return x

def assembly(l, i, t):
    """Convert label l, instruction i, target t to assembly format"""
    return (l + ':\t' if l else '\t') + i + (', ' + t if t else '')

# public functions

def init():
    """initializes the code generator"""
    global asm, curlev, regs, vregs
    asm, curlev = [], 0
    regs = {('w' + str(i)) for i in range(9,16)}
    vregs = {('v' + str(i)) for i in range(31)}
                                
def genRec(r):
    """Assuming r is Record, determine fields offsets and the record size"""
    s = 0
    for f in r.fields:
        f.offset, s = s, s + f.tp.size
    r.size = s
    return r

def genArray(a):
    """Assuming r is Array, determine its size"""
    # adds size
    a.size = a.length * a.base.size
    return a

def genLocalVars(sc, start):
    """For list sc of local variables, starting at index start, determine the
    $fp-relative addresses of variables"""
    s = 0 # local block size
    for i in range(start, len(sc)):
        if type(sc[i]) == Var:
            s = s + sc[i].tp.size
            sc[i].adr = - s - 16
    return s

def genGlobalVars(sc, start):
    """For list sc of global variables, starting at index start, determine the
    address of each variable, which is its name with a trailing _"""
    for i in range(len(sc) - 1, start - 1, - 1):
        if type(sc[i]) == Var:
            sc[i].adr = sc[i].name + '_'
            putLab(sc[i].adr, '.space ' + str(sc[i].tp.size))

def progStart():
    putInstr('.data')

def progEntry(ident):
    putInstr('.text')
    putInstr('.global main')
    #putInstr('.entry')
    putLab('main')

def progExit(x):
    put('mov', 'w8', '#93')  # w8 for exit
    put('mov', 'w0', 'wzr')  # w0 is exit code
    putInstr('svc #0')
    putInstr("nop")
    #putInstr('li $v0, 10')
    #putInstr('syscall')
    #putInstr('.end main')
    return '\n'.join(assembly(l, i, t) for (l, i, t) in asm)
        
def procStart():
    global curlev, parblocksize
    curlev = curlev + 1
    putInstr('.text')

def genFormalParams(sc):
    """For list sc with formal procedure parameters, determine the $fp-relative
    address of each parameters; each parameter must be type integer, boolean
    or must be a reference parameter"""
    # Stack is 16-byte aligned, so calculate the needed padding
    s = 16-len(sc)*4 &0xF # parameter block size
    for p in reversed(sc):
        if p.tp == Int or p.tp == Bool or type(p) == Ref:
            p.adr, s = s, s + 4
        else: mark('no structured value parameters')
    return s

# Stack frames are structured so FP and LNK go between consecutive "user" frames
# Everything outside those two values being between frames is up to compiler
# interpretation
def genProcEntry(ident, parsize, localsize):
    """Declare procedure name, generate code for procedure entry"""
    putInstr('.global ' + ident)        # global declaration directive
    putLab(ident)                      # procedure entry label
    # TODO: AArch64 calling conventions
    #putM('sw', FP, SP, - parsize - 4)  # push frame pointer
    #putM('sw', LNK, SP, - parsize - 8) # push return address
    # TODO: Handle reg passed values
    # TODO: Par size is set in caller
    put('mov', FP, SP)  # Set new FP
    put('sub', SP, FP, '#' + str((localsize+0xF)&~0xF))  # set stack pointer making room for locals

def genProcExit(x, parsize, localsize): # generates return code
    global curlev
    curlev = curlev - 1
    put('mov', SP, FP)  # Locals out of scope
    putInstr("ret")

def genSelect(x, f):
    # x.f, assuming y is name in one of x.fields
    x.tp = f.tp
    if type(x) != Reg:
        if type(x.adr) == int:
            x.adr += f.offset
            if x.reg != ZR:
                s = x.reg
            else:
                s = obtainReg()
            put('add', s, s, '#' + str(x.adr))
            x = Reg(x.tp, s)
        else:
            s = loadAddressOfLabel(x.adr)
            put('add', s, s, '#' + str(f.offset))
            x = Reg(x.tp, s)
    else:
        put('add', x.reg, x.reg, '#' + str(f.offset))
    return x

def genIndex(x, y):
    # x[y], assuming x is ST.Var or ST.Ref, x.tp is ST.Array, y.tp is ST.Int
    # assuming y is Const and y.val is valid index, or Reg integer
    if type(y) == Const and type(x) != Reg:
        offset = (y.val - x.tp.lower) * x.tp.base.size
        if type(x.adr) == int:
            x.adr += offset
            if x.reg != ZR:
                s = x.reg
            else:
                s = obtainReg()
            put('add', s, s, '#' + str(x.adr))
            x = Reg(x.tp, s)
        else:
            s = loadAddressOfLabel(x.adr)
            put('add', s, s, '#' + str(offset))
            x = Reg(x.tp, s)
        #x.adr = x.adr + (offset if type(x.adr) == int else '+' + str(offset))
    else:
        if type(x) != Reg:
            if type(x.adr) == str:
                x = Reg(x.tp, loadAddressOfLabel(x.adr))
            else:
                if x.reg != ZR:
                    s = x.reg
                else:
                    s = obtainReg()
                put('add', s, s, '#' + str(x.adr))
                x = Reg(x.tp, s)
        if type(y) != Reg: y = loadItem(y)
        put('sub', y.reg, y.reg, '#' + str(x.tp.lower))
        r = obtainReg()
        put('mov', r, '#' + str(x.tp.base.size))
        if x.reg != ZR:
            put('madd', y.reg, r, y.reg, x.reg); releaseReg(x.reg)
        else:
            put('mul', y.reg, y.reg, r)
        releaseReg(r)
        x.reg = y.reg
    x.tp = x.tp.base
    return x

def genVar(x):  # TODO, ref is in mem, need offset from FP to get arg to load TODO TODO
    # assuming x is ST.Var, ST.Ref, ST.Const
    # for ST.Const: no code, x.val is constant
    # for ST.Var: x.reg is FP for local, 0 for global vars,
    #   x.adr is relative or absolute address
    # for ST.Ref: address is loaded into register
    # returns ST.Var, ST.Const
    if type(x) == Const: y = x
    else:
        if x.lev == 0: s = ZR
        elif x.lev == curlev: s = FP
        else: mark('level!'); s = ZR
        y = Var(x.tp); y.lev = x.lev
        if type(x) == Ref: # reference is loaded into register relative to SP
            r = obtainReg()
            #putInstr("ldr " + r + ", [" + s + ", #" + str(x.adr+16) +"]")
            putM('ldr', r, s, x.adr+16)
            y.reg, y.adr = r, 0
        elif type(x) == Var:
            y.reg, y.adr = FP if s != ZR else ZR, x.adr
            if type(y.adr) == int and y.reg == FP:
                y.adr += 16 # 16 for gap between FP and FP' and LNK'
            #print(y.reg)
            #print(y.adr)
        else: y = x # error, pass dummy item
    return y

def genConst(x):
    # assumes x is ST.Const
    return x

def genUnaryOp(op, x):
    """If op is MINUS, NOT, x must be an Int, Bool, and op x is returned.
    If op is AND, OR, x is the first operand (in preparation for the second
    operand"""
    if op == MINUS: # subtract from 0
        if type(x) == Var: x = loadItem(x)
        put('neg', x.reg, x.reg)
    elif op == NOT: # switch condition and branch targets, no code
        if type(x) != Cond: x = loadBool(x)
        x.cond = negate(x.cond); x.labA, x.labB = x.labB, x.labA
    elif op == AND: # load first operand into register and branch
        if type(x) != Cond: x = loadBool(x)
        #putB(condOp(negate(x.cond)), x.left, x.right, x.labA[0])
        put('cmp', x.left, x.right)
        putInstr(condOp(negate(x.cond)) + " " + x.labA[0])
        releaseReg(x.left); releaseReg(x.right); putLab(x.labB)
    elif op == OR: # load first operand into register and branch
        if type(x) != Cond: x = loadBool(x)
        #putB(condOp(x.cond), x.left, x.right, x.labB[0])
        put('cmp', x.left, x.right)
        putInstr(condOp(x.cond) + " " + x.labB[0])
        releaseReg(x.left); releaseReg(x.right); putLab(x.labA)
    else: assert False
    return x

def genBinaryOp(op, x, y):
    """assumes x.tp == Int == y.tp and op is TIMES, DIV, MOD
    or op is AND, OR"""
    if op == PLUS: y = putOp('add', x, y)
    elif op == MINUS: y = putOp('sub', x, y)
    elif op == TIMES: y = putOp('mul', x, y, imm=False)
    elif op == DIV: y = putOp('sdiv', x, y, imm=False)
    elif op == MOD:
        q = putOp('sdiv', x, y)  # Generates quotient
        # y and x are relased at this point. BOTH need to be loaded into
        # Reg because msub does NOT take imm operands.
        x = loadItem(x)
        x = loadItem(y)
        put('msub', q, q, y, x)
        releaseReg(y)
        releaseReg(x)
        y = q
    elif op == AND: # load second operand into register 
        if type(y) != Cond: y = loadBool(y)
        y.labA += x.labA # update branch targets
    elif op == OR: # load second operand into register
        if type(y) != Cond: y = loadBool(y)
        y.labB += x.labB # update branch targets
    else: assert False
    return y

def negate(cd):
    """Assume cd in {EQ, NE, LT, LE, GT, GE}, return not cd"""
    return NE if cd == EQ else \
           EQ if cd == NE else \
           GE if cd == LT else \
           GT if cd == LE else \
           LE if cd == GT else \
           LT

def condOp(cd):
    """Assumes cd in {EQ, NE, LT, LE, GT, GE}, return instruction mnemonic"""
    return 'beq' if cd == EQ else \
           'bne' if cd == NE else \
           'blt' if cd == LT else \
           'ble' if cd == LE else \
           'bgt' if cd == GT else \
           'bge'

def genRelation(cd, x, y):
    """Assumes x, y are Int and cd is EQ, NE, LT, LE, GT, GE;
    x and y cannot be both constants; return Cond for x cd y"""
    if type(x) != Reg: x = loadItem(x)
    if type(y) != Reg: y = loadItem(y)
    return Cond(cd, x.reg, y.reg)

assignCount = 0

def genAssign(x, y):
    """Assume x is Var, generate x := y"""
    global assignCount, regs
    if type(y) == Cond:
        #putB(condOp(negate(y.cond)), y.left, y.right, y.labA[0])
        put('cmp', y.left, y.right)
        putInstr(condOp(negate(y.cond)) + " " + y.labA[0])
        releaseReg(y.left); releaseReg(y.right); r = obtainReg()
        putLab(y.labB); put('mov', r, '#1') # load true
        lab = '.A' + str(assignCount); assignCount += 1
        putInstr('B ' + lab)
        putLab(y.labA); put('mov', r, '#0') # load false 
        putLab(lab)
    elif type(y) != Reg: 
        y = loadItem(y); r = y.reg
    else: r = y.reg
    # Need to load x to reg because of label
    if x.reg == ZR:
        # TODO: Int's here mean stack var
        #print(x.adr)
        s = loadAddressOfLabel(x.adr)
    else:
        s = x.reg
    #print(x.reg)
    #putInstr('str ' + r + ', [x' + s[1:] + ']')
    if x.reg == FP:
        putM('str', r, s, x.adr)
    else:
        putM('str', r, s)
    releaseReg(s) 
    releaseReg(r)


def genActualPara(ap, fp, n):  # TODO
    """Pass parameter, ap is actual parameter, fp is the formal parameter,
    either Ref or Var, n is the parameter number"""
    if type(fp) == Ref and type(ap) != Reg:  #  reference parameter, assume p is Var
        if ap.adr != 0:  #  load address in register
            r = loadAddressOfLabel(ap.adr)
        else: r = ap.reg  #  address already in register
        #putInstr('str ' + r + ', [' + SP + ', #' + str(-16 - 4*(n+1)) + ']')  # -16 for the padding between frames
        putM('str', r, SP,  - 4*(n+1))
        releaseReg(r)
    elif type(ap) == Reg:
        putM('str', ap.reg, SP,- 4*(n+1))
        releaseReg(ap.reg)
    else:  #  value parameter
        if type(ap) != Cond:
            if type(ap) != Reg: ap = loadItem(ap)
            #putInstr('str ' + ap.reg + ', [' + SP + ', #' + str(-16 - 4*(n+1)) + ']') 
            putM('str', ap.reg, SP, - 4*(n+1))
            releaseReg(ap.reg)
        else: mark('unsupported parameter type')

def genCall(pr):
    """Assume pr is Proc"""
    put('sub', SP, SP, '#' + str((len(pr.par) + 0xF)&~0xF))       # Bring the set variables in to scope; end the previous stack frame; make it divisible by 0x10 rounded up
    putInstr('stp ' + FP + ", " + LNK + ", [" + SP + ", #-16]!")  # Save FP and LNK, should be before the call...
    putInstr('bl ' + pr.name)
    putInstr('ldp ' + FP + ", " + LNK + ", [" + SP  + "], #16")  # Load old FP and LNK
    put('add', SP, SP, '#' + str((len(pr.par) + 0xF)&~0xF))  # Pop off args

def genRead(x):
    """Assume x is Var"""
    """Assume that there is a layer which converts an input integer into an equivalent 4-byte number, similar to MIPS syscall"""
    put('mov', 'w8', '#63')  # w8 is the svc reg thing
    put('mov', 'w0', 'wzr')  # fd
    put('adrp', 'x1', str(x.adr))
    put('add', 'x1', 'x1', ':lo12:' + str(x.adr))  # char*
    put('mov', 'w2', '#4')  # len
    putInstr('svc #0')
    #putInstr('li $v0, 5'); putInstr('syscall')
    #putM('sw', '$v0', x.reg, x.adr)

def genWrite(x):
    """Assumes x is Ref, Var, Reg"""
    """Assume writing 4 bytes to stdout will be translated to an integer similar to the MIPS syscall"""
    put('mov', 'w8', '#64')  # write syscall
    put('mov', 'w0', '#1')  # fd
    if type(x) == Var:
        #put('adrp', 'x1', str(x.adr))
        #put('add', 'x1', 'x1', ':lo12:' + str(x.adr))  # char*
        if x.reg == ZR:
            r = loadAddressOfLabel(x.adr)
        else:
            r = x.reg
        put('mov', 'x1', 'x' + r[1:])
        if x.reg == FP:
            put('add', 'x1', 'x1', x.adr)
        releaseReg(r)
    elif type(x) == Reg:
        #put('sub', SP, SP, '#16')
        #putM('str', x.reg, SP)
        put('mov', 'x1', ('x' + x.reg[1:]) if x.reg != SP else SP)
        releaseReg(x.reg)
    elif type(x) == Const:
        put('sub', SP, SP, '#16')  # Make stack room
        r = obtainReg();
        put('mov', r, x.val)
        putM('sturb', r, ZR, SP)
        put('mov', 'x1', SP)
        releaseReg(r)
    put('mov', 'w2', '#4')  # len
    putInstr('svc #0')
    if type(x) == Const:
        put('add', SP, SP, '#16')
    #loadItemReg(x, '$a0'); putInstr('li $v0, 1'); putInstr('syscall')

def genWriteln():
    # Store \n char on stack to use as buf
    put('mov', 'w8', '#64')  # write syscall
    put('mov', 'w0', '#1')  # fd
    put('sub', SP, SP, '#16')  # Make stack room
    # Place \n on stack
    r = obtainReg();
    put('mov', r, '#10')
    putM('sturb', r, ZR, SP)
    put('mov', 'x1', SP)
    put('mov', 'w2', '#1')  # len
    putInstr('svc #0')
    put('add', SP, SP, '#16')
    releaseReg(r)

    #putInstr('li $v0, 11'); putInstr("li $a0, '\\n'"); putInstr('syscall')

def genSeq(x, y):
    """Assume x and y are statements, generate x ; y"""
    pass

def genCond(x):
    """Assume x is Bool, generate code for branching on x"""
    if type(x) != Cond: x = loadBool(x)
    put('cmp', x.left, x.right)
    putInstr(condOp(negate(x.cond)) + " " + x.labA[0])
    #putB(condOp(negate(x.cond)), x.left, x.right, x.labA[0])
    releaseReg(x.left); releaseReg(x.right); putLab(x.labB)
    return x

def genIfThen(x, y):
    """Generate code for if-then: x is condition, y is then-statement"""
    putLab(x.labA)

ifCount = 0

def genThen(x, y):
    """Generate code for if-then-else: x is condition, y is then-statement"""
    global ifCount
    lab = '.I' + str(ifCount); ifCount += 1
    putInstr('b ' + lab)
    putLab(x.labA); 
    return lab

def genIfElse(x, y, z):
    """Generate code of if-then-else: x is condition, y is then-statement,
    z is else-statement"""
    putLab(y)

loopCount = 0

def genTarget():
    """Return target for loops with backward branches"""
    global loopCount
    lab = '.L' + str(loopCount); loopCount += 1
    putLab(lab)
    return lab

def genWhile(lab, x, y):
    """Generate code for while: lab is target, x is condition, y is body"""
    putInstr('b ' + lab)
    putLab(x.labA); 

# SIMD codegen below

simd_loop = False
simd_deferred_block = None

def putSIMDLoopPrologue(x):
    global simd_loop
    global simd_deferred_block
    if not simd_loop:
        inc = obtainReg()
        bc = obtainReg()
        put("mov", inc, 'wzr')
        length = genArray(x.tp)
        put("mov", bc, '#' + str(length.size//4-((length.size//4)&3)))  # Hack because all data types are 4 bytes
        tar = genTarget()
        inc_ret = Reg(Int, inc)
        simd_deferred_block = inc_ret
    else:
        bc = 0
        tar = ""
    if simd_loop:
        db = DeferredBlock(x.tp, [], simd_deferred_block, None, bc, tar)
    else:
        db = DeferredBlock(x.tp, [], inc_ret, None, bc, tar)
    simd_loop = True
    return db

def genDeferredAssign(x, y):  # TODO: support for residual and merge into genAssign
    global simd_loop
    #ret = None
    #for i in range(len(y.func)):
    #    if i > 0:
    #        ret = y.func[i](inc, ret)
    #    else:
    #        ret = y.func[i](inc)
    x_loc = obtainReg()
    tmp = obtainReg()
    put('adrp', 'x' + x_loc[1:], x.adr)
    put('mov', tmp, '#4')
    put('madd', x_loc, tmp, y.i.reg, x_loc)
    put('add', x_loc, x_loc, ':lo12:' + x.adr)
    putM('st1', '{' + y.reg + '.4S}', ZR, x_loc)
    #x_loc = 'w' + x_loc[1:]
    releaseReg(x_loc)
    releaseReg(tmp)
    put("add", y.i.reg, y.i.reg, '#4')
    put("cmp", y.i.reg, y.bc)
    putInstr('blt ' + y.tar)
    for i in range(x.tp.length&3):
        ret = None
        for j in range(len(y.func)):
            if j > 0:
                ret = y.func[j](x.tp.length//4*4+i, ret)
            else:
                ret = y.func[j](x.tp.length//4*4+i)
        x_loc = obtainReg()
        put('adrp', 'x' + x_loc[1:], x.adr)
        put('add', x_loc, x_loc, '#' + str(4*(x.tp.length//4*4+i)))
        put('add', x_loc, x_loc, ':lo12:' + x.adr)
        putM('str', ret.reg, x_loc)
        releaseReg(x_loc)
        releaseReg(ret.reg)
        #x.tp.length//4*4+i
    releaseReg(y.i.reg)
    releaseReg(y.bc)
    simd_loop = False

def loadLabelIntoVectorRegister(x, idx):
    # idx is a register
    t = obtainVectorReg()
    x_loc = obtainReg()
    tmp = obtainReg()
    put('adrp', 'x' + x_loc[1:], x.adr)
    put('mov', tmp, '#4')
    put('madd', x_loc, tmp, idx, x_loc)
    put('add', x_loc, x_loc, ':lo12:' + x.adr)
    putM('ld1', '{' + t + '.4S}', ZR, x_loc)
    releaseReg(tmp)
    releaseReg(x_loc)
    return t

def loadLabelAndSpreadIntoVectorRegister(x):
    x_loc = loadItem(x)
    x_loc = x_loc.reg
    scalar_op = obtainVectorReg()
    put('dup', scalar_op + '.4S', x_loc)
    releaseReg(x_loc)
    return scalar_op

def loadSingleArrayElementForSIMDOp(x, idx):
    # idx is a 0 based "traditional" array index
    x_loc = obtainReg()
    t = obtainReg()
    put('adrp', 'x' + x_loc[1:], x.adr)
    if type(x.tp) == Array:
        put('add', x_loc, x_loc, '#' + str(4*idx))
    put('add', x_loc, x_loc, ':lo12:' + x.adr)
    putM('ldr', t, x_loc)
    releaseReg(x_loc)
    return t

def genArrayVectorOp(op, x, y):
    if type(x) != DeferredBlock and type(y) != DeferredBlock:
        ret = putSIMDLoopPrologue(x if type(x.tp) == Array else y)
    elif type(x) == DeferredBlock:
        ret = x
    elif type(y) == DeferredBlock:
        ret = y
    if type(x) == DeferredBlock:
        iterate = x.i
        t = x.reg
    elif type(x.tp) == Array:
        t = loadLabelIntoVectorRegister(x, ret.i.reg)
    if type(y) == DeferredBlock:
        iterate = y.i
        s = y.reg
    elif type(y.tp) == Array:
        s = loadLabelIntoVectorRegister(y, ret.i.reg)
    opcode = 'nop'
    if op == TIMES: opcode = 'mul'
    elif op == DIV: opcode = 'fdiv'
    elif op == PLUS: opcode = 'add'
    elif op == MINUS: opcode = 'sub'
    elif op == MOD: opcode = 'fdiv'
    elif op == AND: opcode = 'smin'  # Signed minimum, result is only 1 iff a and b are 1
    elif op == OR: opcode = 'smax'  # Signed maximum, result is only 1 iff a or b is 1
    elif op == NOT: opcode = 'uabd'  # Unsigned absolute difference y should ALWAYS be 1 so 1 -1 = 0 0 - 1 = abs(-1)
    else: assert False
    if op == DIV:
        #convert from int to float
        put('scvtf', t + '.4S', t + '.4S')
        put('scvtf', s + '.4S', s + '.4S')
        #floating point divide
        put(opcode, t + '.4S', t + '.4S', s + '.4S')
        #convert from float to int
        put('fcvtzs', t + '.4S', t + '.4S')
    elif op == MOD:
        #convert from int to float
        put('scvtf', t + '.4S', t + '.4S')
        put('scvtf', s + '.4S', s + '.4S')
        #floating point divide
        q = obtainVectorReg()
        put(opcode, q + '.4S', t + '.4S', s + '.4S')
        #convert from float to int
        put('fcvtzs', s + '.4S', s + '.4S')
        put('fcvtzs', t + '.4S', t + '.4S')
        put('fcvtzs', q + '.4S', q + '.4S')
        put('mul', q + '.4S', q + '.4S', s + '.4S')
        put('sub', t + '.4S', t + '.4S', q + '.4S')
        releaseReg(q)
    else: put(opcode, t + '.4S', t + '.4S', s + '.4S')
    releaseReg(s)
    def deferred_block(op, iterate, x, y):
        # Op is the op to do
        # iterate is a constant from the residual counter, the array size is known at compile-time thus a constant may be inserted and residual unrolled
        if type(x) == Reg:
            t = x.reg
        else:
            t = loadSingleArrayElementForSIMDOp(x, iterate)
        if type(y) == Reg:
            s = y.reg
        elif type(y) == DeferredBlock:
            ret = None
            for j in range(len(y.func)):
                if j > 0:
                    ret = y.func[j](iterate, ret)
                else:
                    ret = y.func[j](iterate)
            s = ret.reg
        else:
            s = loadSingleArrayElementForSIMDOp(y, iterate)
        if op == TIMES: opcode = 'mul'
        elif op == DIV: opcode = 'sdiv'
        elif op == PLUS: opcode = 'add'
        elif op == MINUS: opcode = 'sub'
        elif op == MOD: opcode = 'sdiv'
        elif op == AND: opcode = 'and'
        elif op == OR: opcode = 'orr'
        elif op == NOT: opcode = 'eor'
        else: assert False
        if op == MOD:
            q = obtainReg()
            put(opcode, q, t, s)  # Generates quotient
            # y and x are relased at this point. BOTH need to be loaded into
            # Reg because msub does NOT take imm operands.
            put('msub', q, q, s, t)
            releaseReg(t)
            t = q
        else:
            put(opcode, t, t, s)
        releaseReg(s)
        return Reg(x.tp, t)
    if type(x) == DeferredBlock:
        x.func.append(lambda iterate, z: deferred_block(op, iterate, z, y))
        x.reg = t
        a = x
    else:
        if type(y) == DeferredBlock:
            ret = y
            ret.func.append(lambda iterate, z: deferred_block(op, iterate, x, z))
        else:
            ret.func = [lambda iterate: deferred_block(op, iterate, x, y)]
        ret.reg = t
        a = ret
    return a

def genArrayScalarOp(op, x, y):
    if type(x) != DeferredBlock and type(y) != DeferredBlock:
        ret = putSIMDLoopPrologue(x if type(x.tp) == Array else y)
    elif type(x) == DeferredBlock:
        ret = x
    elif type(y) == DeferredBlock:
        ret = y
    if type(x) == DeferredBlock:
        iterate = x.i
        t = x.reg
    elif type(x.tp) == Array:
        t = loadLabelIntoVectorRegister(x, ret.i.reg)
    else:
        t = loadLabelAndSpreadIntoVectorRegister(x)
    if type(y) == DeferredBlock:
        iterate = y.i
        scalar_op = y.reg
    elif type(y.tp) == Array:
        scalar_op = loadLabelIntoVectorRegister(y, ret.i.reg)
    else:
        scalar_op = loadLabelAndSpreadIntoVectorRegister(y)
    opcode = 'nop'
    if op == TIMES: opcode = 'mul'
    elif op == DIV: opcode = 'fdiv'
    elif op == PLUS: opcode = 'add'
    elif op == MINUS: opcode = 'sub'
    elif op == MOD: opcode = 'fdiv'
    elif op == AND: opcode = 'smin'  # Signed minimum, result is only 1 iff a and b are 1
    elif op == OR: opcode = 'smax'  # Signed maximum, result is only 1 iff a or b is 1
    elif op == NOT: opcode = 'uabd'  # Unsigned absolute difference y should ALWAYS be 1 so 1 -1 = 0 0 - 1 = abs(-1)
    else: assert False
    s = obtainVectorReg()
    if op == DIV:
        #convert from int to float
        put('scvtf', t + '.4S', t + '.4S')
        put('scvtf', scalar_op + '.4S', scalar_op + '.4S')
        #floating point divide
        put(opcode, s + '.4S', t + '.4S', scalar_op + '.4S')
        #convert from float to int
        put('fcvtzs', s + '.4S', s + '.4S')
    elif op == MOD:
        #convert from int to float
        put('scvtf', t + '.4S', t + '.4S')
        put('scvtf', scalar_op + '.4S', scalar_op + '.4S')
        #floating point divide
        put(opcode, s + '.4S', t + '.4S', scalar_op + '.4S')
        #convert from float to int
        put('fcvtzs', s + '.4S', s + '.4S')
        put('fcvtzs', t + '.4S', t + '.4S')
        put('fcvtzs', scalar_op + '.4S', scalar_op + '.4S')
        q = obtainVectorReg()
        put('mul', q + '.4S', s + '.4S', scalar_op + '.4S')
        put('sub', s + '.4S', t + '.4S', q + '.4S')
        releaseReg(q)
    else: put(opcode, s + '.4S', t + '.4S', scalar_op + '.4S')
    releaseReg(scalar_op)
    releaseReg(t)
    def deferred_block(op, iterate, x, y):
        # Op is the op to do
        # iterate is a constant from the residual counter, the array size is known at compile-time thus a constant may be inserted and residual unrolled
        if type(x) == Reg:
            t = x.reg
        elif type(x) == Const:
            t = obtainReg()
            put('mov', t, '#' + str(x.val))
        else:
            t = loadSingleArrayElementForSIMDOp(x, iterate)
        if type(y) == Reg:
            s = y.reg
        elif type(y) == Const:
            s = obtainReg()
            put('mov', s, '#' + str(y.val))
        else:
            s = loadSingleArrayElementForSIMDOp(y, iterate)
        if op == TIMES: opcode = 'mul'
        elif op == DIV: opcode = 'sdiv'
        elif op == PLUS: opcode = 'add'
        elif op == MINUS: opcode = 'sub'
        elif op == MOD: opcode = 'sdiv'
        elif op == AND: opcode = 'and'
        elif op == OR: opcode = 'orr'
        elif op == NOT: opcode = 'eor'
        else: assert False
        if op == MOD:
            q = obtainReg()
            put(opcode, q, t, s)  # Generates quotient
            # y and x are relased at this point. BOTH need to be loaded into
            # Reg because msub does NOT take imm operands.
            put('msub', q, q, s, t)
            releaseReg(t)
            t = q
        else:
            put(opcode, t, t, s)
        releaseReg(s)
        return Reg(x.tp, t)
    if type(x) == DeferredBlock:
        x.func.append(lambda iterate, z: deferred_block(op, iterate, z, y))
        x.reg = s
        a = x
    else:
        if type(y) == DeferredBlock:
            ret = y
            ret.func.append(lambda iterate, z: deferred_block(op, iterate, x, z))
        else:
            ret.func = [lambda iterate: deferred_block(op, iterate, x, y)]
        ret.reg = s
        a = ret
    return a