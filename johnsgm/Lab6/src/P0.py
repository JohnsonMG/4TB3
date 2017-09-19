"""
Pascal0 Parser, Emil Sekerinski, February 2017,
Main program, type-checks, folds constants, calls scanner SC and code
generator CG, uses symbol table ST
"""

from sys import argv
import SC  #  used for SC.init, SC.sym, SC.val, SC.error
from SC import TIMES, DIV, MOD, AND, PLUS, MINUS, OR, EQ, NE, LT, GT, \
     LE, GE, PERIOD, COMMA, COLON, RPAREN, RBRAK, OF, THEN, DO, LPAREN, \
     LBRAK, NOT, BECOMES, NUMBER, IDENT, SEMICOLON, END, ELSE, IF, WHILE, \
     ARRAY, RECORD, CONST, TYPE, VAR, PROCEDURE, BEGIN, PROGRAM, EOF, \
     getSym, mark
import ST  #  used for ST.init
from ST import Var, Ref, Const, Type, Proc, StdProc, Int, Bool, Enum, \
     Record, Array, newObj, find, openScope, topScope, closeScope
import CGmips as CG  #  used for CG.init


# first and follow sets for recursive descent parsing

FIRSTFACTOR = {IDENT, NUMBER, LPAREN, NOT}
FOLLOWFACTOR = {TIMES, DIV, MOD, AND, OR, PLUS, MINUS, EQ, NE, LT, LE, GT, GE,
                COMMA, SEMICOLON, THEN, ELSE, RPAREN, RBRAK, DO, PERIOD, END}
FIRSTEXPRESSION = {PLUS, MINUS, IDENT, NUMBER, LPAREN, NOT}
FIRSTSTATEMENT = {IDENT, IF, WHILE, BEGIN}
FOLLOWSTATEMENT = {SEMICOLON, END, ELSE}
FIRSTTYPE = {IDENT, RECORD, ARRAY, LPAREN}
FOLLOWTYPE = {SEMICOLON}
FIRSTDECL = {CONST, TYPE, VAR, PROCEDURE}
FOLLOWDECL = {BEGIN}
FOLLOWPROCCALL = {SEMICOLON, END, ELSE}
STRONGSYMS = {CONST, TYPE, VAR, PROCEDURE, WHILE, IF, BEGIN, EOF}

# parsing procedures

def selector(x):
    """
    Parses
        selector = {"." ident | "[" expression "]"}.
    Assumes x is the entry for the identifier in front of the selector;
    generates code for the selector if no error is reported
    """
    while SC.sym in {PERIOD, LBRAK}:
        if SC.sym == PERIOD:  #  x.f
            getSym()
            if SC.sym == IDENT:
                if type(x.tp) == Record:
                    for f in x.tp.fields:
                        if f.name == SC.val:
                            x = CG.genSelect(x, f); break
                    else: mark("not a field")
                    getSym()
                else: mark("not a record")
            else: mark("identifier expected")
        else:  #  x[y]
            getSym(); y = expression()
            if type(x.tp) == Array:
                if y.tp == Int:
                    if type(y) == Const and \
                       (y.val < x.tp.lower or y.val >= x.tp.lower + x.tp.length):
                        mark('index out of bounds')
                    else: x = CG.genIndex(x, y)
                else: mark('index not integer')
            else: mark('not an array')
            if SC.sym == RBRAK: getSym()
            else: mark("] expected")
    return x

def factor():
    """
    Parses
        factor = ident selector | integer | "(" expression ")" | "not" factor.
    Generates code for the factor if no error is reported
    """
    if SC.sym not in FIRSTFACTOR:
        mark("expression expected"); getSym()
        while SC.sym not in FIRSTFACTOR | STRONGSYMS | FOLLOWFACTOR:
            getSym()
    if SC.sym == IDENT:
        x = find(SC.val)
        if type(x) in {Var, Ref}: x = CG.genVar(x)
        elif type(x) == Const: x = Const(x.tp, x.val); x = CG.genConst(x)
        else: mark('expression expected')
        getSym(); x = selector(x)
    elif SC.sym == NUMBER:
        x = Const(Int, SC.val); x = CG.genConst(x); getSym()
    elif SC.sym == LPAREN:
        getSym(); x = expression()
        if SC.sym == RPAREN: getSym()
        else: mark(") expected")
    elif SC.sym == NOT:
        getSym(); x = factor()
        if x.tp != Bool: mark('not boolean')
        elif type(x) == Const: x.val = 1 - x.val # constant folding
        else: x = CG.genUnaryOp(NOT, x)
    else: x = Const(None, 0)
    return x

def term():
    """
    Parses
        term = factor {("*" | "div" | "mod" | "and") factor}.
    Generates code for the term if no error is reported
    """
    x = factor()
    while SC.sym in {TIMES, DIV, MOD, AND}:
        op = SC.sym; getSym();
        if op == AND and type(x) != Const: x = CG.genUnaryOp(AND, x)
        y = factor() # x op y
        if x.tp == Int == y.tp and op in {TIMES, DIV, MOD}:
            if type(x) == Const == type(y): # constant folding
                if op == TIMES: x.val = x.val * y.val
                elif op == DIV: x.val = x.val // y.val
                elif op == MOD: x.val = x.val % y.val
            else: x = CG.genBinaryOp(op, x, y)
        elif x.tp == Bool == y.tp and op == AND:
            if type(x) == Const: # constant folding
                if x.val: x = y # if x is true, take y, else x
            else: x = CG.genBinaryOp(AND, x, y)
        else: mark('bad type')
    return x

def simpleExpression():
    """
    Parses
        simpleExpression = ["+" | "-"] term {("+" | "-" | "or") term}.
    Generates code for the simpleExpression if no error is reported
    """
    if SC.sym == PLUS:
        getSym(); x = term()
    elif SC.sym == MINUS:
        getSym(); x = term()
        if x.tp != Int: mark('bad type')
        elif type(x) == Const: x.val = - x.val # constant folding
        else: x = CG.genUnaryOp(MINUS, x)
    else: x = term()
    while SC.sym in {PLUS, MINUS, OR}:
        op = SC.sym; getSym()
        if op == OR and type(x) != Const: x = CG.genUnaryOp(OR, x)
        y = term() # x op y
        if x.tp == Int == y.tp and op in {PLUS, MINUS}:
            if type(x) == Const == type(y): # constant folding
                if op == PLUS: x.val = x.val + y.val
                elif op == MINUS: x.val = x.val - y.val
            else: x = CG.genBinaryOp(op, x, y)
        elif x.tp == Bool == y.tp and op == OR:
            if type(x) == Const: # constant folding
                if not x.val: x = y # if x is false, take y, else x
            else: x = CG.genBinaryOp(OR, x, y)
        else: mark('bad type')
    return x

def expression():
    """
    Parses
        expression = simpleExpression
                     {("=" | "<>" | "<" | "<=" | ">" | ">=") simpleExpression}.
    Generates code for the expression if no error is reported
    """
    x = simpleExpression()
    while SC.sym in {EQ, NE, LT, LE, GT, GE}:
        op = SC.sym; getSym(); y = simpleExpression() # x op y
        if (x.tp == Int == y.tp) or (type(x.val) == type(y.val)): ### MODIFIED BY GAVIN JOHNSON###############
            x = CG.genRelation(op, x, y)
        else: mark('bad type')
    return x

def compoundStatement():
    """
    Parses
        compoundStatement = "begin" statement {";" statement} "end"
    Generates code for the compoundStatement if no error is reported
    """
    if SC.sym == BEGIN: getSym()
    else: mark("'begin' expected")
    x = statement()
    while SC.sym == SEMICOLON or SC.sym in FIRSTSTATEMENT:
        if SC.sym == SEMICOLON: getSym()
        else: mark("; missing")
        y = statement(); x = CG.genSeq(x, y)
    if SC.sym == END: getSym()
    else: mark("'end' expected")
    return x

def statement():
    """
    Parses
        statement = ident selector ":=" expression |
                    ident "(" [expression {"," expression}] ")" |
                    compoundStatement |
                    "if" expression "then" statement ["else"statement] |
                    "while" expression "do" statement.
    Generates code for the statement if no error is reported
    """
    if SC.sym not in FIRSTSTATEMENT:
        mark("statement expected"); getSym()
        while SC.sym not in FIRSTSTATEMENT | STRONGSYMS | FOLLOWSTATEMENT:
            getSym()
    if SC.sym == IDENT:
        x = find(SC.val); getSym(); x = CG.genVar(x)
        if type(x) in {Var, Ref}:
            x = selector(x)
            if SC.sym == BECOMES:
                getSym(); y = expression()
                if x.tp == y.tp: #in {Bool, Int}: # and not SC.error: type(y) could be Type 
                    #if type(x) == Var: ### and type(y) in {Var, Const}: incomplete, y may be Reg
                        x = CG.genAssign(x, y)
                    #else: mark('illegal assignment')
                else: mark('incompatible assignment')
            elif SC.sym == EQ:
                mark(':= expected'); getSym(); y = expression()
            else: mark(':= expected')
        elif type(x) in {Proc, StdProc}:
            fp, i = x.par, 0  #  list of formals, count of actuals
            if SC.sym == LPAREN:
                getSym()
                if SC.sym in FIRSTEXPRESSION:
                    y = expression()
                    if i < len(fp):
                        if (type(fp[i]) == Var or type(y) == Var) and \
                           fp[i].tp == y.tp:
                            if type(x) == Proc: CG.genActualPara(y, fp[i], i)
                            i = i + 1
                        else: mark('illegal parameter mode')
                    else: mark('extra parameter')
                    while SC.sym == COMMA:
                        getSym()
                        y = expression()
                        if i < len(fp):
                            if (type(fp[i]) == Var or type(y) == Var) and \
                               fp[i].tp == y.tp:
                                if type(x) == Proc: CG.genActualPara(y, fp[i], i)
                                i = i + 1
                            else: mark('illegal parameter mode')
                        else: mark('extra parameter')
                if SC.sym == RPAREN: getSym()
                else: mark("')' expected")
            if i < len(fp): mark('too few parameters')
            if type(x) == StdProc:
                if x.name == 'read': x = CG.genRead(y)
                elif x.name == 'write': x = CG.genWrite(y)
                elif x.name == 'writeln': x = CG.genWriteln()
            else: x = CG.genCall(x)
        else: mark("variable or procedure expected")
    elif SC.sym == BEGIN: x = compoundStatement()
    elif SC.sym == IF:
        getSym(); x = expression();
        if x.tp == Bool: x = CG.genCond(x)
        else: mark('boolean expected')
        if SC.sym == THEN: getSym()
        else: mark("'then' expected")
        y = statement()
        if SC.sym == ELSE:
            if x.tp == Bool: y = CG.genThen(x, y);
            getSym(); z = statement();
            if x.tp == Bool: x = CG.genIfElse(x, y, z)
        else:
            if x.tp == Bool: x = CG.genIfThen(x, y)
    elif SC.sym == WHILE:
        getSym(); t = CG.genTarget(); x = expression()
        if x.tp == Bool: x = CG.genCond(x)
        else: mark('boolean expected')
        if SC.sym == DO: getSym()
        else: mark("'do' expected")
        y = statement()
        if x.tp == Bool: x = CG.genWhile(t, x, y)
    else: x = None
    return x

def typ():
    """
    Parses
        type = ident |
               "array" "[" expression ".." expression "]" "of" type |
               "record" typedIds {";" typedIds} "end".
    Returns a type descriptor 
    """
    if SC.sym not in FIRSTTYPE:
        getSym(); mark("type expected")
        while SC.sym not in FIRSTTYPE | STRONGSYMS | FOLLOWTYPE:
            getSym()
    if SC.sym == IDENT:
        ident = SC.val; x = find(ident); getSym()
        if isinstance(x, Type): x = Type(x.tp)
        else: mark('not a type'); x = Type(None)
    #ADDED CODE by Gavin Johnson########################
    elif SC.sym == LPAREN:
        enum = Enum([], 4)
        getSym()
        i = 0
        if SC.sym == IDENT:
            enum.elements.append(SC.val); newObj(SC.val, CG.genConst(Const(enum, i)))
            getSym()
        else: mark("identifier expected")
        while SC.sym == COMMA:
            i = i + 1
            getSym()
            if SC.sym == IDENT: 
                enum.elements.append(SC.val); newObj(SC.val, CG.genConst(Const(enum, i)))
                getSym()
            else: mark("identifier expected")
        if SC.sym == RPAREN: getSym()
        else: mark("')' expected. recieved "+str(SC.sym))
        x = Type(enum)
    #END ADDED CODE#####################################
    elif SC.sym == ARRAY:
        getSym()
        if SC.sym == LBRAK: getSym()
        else: mark("'[' expected")
        x = expression()
        if SC.sym == PERIOD: getSym()
        else: mark("'.' expected")
        if SC.sym == PERIOD: getSym()
        else: mark("'.' expected")
        y = expression()
        if SC.sym == RBRAK: getSym()
        else: mark("']' expected")
        if SC.sym == OF: getSym()
        else: mark("'of' expected")
        z = typ().tp;
        if type(x) != Const or x.val < 0:
            mark('bad lower bound'); x = Type(None)
        elif type(y) != Const or y.val < x.val:
            mark('bad upper bound'); y = Type(None)
        else: x = Type(CG.genArray(Array(z, x.val, y.val - x.val + 1)))
    elif SC.sym == RECORD:
        getSym(); openScope(); typedIds(Var)
        while SC.sym == SEMICOLON:
            getSym(); typedIds(Var)
        if SC.sym == END: getSym()
        else: mark("'end' expected")
        r = topScope(); closeScope()
        x = Type(CG.genRec(Record(r)))
    else: x = Type(None)
    return x

def typedIds(kind):
    """
    Parses
        typedIds = ident {"," ident} ":" type.
    Updates current scope of symbol table
    Assumes kind is Var or Ref and applies it to all identifiers
    Reports an error if an identifier is already defined in the current scope
    """
    if SC.sym == IDENT: tid = [SC.val]; getSym()
    else: mark("identifier expected"); tid = []
    while SC.sym == COMMA:
        getSym()
        if SC.sym == IDENT: tid.append(SC.val); getSym()
        else: mark('identifier expected')
    if SC.sym == COLON:
        getSym(); tp = typ().tp
        if tp != None:
            for i in tid: newObj(i, kind(tp))
    else: mark("':' expected")

def declarations(allocVar):
    """
    Parses
        declarations =
            {"const" ident "=" expression ";"}
            {"type" ident "=" type ";"}
            {"var" typedIds ";"}
            {"procedure" ident ["(" [["var"] typedIds {";" ["var"] typedIds}] ")"] ";"
                declarations compoundStatement ";"}.
    Updates current scope of symbol table.
    Reports an error if an identifier is already defined in the current scope.
    For each procedure, code is generated
    """
    if SC.sym not in FIRSTDECL | FOLLOWDECL:
        getSym(); mark("'begin' or declaration expected")
        while SC.sym not in FIRSTDECL | STRONGSYMS | FOLLOWDECL: getSym()
    while SC.sym == CONST:
        getSym()
        if SC.sym == IDENT:
            ident = SC.val; getSym()
            if SC.sym == EQ: getSym()
            else: mark("= expected")
            x = expression()
            if type(x) == Const: newObj(ident, x)
            else: mark('expression not constant')
        else: mark("constant name expected")
        if SC.sym == SEMICOLON: getSym()
        else: mark("; expected")
    while SC.sym == TYPE:
        getSym()
        if SC.sym == IDENT:
            ident = SC.val; getSym()
            if SC.sym == EQ: getSym()
            else: mark("= expected")
            x = typ(); newObj(ident, x)  #  x is of type ST.Type
            if SC.sym == SEMICOLON: getSym()
            else: mark("; expected")
        else: mark("type name expected")
    start = len(topScope())
    while SC.sym == VAR:
        getSym(); typedIds(Var)
        if SC.sym == SEMICOLON: getSym()
        else: mark("; expected")
    varsize = allocVar(topScope(), start)
    while SC.sym == PROCEDURE:
        getSym()
        if SC.sym == IDENT: getSym()
        else: mark("procedure name expected")
        ident = SC.val; newObj(ident, Proc([])) #  entered without parameters
        sc = topScope()
        CG.procStart(); openScope() # new scope for parameters and body
        if SC.sym == LPAREN:
            getSym()
            if SC.sym in {VAR, IDENT}:
                if SC.sym == VAR: getSym(); typedIds(Ref) #, procParams)
                else: typedIds(Var) #, procParams)
                while SC.sym == SEMICOLON:
                    getSym()
                    if SC.sym == VAR: getSym(); typedIds(Ref) #, procParams)
                    else: typedIds(Var) #, procParams)
            else: mark("formal parameters expected")
            fp = topScope()
            sc[-1].par = fp[:] #  procedure parameters updated
            if SC.sym == RPAREN: getSym()
            else: mark(") expected")
        else: fp = []
        parsize = CG.genFormalParams(fp)
        if SC.sym == SEMICOLON: getSym()
        else: mark("; expected")
        localsize = declarations(CG.genLocalVars)
        CG.genProcEntry(ident, parsize, localsize)
        x = compoundStatement(); CG.genProcExit(x, parsize, localsize)
        closeScope() #  scope for parameters and body closed
        if SC.sym == SEMICOLON: getSym()
        else: mark("; expected")
    return varsize

def program():
    """
    Parses
        program = "program" ident ";" declarations compoundStatement.
    Generates code if no error is reported
    """
    newObj('boolean', Type(Bool)); Bool.size = 4
    newObj('integer', Type(Int)); Int.size = 4
    newObj('true', Const(Bool, 1))
    newObj('false', Const(Bool, 0))
    newObj('read', StdProc([Ref(Int)]))
    newObj('write', StdProc([Var(Int)]))
    newObj('writeln', StdProc([]))
    CG.progStart()
    if SC.sym == PROGRAM: getSym()
    else: mark("'program' expected")
    ident = SC.val
    if SC.sym == IDENT: getSym()
    else: mark('program name expected')
    if SC.sym == SEMICOLON: getSym()
    else: mark('; expected')
    declarations(CG.genGlobalVars); CG.progEntry(ident)
    x = compoundStatement()
    return CG.progExit(x)

def compileString(src, dstfn = None, target = 'mips'):
    """Compiles string src; if dstfn is provided, the code is written to that
    file, otherwise printed on the screen"""
    global CG
    #  used for init, genRec, genArray, progStart, genGlobalVars, \
    #  progEntry, progExit, procStart, genFormalParams, genActualPara, \
    #  genLocalVars, genProcEntry, genProcExit, genSelect, genIndex, \
    #  genVar, genConst, genUnaryOp, genBinaryOp, genRelation, genSeq, \
    #  genAssign, genCall, genRead, genWrite, genWriteln, genCond, \
    #  genIfThen, genThen, genIfElse, genTarget, genWhile
    if target == 'mips': import CGmips as CG
    elif target == 'ast': import CGast as CG
    else: print('unknown target'); return
    SC.init(src)
    ST.init()
    CG.init()
    p = program()
    if p != None and not SC.error:
        if dstfn == None: print(p)
        else:
            with open(dstfn, 'w') as f: f.write(p);

def compileFile(srcfn):
    if srcfn.endswith('.p'):
        with open(srcfn, 'r') as f: src = f.read()
        dstfn = srcfn[:-2] + '.s'
        compileString(src, dstfn)
    else: print("'.p' file extension expected")

# sampe usage:
# import os
# os.chdir('/Users/emil/Google Drive/CS 4TB4 - Winter 2016/Assignment 3')
# compileFile('disassembly.p')
# os.chdir('/Users/emil/Documents/Courses/CS 4TB3 Winter 16/Demos/errors')
# compileFile('matrix.p')
