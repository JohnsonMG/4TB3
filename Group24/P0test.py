from P0 import compileString
from ST import printSymTab

def testSyntaxCheck0():
    """produces error "] expected" """
    compileString("""
program p;
  var x: integer;
  var a: array [1..10] of integer;
  begin x := a[4 end
""")

def testSyntaxCheck1():
    """produces error "expression expected",
    "incompatible assignment" """
    compileString("""
program p;
  var x: integer;
  begin x := * end
""")

def testSyntaxCheck2():
    """produces error ") expected" """
    compileString("""
program p;
  var x: integer;
  begin x := (5 end
""")

def testSyntaxCheck3():
    """produces error "'begin' expected" """
    compileString("""
program p;
  var x: integer;
  x := 3
end
""")

def testSyntaxCheck4():
    """produces error "; expected" """
    compileString("""
program p;
  begin
    writeln
    writeln()
  end
""")

def testSyntaxCheck5():
    """produces error "'end' expected" """
    compileString("""
program p; begin writeln()
""")

def testSyntaxCheck6():
    """produces error "statement expected" """
    compileString("""
program p;
  begin var end
""")

def testSyntaxCheck7():
    """produces error ":= expected" """
    compileString("""
program p;
  var x: integer;
  begin x = 3 end
""")

def testSyntaxCheck8():
    """produces error ":= expected" """
    compileString("""
program p;
  var x: integer;
  begin x + 3 end
""")

def testSyntaxCheck9():
    """produces error "')' expected" """
    compileString("""
program p;
  begin write(3 end
""")

def testSyntaxCheck10():
    """produces error "'then' expected" """
    compileString("""
program p;
  begin
    if true write(5)
  end
""")

def testSyntaxCheck11():
    """produces error "'do' expected" """
    compileString("""
program p;
  begin
    while true writeln
  end
""")

def testSyntaxCheck12():
    """produces error "'[' expected", '.' expected,
    "expression expected", "']' expected", "type expected" """
    compileString("""
program p;
  type T = array 5 of integer;
  begin writeln end
""")

def testSyntaxCheck14():
    """produces error "'end' expected" """
    compileString("""
program p;
  type T = record x:integer
  begin writeln end
""")

def testSyntaxCheck15():
    """produces error "identifier expected" """
    compileString("""
program p;
  type T = record end;
  begin writeln end
""")

def testSyntaxCheck16():
    """produces error "':' expected" """
    compileString("""
program p;
  type T = record f end;
  begin writeln end
""")

def testSyntaxCheck17():
    """produces error "identifier expected" """
    compileString("""
program p;
  type T = record f, end;
  begin writeln end
""")

def testSyntaxCheck18():
    """produces error "'begin' or declaration expected" """
    compileString("""
program p;
  integer x;
  begin writeln end
""")

def testSyntaxCheck19():
    """produces error "= expected" """
    compileString("""
program p;
  const c: 5;
  begin writeln end
""")

def testSyntaxCheck20():
    """produces error "constant name expected", "'end' expected" """
    compileString("""
program p;
  const 5 = 7;
  begin writeln end
""")

def testSyntaxCheck21():
    """produces error "; expected" """
    compileString("""
program p;
  const c = 5
  begin writeln end
""")

def testSyntaxCheck22():
    """produces error "= expected", "type expected" """
    compileString("""
program p;
  type T: integer;
  begin writeln end
""")

def testSyntaxCheck23():
    """produces error "; expected" """
    compileString("""
program p;
  type T = integer
  begin writeln end
""")

def testSyntaxCheck24():
    """produces error "type name expected", "variable or procedure expected",
    "'end' expected" """
    compileString("""
program p;
  type 5 = integer;
  begin writeln end
""")

def testSyntaxCheck25():
    """produces error "; expected" """
    compileString("""
program p;
  var v: integer
  begin writeln end
""")

def testSyntaxCheck26():
    """produces error "procedure name expected" """
    compileString("""
program p;
  procedure;
    begin writeln end;
  begin writeln end
""")

def testSyntaxCheck27():
    """produces error "formal parameters expected" """
    compileString("""
program p;
  procedure q();
    begin writeln end;
  begin writeln end
""")

def testSyntaxCheck28():
    """produces error ") expected" """
    compileString("""
program p;
  procedure q(x: integer
    begin writeln end;
  begin writeln end
""")

def testSyntaxCheck29():
    """produces error "; expected" """
    compileString("""
program p;
  procedure q
    begin writeln end;
  begin writeln end
""")

def testSyntaxCheck30():
    """produces error "; expected" """
    compileString("""
program p;
  procedure q;
    begin writeln end
  begin writeln end
""")

def testSyntaxCheck31():
    """produces error "'program' expected'" """
    compileString("""
p; begin writeln end
""")

def testSyntaxCheck32():
    """produces error 'program name expected'"""
    compileString("""
program begin writeln end
""")

def testSyntaxCheck33():
    """produces error '; expected'"""
    compileString("""
program p begin writeln end
""")

def testSyntaxCheck34():
    """produces cascasde of errors """
    compileString("""
program p;
  type T = while;
  begin writeln end
""")

def testSymTab0():
    """produces "undefined identifier", "not a type",
    "incompatible assignment" """
    compileString("""
program p;
  var y: U;
  begin y := true
  end
""")

def testSymTab1():
    """produces "undefined identifier N",
    "bad upper bound" """
    compileString("""
program p;
  type T = array [7 .. N] of integer;
  var x: T;
  begin writeln()
  end
""")

def testSymTab2():
    """produces "multiple definition"
    "variable or procedure expected" """
    compileString("""
program p;
  const x = 9;
  var x : integer;
  begin x := 7
  end
""")

def testSymTab3():
    """produces "multiple definition" """
    compileString("""
program p;
  procedure q(var z: boolean);
    var z: integer
    begin z := true end;
  begin writeln()
  end
""")

def testSymTab4():
    compileString("""
program p;
  const N = 10;
  type T = array [1 .. N] of integer;
  var x: T;
  var y: boolean;
  var z: record f: integer; g: boolean end;
  procedure q(var z: boolean);
    begin z := false end;
  begin y := true
  end
""", "ST4.S")
    printSymTab()
"""produces
type boolean lev 0: <class 'ST.Bool'>
type integer lev 0: <class 'ST.Int'>
const true lev 0: <class 'ST.Bool'>=1
const false lev 0: <class 'ST.Bool'>=0
stdproc read lev 0 par ["ref  lev : <class 'ST.Int'>"]
stdproc write lev 0 par ["var  lev : <class 'ST.Int'>"]
stdproc writeln lev 0 par []
const N lev 0: <class 'ST.Int'>=10
type T lev 0: array lower 1 length 10 base <class 'ST.Int'>
var x lev 0: array lower 1 length 10 base <class 'ST.Int'>
var y lev 0: <class 'ST.Bool'>
var z lev 0: record ["var f lev 1: <class 'ST.Int'>", "var g lev 1: <class 'ST.Bool'>"]
proc q lev 0 par ["ref z lev 1: <class 'ST.Bool'>"]
"""

def testTypeCheck0():
    """produces error "not a field",
    "incompatible assignment" """
    compileString("""
program p;
  var v: record f: integer end;
  begin v.g := 4
  end
""")

def testTypeCheck1():
    """produces error "not a record",
    "variable or procedure expected" """
    compileString("""
program p;
  var v: integer;
  begin v.g := 4
  end
""")

def testTypeCheck2():
    """produces error "identifier expected" """
    compileString("""
program p;
  var v: record f: integer end;
  begin v.3 := 4
  end
""")

def testTypeCheck3():
    """produces error "index out of bounds" """
    compileString("""
program p;
  var x: array [5 .. 7] of integer;
  begin x[4] := 3
  end
""")
 
def testTypeCheck4():
    """produces "index out of bounds" """
    compileString("""
program p;
  var x: array [5 .. 7] of integer;
  begin x[8] := 3
  end
""")
 
def testTypeCheck5():
    """produces error "index not integer" """
    compileString("""
program p;
  var x: array [5 .. 7] of integer;
  begin x[x] := 3
  end
""")
 
def testTypeCheck6():
    """produces "not an array" """
    compileString("""
program p;
  var x: integer;
  begin x[9] := 3
  end
""")

def testTypeCheck7():
    """produces "expression expected", "illegal assignment" """
    compileString("""
program p;
  var x: integer;
  begin x := integer
  end
""")

def testTypeCheck8():
    """produces error "incompatible assignment" twice"""
    compileString("""
program p;
  var x: boolean;
  procedure q;
    var x: integer;
    begin x := true
    end;
  begin x := 3
  end
""")

def testTypeCheck9():
    """produces error "illegal parameter mode" """
    compileString("""
program p;
  procedure q(a: integer);
    begin a := 7
    end;
  begin q(true)
  end
""")
    
def testTypeCheck10():
    """produces error "illegal parameter mode" """
    compileString("""
program p;
  procedure q(var a: integer);
    begin a := 7
    end;
  begin q(5)
  end
""")

def testTypeCheck11():
    """produces error "extra parameter" """
    compileString("""
program p;
  var x: integer;
  procedure q;
    begin x := 7
    end;
  begin q(x)
  end
""")
    
def testTypeCheck12():
    """produces error "too few parameters" """
    compileString("""
program p;
  procedure q(a: integer);
    begin a := 7
    end;
  begin q()
  end
""")
    
def testTypeCheck13():
    """produces error "variable or procedure expected" """
    compileString("""
program p;
  const c = 7;
  begin c := 4
  end
""")

def testTypeCheck14():
    """produces error "boolean expected" """
    compileString("""
program p;
  begin
    while 5 do writeln
  end
""")

def testTypeCheck15():
    """produces error "boolean expected" """
    compileString("""
program p;
  begin
    if 5 then writeln
  end
""")

def testTypeCheck16():
    """produces error "not a type" """
    compileString("""
program p;
  const c = 3;
  type T = c;
  begin writeln end
""")

def testTypeCheck17():
    """produces error "bad lower bound" """
    compileString("""
program p;
  var v: array[-1 .. 5] of integer;
  begin writeln end
""")

def testTypeCheck18():
    """produces error "bad upper bound" """
    compileString("""
program p;
  var v: array[5 .. 3] of integer;
  begin writeln end
""")

def testTypeCheck19():
    """produces error "expression not constant" """
    compileString("""
program p;
  var v: integer;
  procedure q;
    const c = v;
    begin writeln end;
  begin writeln end
""")


def testCodeGenCheck0():
    """produces error 'value too large'"""
    compileString("""
program p;
  const c = 100000;
  var x: integer;
  begin x := c
  end
""")

def testCodeGenCheck1():
    """produces error 'no structured value parameters'"""
    compileString("""
program p;
  type a = array [1..10] of integer;
  procedure q(f: a);
    begin a := 4
    end
  begin a(5)
  end
""")

def testCodeGenCheck2():
    """produces error 'out of register'"""
    compileString("""
program p;
  var x: integer;
  begin
    x := 0*x + (1*x + (2*x + (3*x + (4*x + (5*x + (6*x + (7*x + (8*x))))))))
  end
""")

def testCodeGenCheck3():
    """produces error 'level!'"""
    compileString("""
program p;
  procedure q;
    var x: integer;
    procedure r;
      begin x := 5
      end;
    begin x := 3
    end;
  begin x := 7
  end
""")

def testCodeGenCheck4():
    """produces error 'unsupported parameter type'"""
    compileString("""
program p;
  var x: integer;
  procedure q(b: boolean);
    begin b := false
    end;
  begin q(x > 7)
  end
""")

def testCompiling0():
    """input & output"""
    compileString("""
program p;
  var x: integer;
  begin read(x);
    x := 3 * x;
    write(x);
    writeln();
    writeln();
    write(x * 5)
  end
""", 'T0.s')
    """ generates
	.data
x:	.space 4
	.text
	.globl main
	.ent main
main:	
	li $v0, 5
	syscall
	sw $v0, x
	addi $t0, $0, 3
	lw $t4, x
	mul $t0, $t0, $t4
	sw $t0, x
	lw $a0, x
	li $v0, 1
	syscall
	li $v0, 11
	li $a0, '\n'
	syscall
	li $v0, 11
	li $a0, '\n'
	syscall
	lw $t6, x
	mul $t6, $t6, 5
	add $a0, $t6, $0
	li $v0, 1
	syscall
	li $v0, 10
	syscall
	.end main
"""

def testCompiling1():
    """parameter passing"""
    compileString("""
program p;
  type T = array [1..10] of integer;
  var x: integer;
  var z: T;
  procedure q({-4($sp)}a: integer {4($fp)}; {-8($sp)}var b: integer {($fp)});
    var y: integer;{-12($fp)}
    begin y := a; write(y); writeln(); {writes 7}
      a := b; write(x); write(a); writeln(); {writes 5, 5}
      b := y; write(b); write(x); writeln(); {writes 7, 5}
      write(a); write(y); writeln(); {writes 5, 7}
      write(z[4]) {writes 0}
    end;
  procedure r(var c: T);
    begin c[x] := x; q(7, c[x]); write(x) {writes 5}
    end;
  begin x := 5; r(z)
  end
""", 'T1.s')

def testCompiling2():
    """arrays and records"""
    compileString("""
program p;
  type a = array [1 .. 7] of integer;
  type r = record f: integer; g: a; h: integer end;
  var v: a;
  var w: r;
  var x: integer;
  procedure q(var c: a; var d: r);
    var y: integer;
    begin y := 3;
      write(d.h); write(c[1]); write(d.g[y]); {writes 5, 3, 9}
      writeln(); c[7] := 7; write(c[y+4]); {writes 7}
      d.g[y*2] := 7; write(d.g[6]) {writes 7}
    end;
  begin x := 9;
    w.h := 12 - 7; write(w.h); {writes 5}
    v[1] := 3; write(v[x-8]); {writes 3}
    w.g[x div 3] := 9; write(w.g[3]); {writes 9}
    writeln(); q(v, w); writeln();
    write(v[7]); write(w.g[6]) {writes 7, 7}
  end
""", 'T2.s')

def testCompiling3():
    """booleans and conditions"""
    compileString("""
program p;
  const five = 5;
  const seven = 7;
  const always = true;
  const never = false;
  var x, y, z: integer;
  var b, t, f: boolean;
  begin x := seven; y := 9; z := 11; t := true; f := false;
    if true then write(7) else write(9);    {writes 7}
    if false then write(7) else write(9);   {writes 9}
    if t then write(7) else write(9);       {writes 7}
    if f then write(7) else write(9);       {writes 9}
    if not t then write(7) else write(9);   {writes 9}
    if not f then write(7) else write(9);   {writes 7}
    if t or t then write(7) else write(9);  {writes 7}
    if t or f then write(7) else write(9);  {writes 7}
    if f or t then write(7) else write(9);  {writes 7}
    if f or f then write(7) else write(9);  {writes 9}
    if t and t then write(7) else write(9); {writes 7}
    if t and f then write(7) else write(9); {writes 9}
    if f and t then write(7) else write(9); {writes 9}
    if f and f then write(7) else write(9); {writes 9}
    writeln();
    b := true;
    if b then write(3) else write(5); {writes 3}
    b := false;
    if b then write(3) else write(5); {writes 5}
    b := x < y;
    if b then write(x) else write(y); {writes 7}
    b := (x > y) or t;
    if b then write(3) else write(5); {writes 3}
    b := (x > y) or f;
    if b then write(3) else write(5); {writes 5}
    b := (x = y) or (x > y);
    if b then write(3) else write(5); {writes 5}
    b := (x = y) or (x < y);
    if b then write(3) else write(5); {writes 3}
    b := f and (x >= y);
    if b then write(3) else write(5); {writes 5}
    writeln();
    while y > 3 do                    {writes 9, 8, 7, 6, 5, 4}
      begin write(y); y := y - 1 end;
    write(y); writeln();              {writes 3}
    if not(x < y) and t then          {writes 7}
      write(x)
  end
""", 'T3.s')

def testCompiling4():
    """constant folding; local & global variables'"""
    compileString("""
program p;
  const seven = (9 mod 3 + 5 * 3) div 2;
  type int = integer;
  var x, y: integer;
  procedure q;
    const sotrue = true and true;
    const sofalse = false and true;
    const alsotrue = false or true;
    const alsofalse = false or false;
    var x: int;
    begin x := 3;
      if sotrue then y := x else y := seven;
      write(y); {writes 3}
      if sofalse then y := x else y := seven;
      write(y); {writes 7}
      if alsotrue then y := x else y := seven;
      write(y); {writes 3}
      if alsofalse then y := x else y := seven;
      write(y); {writes 7}
      if not(true or false) then write(5) else write(9)
    end;
  begin x := 7; q(); write(x) {writes 7}
  end
""", 'T4.s')

def testCompiling5():
    """example with procedures"""
    compileString("""
program p;
  var g: integer;          {global variable}
  procedure q(v: integer); {value parameter}
    var l: integer;        {local variable}
    begin
      l := 9;
      if l > v then
         write(l)
      else
         write(g)
    end;
  begin
    g := 5;
    q(7)
  end
""", 'T5.s')
""" generates:
	.data
g_:	.space 4
	.text
	.globl q
	.ent q
q:	                   # procedure q
	sw $fp, -8($sp)    # M[$sp - 8] := $fp
	sw $ra, -12($sp)   # M[$sp - 12] := $ra
	sub $fp, $sp, 4    # $fp := $sp - 4
	sub $sp, $fp, 12   # $sp := $fp - 12
	                   # adr(v) = M[$fp]
	                   # adr(l) = M[$fp - 12]
	addi $t0, $0, 9    # $t0 := 9
	sw $t0, -12($fp)   # l := $t0
	lw $t5, -12($fp)   # $t5 := l
	lw $t1, 0($fp)     # $t1 := v
	ble $t5, $t1, C0   # if $t5 <= $t1 then pc := adr(C0)
C1:	
	lw $a0, -12($fp)   # $a0 := l
	li $v0, 1          # write($a0)
	syscall
	b I0               # goto I0
C0:	
	lw $a0, g_         # $a0 := g
	li $v0, 1          # write($a0)
	syscall
I0:	
	add $sp, $fp, 4    # $sp := $fp + 4
	lw $ra, -8($fp)    # $ra := M[$fp - 8]
	lw $fp, -4($fp)    # $fp := M[$fp - 4]
	jr $ra             # $pc := $ra
	.text
	.globl main
	.ent main
main:	
	addi $t6, $0, 5    # $t6 := 5
	sw $t6, g_         # g := $t6
	addi $t4, $0, 7    # $t4 := 7
	sw $t4, -4($sp)    # M[$sp - 4] := $t4
	jal q              # $ra := $pc + 4; $pc := adr(q)
	li $v0, 10
	syscall	.end main
"""

def testCompiling6():
    """illustrating lack of 'optimization'"""
    compileString("""
program p;
  var x: integer;
  begin x := 5;
    x := x + 0;
    x := 0 + x;
    x := x * 1;
    x := 1 * x;
    x := x + 3;
    x := 3 + x
  end
""", 'T6.s')
""" generates
	.data
x_:	.space 4
	.text
	.globl main
	.ent main
main:	
	addi $t4, $0, 5
	sw $t4, x_
	lw $t6, x_
	add $t6, $t6, 0
	sw $t6, x_
	lw $t7, x_
	add $t8, $0, $t7
	sw $t8, x_
	lw $t3, x_
	mul $t3, $t3, 1
	sw $t3, x_
	addi $t2, $0, 1
	lw $t1, x_
	mul $t2, $t2, $t1
	sw $t2, x_
	lw $t5, x_
	add $t5, $t5, 3
	sw $t5, x_
	addi $t0, $0, 3
	lw $t4, x_
	add $t0, $t0, $t4
	sw $t0, x_
	li $v0, 10
	syscall
	.end main
"""

def demoAST0():
    print('Abstract Syntax Tree:')
    compileString("""
program p;
  var x: integer;
  var y: array [1..10] of integer;
  begin
    read(x);
    if x > 0 then
      while y[x] < 7 do
        x := x + 1
    else write(x);
    writeln
  end
""", target='ast')
    print('Symbol Table:')
    printSymTab()

def demoRecords0():
    compileString("""
program p;
  var a: integer;
  var b: integer;
  var x: record f, g: integer end;
  begin
    a := 7;
    b := 9;
    x.g := 3;
    x.f := 5
  end
""", target='ast')

def demoArrays0():
    compileString("""
program p;
  var i: integer;
  var x: array [1..10] of integer;
  begin
    x[5] := 3;
    x[i] := 5;
    x[i + 7] := i + 9
  end
""")

def demoDeclarations0():
    compileString("""
program p;
  type T = array [3..9] of integer;
  type U = record f: boolean; g: T end;
  var x: U;
  var y: integer;
  begin
    y := y + 3;
    x.f := true;
    x.g[y] := 5;
    x.g[y+5] := 5
  end
""")
"""generates:
	.data
y_:	.space 4          # size(y) = size(integer) = 4
x_:	.space 32         # size(T) = (9-3+1)*size(integer) = 7*4=28
                          # size(U) = size(integer)+size(T) = 32
                          # size(x) = size(U) = 32
                          # offset(f) = 0
                          # offset(g) = size(f) = 4
	.text
	.globl main
	.ent main
main:	
	lw $t3, y_        # $t3 := y
	add $t3, $t3, 3   # $t3 := $t3 + 3
	sw $t3, y_        # y := $t3
	addi $t1, $0, 1   # $t1 := 1
	sw $t1, x_+0      # [x+offset(f)* =] x.f := $t1
	lw $t0, y_        # $t0 := y
	sub $t0, $t0, 3   # $t0 := $t0 - 3
	mul $t0, $t0, 4   # $t0 := $t0 * size(integer)
	addi $t8, $0, 5   # $t8 := 5
	sw $t8, x_+4($t0) # [(x+offset(g)+adr(g[y]))* =] x.g[5] := t8
	li $v0, 10
	syscall
	.end main
"""

def demoDeclarations1():
    # The commented out variant has an OOB read...
    '''compileString("""
program p;
  type R = record f, g: boolean end;
  type S = array [1..11] of R;
  type T = array [3..9] of S;
  var x: T; 
  var y: integer;
  begin
    x[y][5].g := false;
    x[y][y + 1].f := true
  end
""")'''
    compileString("""
program p;
  type R = record f, g: boolean end;
  type S = array [1..11] of R;
  type T = array [3..9] of S;
  var x: T; 
  var y: integer;
  begin
    y:= y + 6;
    x[y][5].g := true; {So we can see the output in mem}
    x[y][y + 1].f := true
  end
""")

def demoAssignment0():
    compileString("""
program p;
  var x, y, z: integer;
  begin
    z := 3;
    z := x + y * 7;
    z := 0
  end
""")

def demoRelations0():
    compileString("""
program p;
  var x, y: integer;
  begin
    if x > y then x := 0
  end
""")

def testSIMD1():
    compileString("""
program p;
  type S = array [0..11] of integer;
  var x: S;
  var z: S;
  begin
    x := x + 1;
    z := z + 3;
    z[0] := 9;
    x := z + x;
    write(x[0]);
    writeln();
    write(x[5]);
    writeln();
    write(x[11]);
    writeln()
  end
""", "out.S")

def testSIMD2():
    compileString("""
program p;
  type S = array [0..11] of integer;
  var y: integer;
  var x: S;
  var z: S;
  begin
    y := 7;
    x := y * z * y
  end
""", "out.S")

def testSIMD3():
    compileString("""
program p;
  type S = array [0..11] of integer;
  var y: integer;
  var x: S;
  var z: S;
  begin
    x := 7 * z * 7
  end
""", "out.S")

def testSIMD4():
    compileString("""
program p;
  type S = array [0..14] of integer;
  var y: integer;
  var x: S;
  var z: S;
  begin
    y := 5;
    z[0] := 1;
    z[14] := 1;
    x := (z+3) * (-z + (7 * (-(1 + z))));
    x := -x;
    x := x mod 6;
    x := x + y;
    write(x[0]); writeln(); {prints 0}
    write(x[1]); writeln(); {prints 3}
    write(x[11]); writeln(); {prints 3}
    write(x[12]); writeln(); {prints 3}
    write(x[13]); writeln(); {prints 3}
    write(x[14]); writeln() {prints 0}
    {x := x * z;}
  end
""", "out.S")

def testSIMD5():
    compileString("""
program p;
  type S = array [0..14] of boolean;
  var y: integer;
  var x: S;
  var z: S;
  begin
    x := not x;
    {x := x or z;}
    x := x and x
    {x := x * z;}
    {write(x[0]); writeln();} {prints 0}
    {write(x[1]); writeln()} {prints 3}
  end
""", "out.S")


def testAll():
  testSymTab4()  # Pass
  testCompiling0()  # Pass
  testCompiling1()
  testCompiling2()  # Pass
  testCompiling3()  # Pass
  testCompiling4()  # Pass
  testCompiling5()  # Pass
  testCompiling6()  # Pass
  demoArrays0()  # Pass
  demoDeclarations0()  # Pass
  demoDeclarations1()  # Pass
  demoAssignment0()  # Pass
  demoRelations0()  # Pass