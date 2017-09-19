from P0 import compileString
from ST import printSymTab

def testEnum0():
    """produces error "identifier expected", "undefined identifier a",
    "incompatible assignment" """
    compileString("""
program p;
  type T = ();
  var x: T;
  begin
    x := a
  end
""")

def testEnum1():
    """produces error "identifier expected" """
    compileString("""
program p;
  type T = (a,);
  var x: T;
  begin
    x := a
  end
""")

def testEnum2():
    """produces error "multiple definition", "incompatible assignment" """
    compileString("""
program p;
  const c = 10;
  type T = (a, b, c);
  var x: T;
  begin
    x := a;
    x := b;
    x := c
  end
""")

def testEnum3():
    """produces error "multiple definition" """
    compileString("""
program p;
  type T = (a, b, b);
  var x: T;
  begin
    x := a;
    x := b
  end
""")

def testEnum4():
    """produces error "multiple definition" """
    compileString("""
program p;
  type T = (a, b, c);
  type U = (c, d);
  var x: T;
  begin
    x := a;
    x := b
  end
""")

def testEnum5():
    """produces error "incompatible assignment", "statement expected" """
    compileString("""
program p;
  type T = (a, b);
  var x: T;
  var u: (c, d);
  begin
    x := c;
  end
""")

def testEnum6():
    """produces error "incompatible assignment" """
    compileString("""
program p;
  var x: (a, b);
  procedure q;
    var y: (a, b);
    begin
      x := a
    end;
  begin
    x := a
  end
""")

def testEnum6():
    """produces error "incompatible assignment" """
    compileString("""
program p;
  var x: (a, b);
  procedure q;
    var y: (a, b);
    begin
      x := a
    end;
  begin
    x := a
  end
""")

def testEnum8():
    compileString("""
program p;
  type T = (a, b, c);
  var x, y: T;
  var u: (d, e, f);
  var v: boolean;
  begin
    x := y; {copied like any other basic type}
    u := d; {d is 0}
    u := e; {e is 1}
    u := f; {f is 2}
    v := e > f {constant evaluation}
  end
""")
""" generates
	.data
v_:	.space 4
u_:	.space 4
y_:	.space 4
x_:	.space 4
	.text
	.globl main
	.ent main
main:	
	lw $t8, y_
	sw $t8, x_
	sw $0, u_
	addi $t4, $0, 1
	sw $t4, u_
	addi $t1, $0, 2
	sw $t1, u_
	addi $t5, $0, 1
	addi $t2, $0, 2
	ble $t5, $t2, C0
C1:	
	addi $t7, $0, 1
	b A0
C0:	
	addi $t7, $0, 0
A0:	
	sw $t7, v_
	li $v0, 10
	syscall
	.end main
"""
testEnum8()