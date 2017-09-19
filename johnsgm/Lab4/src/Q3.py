# -*- coding: utf-8 -*-
"""
Evaluator for arithmetic expressions
Emil Sekerinski, McMaster University, Winter 16/17

Attribute rules are followed for the translations; the value
of a sub-expression, a synthesized attribute, is passed as a
result of the parsing procedures. In case of a parsing error,
a value must still be returned.
"""

# Scanner
# symbol ::= {blank} ("+" | "-" | "*" | "/" | "(" | ")" | integer)
# integer ::= digit {digit}
# letter ::= "a" | ... | "z"
# digit ::= "0" | ... | "9"
# blank ::= " " | "\t" | "\r" | "\n"

import math

EOF = 0; Plus = 1; Minus = 2; Times = 3; Div = 4; Lparen = 5
Rparen = 6; Round = 7; Int = 8; Decimal = 9; Other = 10

def getChar():
    """read next character of src in ch; at end ch is set to chr(0)"""
    global pos, ch
    if pos < len(src): ch, pos = src[pos], pos+1
    else: ch = chr(0)

def error(msg):
    print(); print('error:', msg); print(src); print((pos-1)*' '+'^')
    
def getSym():
    """read next symbol in sym; at end sym is set to EOF; if sym
    is Int, val is an integer; if sym is Ident, val is a string"""
    global sym, val
    while ch in ' \t\r\n': getChar()
    if ch == chr(0): sym = EOF
    elif ch == '+': sym = Plus; getChar()
    elif ch == '-': sym = Minus; getChar()
    elif ch == '*': sym = Times; getChar()
    elif ch == '/': sym = Div; getChar()
    elif ch == '(': sym = Lparen; getChar()
    elif ch == ')': sym = Rparen; getChar()
    elif ch == '#': sym = Round; getChar()
    elif '0' <= ch <= '9':
        sym, val = Int, int(ch); getChar()
        while '0' <= ch <= '9':
            val = 10*val+int(ch); getChar()
    elif ch == '.': sym = Decimal; getChar()
    else:
        error('unexpected character')
        sym = Other

# Parser
# expression ::= ["+" | '-'] term {("+" | "-") term)}
# term ::= factor {("*" | "/") factor}
# factor ::= integer | identifier | "(" expression ")"

def expression():
    """
    expression(v0) ::=
        ("+" term(v1) | "-" term(v1) <<v1 = -v1>>| term(v1))
        {"+" term(v2) <<v1 = v1+v2>> | "-" term <<v1 = v1-v2>>}
        <<v0 = v1>>

    expression(v0) ::=
        ("+" term(v1) <<if type(v1) != int or type(v1) != decimal then error>>
        |"−" term(v1) <<if type(v1) = int or type(v1) = decimal then v1 := − v1 else error>>
        |"#" term(v1) <<if type(v1) = decimal then v1 := round (v1) else error>>
        |term(v1))
        {"+" term(v2) <<if type(v1) = int = type(v2) or type(v1) = decimal = type(v2) then v1 := v1 + v2 else error>>
        |"−" term(v2) <<if type(v1) = int = type(v2) or type(v1) = decimal = type(v2) then v1 := v1 − v2 else error>>
        }
        <<v0 := v1>>
    """

    if sym == Plus: 
        getSym()
        v1 = term()
        if (type(v1) != int) and (type(v1) != float): error('incorrect type for addition')
    elif sym == Minus: 
        getSym()
        v1 = term()
        if (type(v1) == int) or (type(v1) == float): v1 = -v1
        else: error("incorrect type for '-' operator")
    elif sym == Round:
        getSym()
        v1 = term() 
        if type(v1) == float: v1 = round(v1)
        else: error('incorrect type for round operator')
    else: v1 = term()
    while sym in {Plus, Minus}:
        if sym == Plus: 
            getSym()
            v2 = term()
            if (type(v1) == int == type(v2)) or (type(v1) == float == type(v2)): v1 = v1 + v2
            else: error('operands of different type'); v1 = None
        else:
            getSym()
            v2 = term()
            if (type(v1) == int == type(v2)) or (type(v1) == float == type(v2)): v1 = v1 - v2
            else: error('operands of different type'); v1 = None
    return v1

def term():
    """
    term(v0) ::=
        factor(v1) {"*" factor(v2) <<v1 = v1*v2>> | "/" factor(v2) <<v1 = v1/v2>>}
        <<v0 = v1>>

    term(v0) ::=
        factor(v1)
        {"∗" factor(v2) <<if type(v1) = int = type(v2) or type(v1) = decimal = type(v2) then v1 := v1 ∗ v2 else error>>
        |"/" factor(v2) <<if type(v1) = int = type(v2) then v1 :=v1//v2
        elif type(v1) = decimal = type(v2) then v1 := v1 / v2
        else error>>}
        <<v0 := v1>>
    """

    v1 = factor()
    while sym in {Times, Div}:
        if sym == Times: 
            getSym()
            v2 = term()
            if (type(v1) == int == type(v2)) or (type(v1) == float == type(v2)): v1 = v1*v2
            else: error('operands of different type'); v1 = None
        else: 
            getSym()
            v2 = term()
            if type(v1) == int == type(v2): v1 = v1 // v2
            elif type(v1) == float == type(v2): v1 = v1 / v2
            else: error('operands of different type'); v1 = None
    return v1

def factor():
    """
    factor(v0) ::=
        integer(val) <<v0 = val>> | "(" expression(v1) <<v0 = v1>> ")"

    factor(v0) ::=
        integer(val) <<v0 := val>>
        | decimal(val) <<v0 := val>>
        | "(" expression(v1) ")" <<v0 := v1>>
    """
    if sym == Int:
        v0 = val
        getSym()
        if sym == Decimal:
            getSym()
            if sym == Int:
                v0 = float(v0) 
                if val > 0: v0 = v0 + float(val)/(10**(int(math.log10(val))+1))
                getSym()
            else: return error('unexpected symbol after decimal')

    elif sym == Lparen:
        getSym(); v0 = expression()
        if sym == Rparen: getSym()
        else: error(') missing')
    else: error('unexpected symbol'); v0 = 0
    return v0

def evaluate(s):
    global src, pos;
    src, pos = s, 0; getChar(); getSym();
    print(expression())

evaluate('1.0+2.0 *  3.0')
evaluate('1.0*2.0+3.0')
evaluate('(1.0 + 2.0) * (3.0 / 4.0)')
evaluate('-1.0-2.0')
evaluate('-(2.0)')
evaluate('-(#2.0)+1')
evaluate('(#8.0)/2+1')
evaluate('8.0/2.0+1.2')
evaluate('(#8.0)+ (#2.0)+(#1.2)')
evaluate('1.0+2 *  3.0')#error
evaluate('1.0*2.0+3')#error
evaluate('(1.0 + 2) * (3.0 / 4.0)')#error
evaluate('-1.0-2')#error
evaluate('-(#2.0)+1.0')#error