package alogic.lexer

import scala.util.parsing.input.Positional

// Lexer tokens are in all capitals

sealed trait AlogicToken extends Positional

case class IDENTIFIER(str: String) extends AlogicToken
case class CONSTANT(str: String) extends AlogicToken
case class LITERAL(str: String) extends AlogicToken  // String enclosed in " "
case class INTTYPE(signed: Boolean, size: Int) extends AlogicToken
case class DOLLAR() extends AlogicToken
case class UINT() extends AlogicToken
case class STAR() extends AlogicToken
case class INT() extends AlogicToken
case class LEFTCURLY() extends AlogicToken
case class LEFTSQUARE() extends AlogicToken
case class LEFTBRACKET() extends AlogicToken
case class RIGHTCURLY() extends AlogicToken
case class RIGHTSQUARE() extends AlogicToken
case class RIGHTBRACKET() extends AlogicToken
case class COMMA() extends AlogicToken
case class COLON() extends AlogicToken
case class MINUSCOLON() extends AlogicToken
case class PLUSCOLON() extends AlogicToken
case class FENCE() extends AlogicToken
case class FSM() extends AlogicToken
case class NETWORK() extends AlogicToken
case class TYPEDEF() extends AlogicToken
case class PIPELINE() extends AlogicToken
case class SEMICOLON() extends AlogicToken
case class EQUALS() extends AlogicToken
case class STRUCT() extends AlogicToken
case class IN() extends AlogicToken
case class OUT() extends AlogicToken
case class CONST() extends AlogicToken
case class VERILOG() extends AlogicToken
case class DOT() extends AlogicToken
case class PLUS() extends AlogicToken
case class MINUS() extends AlogicToken
case class NOT() extends AlogicToken
case class QUESTIONMARK() extends AlogicToken
case class TRUE() extends AlogicToken
case class FALSE() extends AlogicToken
case class VOID() extends AlogicToken
case class AMPERSAND() extends AlogicToken
case class UNARYTILDA() extends AlogicToken
case class RIGHTSHIFT() extends AlogicToken
case class RIGHTSHIFTEQUAL() extends AlogicToken
case class LEFTSHIFT() extends AlogicToken
case class LEFTSHIFTEQUAL() extends AlogicToken
case class PIPE() extends AlogicToken
case class PIPEPIPE() extends AlogicToken
case class LESSTHAN() extends AlogicToken
case class LESSEQUAL() extends AlogicToken
case class GREATERTHAN() extends AlogicToken
case class GREATEREQUAL() extends AlogicToken
case class ANDEQUAL() extends AlogicToken
case class PIPEEQUAL() extends AlogicToken
case class NOTEQUAL() extends AlogicToken
case class WHILE() extends AlogicToken
case class FOR() extends AlogicToken
case class DO() extends AlogicToken
case class AND() extends AlogicToken
case class IF() extends AlogicToken
case class GOTO() extends AlogicToken
case class ANDAND() extends AlogicToken
case class ELSE() extends AlogicToken
case class PLUSEQUALS() extends AlogicToken
case class MINUSEQUALS() extends AlogicToken
case class EQUALSEQUALS() extends AlogicToken
case class BREAK() extends AlogicToken
case class RETURN() extends AlogicToken
case class CASE() extends AlogicToken
case class DEFAULT() extends AlogicToken
case class PLUSPLUS() extends AlogicToken
case class MINUSMINUS() extends AlogicToken
case class VERILOGSTMT(str: String) extends AlogicToken
case class BADDEFINE() extends AlogicToken  // THis token is emitted for the RHS of a unparseable define
