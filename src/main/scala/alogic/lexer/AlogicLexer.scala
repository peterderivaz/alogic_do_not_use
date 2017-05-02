package alogic.lexer

import alogic.compiler.{Location, AlogicLexerError}

import scala.util.parsing.combinator.RegexParsers
import scala.collection._

// A separate Lexer is good because:
//   It makes compilation faster
//   It decomposes the problem into simpler subproblems
//   Makes it possible to skip comments
//   Deals with || and | ambiguity, i.e. variable ifthenelse becomes legal

// Can we handle preprocessor within this?
// We need to be able to define symbols, and lookup them up
// Only tend to use simple lookups, so may be able to keep a list of RHS tokens?
// May be able to treat defines within comments?
// However, may need to recursively invoke parser and then perhaps fold tokenlists?

// TODO
//   Support for #if #else
//   Fix stackoverflow in compiler:
//     http://stackoverflow.com/questions/34318705/stackoverflowerror-in-sbt-during-compile
//     set JAVA_OPTS=-Xmx1G -Xss4M
//   Support for command_writer: NUM_COUNT_BITS'b0

object AlogicLexer extends RegexParsers {
  override def skipWhitespace = true
  
  val defines = mutable.Map[String,List[AlogicToken]]()
  
  def apply(code: String): Either[AlogicLexerError, List[AlogicToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(AlogicLexerError(Location(next.pos.line, next.pos.column, next.pos.longString), msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[AlogicToken]] = {
    phrase(rep1(comment.* ~> tokenlist) <~ comment.*) ^^ (ts => ts.flatten)
  }
  
  def tokenlist: Parser[List[AlogicToken]] = {
	token ^^ (t=>List(t)) |
	identifier ^^ {
	case IDENTIFIER(str) if (defines contains str) => defines(str)
    case x => List(x) }	
  }
  
  // Maybe faster to grab a word, then perform lookups for the keywords!
  
  def token: Parser[AlogicToken] = positioned {
	(leftshiftequal | rightshiftequal | plusequals | notequal | minusequals | equalsequals | lessequal | greaterequal) |
	(colon | minuscolon | pluscolon | andequal | pipeequal | andand | andtok | star | inttok | uint | unarytilda | pipepipe) | 
	(pipe | leftshift | rightshift | lessthan | greaterthan |  ampersand | plusplus | minusminus | plus | minus | not) | 
	dot | comma | whiletok | dotok | fortok /* Don't take up namespace with these words - ready is useful elsewhere sync | ready | wire | accept | bubble */ | 
	(fsm | network | pipeline | typedef | semicolon | iftok | elsetok | dollar | break | goto | returntok | xorequals | xor) |
	(uint_type | int_type | bool | struct | leftcurly | rightcurly | equals | in | out | const | casetok | defaulttok ) | 
	(leftsquare | rightsquare | leftbracket | rightbracket | questionmark | truetok | falsetok | constant | verilog_stmt | verilog | void | fence | equals) | literal }

  def comment: Parser[Any] = regex("//.*".r) | 
          regex("/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/".r) | // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment
		  "#\\s*define\\s+".r ~> identifier ~ "[^\\n]*".r ^^ {
			case IDENTIFIER(id) ~ rhs => defines(id) = parse(tokens,rhs) match {
				case NoSuccess(msg, next) => List(BADDEFINE()) // TODO how to pass an error back from here?  Perhaps with ^? somehow?
				case Success(result,next) => result
			}
		  } withFailureMessage "Unknown symbol found" // Could do parsing later if want recursive defines?
 
  def uint_type: Parser[INTTYPE] = positioned {
    "u[0-9_]+\\b".r ^^ { str => 
		val content = str.substring(1,str.length)
		INTTYPE(false,content.toInt) 
	}
  }
  
  def int_type: Parser[INTTYPE] = positioned {
    "i[0-9_]+\\b".r ^^ { str => 
		val content = str.substring(1,str.length)
		INTTYPE(true,content.toInt) 
	}
  }
  
  def bool: Parser[INTTYPE] = positioned { 
	"bool\\b".r ^^ { _ => INTTYPE(false,1) }
  }
  
  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }
  
  def constant: Parser[CONSTANT] = positioned {
    "[0-9]*'[a-zA-Z0-9_]*".r ^^ { str => CONSTANT(str) } | // TODO make this more explicit
	"[0-9]+".r ^^ { str => CONSTANT(str) }
  }
  
  def verilog_stmt: Parser[VERILOGSTMT] = positioned {
	"void\\s+verilog\\s*".r ~! "\\(\\s*\\)\\s*".r~>verilog_body
  }
  
  def anycontent: Parser[VERILOGSTMT] = positioned {
	"[^\\{\\}]+".r ^^ (x=>VERILOGSTMT(x)) |
	verilog_body
  }
  
  def verilog_body: Parser[VERILOGSTMT] = positioned {
	"{" ~! rep(anycontent) ~! "}" ^^ {
	case a~b~c => VERILOGSTMT(a + b.mkString("") + c) }
  }
  
  // Add regex to prevent textual tokens matching a longer symbol
  // /b means a word boundary

  def colon         = positioned { ":"             ^^ (_ => COLON()) }
  def dollar        = positioned { "$"             ^^ (_ => DOLLAR()) }
  def andand        = positioned { "&&"            ^^ (_ => ANDAND()) }
  def plusplus      = positioned { "++"            ^^ (_ => PLUSPLUS()) }
  def andtok        = positioned { "&"             ^^ (_ => AND()) }
  def minuscolon    = positioned { "-:"            ^^ (_ => MINUSCOLON()) }
  def minusminus    = positioned { "--"            ^^ (_ => MINUSMINUS()) }
  def pluscolon     = positioned { "+:"            ^^ (_ => PLUSCOLON()) }
  def star          = positioned { "*"             ^^ (_ => STAR()) }
  def lessthan      = positioned { "<"             ^^ (_ => LESSTHAN()) }
  def greaterthan   = positioned { ">"             ^^ (_ => GREATERTHAN()) }
  def lessequal     = positioned { "<="            ^^ (_ => LESSEQUAL()) }
  def greaterequal  = positioned { ">="            ^^ (_ => GREATEREQUAL()) }
  def leftshift     = positioned { "<<"            ^^ (_ => LEFTSHIFT()) }
  def leftshiftequal= positioned { "<<="           ^^ (_ => LEFTSHIFTEQUAL()) }
  def rightshift    = positioned { ">>"            ^^ (_ => RIGHTSHIFT()) }
  def rightshiftequal = positioned { ">>="            ^^ (_ => RIGHTSHIFTEQUAL()) }
  def unarytilda    = positioned { "~"             ^^ (_ => UNARYTILDA()) }
  def pipepipe      = positioned { "||"            ^^ (_ => PIPEPIPE()) }
  def pipe          = positioned { "|"             ^^ (_ => PIPE()) }
  def ampersand     = positioned { "&"             ^^ (_ => AMPERSAND()) }
  def questionmark  = positioned { "?"             ^^ (_ => QUESTIONMARK()) }
  def plus          = positioned { "+"             ^^ (_ => PLUS()) }
  def plusequals    = positioned { "+="            ^^ (_ => PLUSEQUALS()) }
  def minusequals   = positioned { "-="            ^^ (_ => MINUSEQUALS()) }
  def equalsequals  = positioned { "=="            ^^ (_ => EQUALSEQUALS()) }
  def minus         = positioned { "-"             ^^ (_ => MINUS()) }
  def notequal      = positioned { "!="            ^^ (_ => NOTEQUAL()) }
  def andequal      = positioned { "&="            ^^ (_ => ANDEQUAL()) }
  def pipeequal     = positioned { "|="            ^^ (_ => PIPEEQUAL()) }
  def not           = positioned { "!"             ^^ (_ => NOT()) }
  def dot           = positioned { "."             ^^ (_ => DOT()) }
  def comma         = positioned { ","             ^^ (_ => COMMA()) }
  def fsm           = positioned { "fsm\\b".r           ^^ (_ => FSM()) }
  def network       = positioned { "network\\b".r       ^^ (_ => NETWORK()) }
  def pipeline      = positioned { "pipeline\\b".r      ^^ (_ => PIPELINE()) }
  def typedef       = positioned { "typedef\\b".r      ^^ (_ => TYPEDEF()) }
  def semicolon     = positioned { ";"             ^^ (_ => SEMICOLON()) }
  def equals        = positioned { "="             ^^ (_ => EQUALS()) }
  def struct        = positioned { "struct\\b".r        ^^ (_ => STRUCT()) }
  def leftcurly     = positioned { "{"             ^^ (_ => LEFTCURLY()) }
  def leftsquare    = positioned { "["             ^^ (_ => LEFTSQUARE()) }
  def leftbracket   = positioned { "("             ^^ (_ => LEFTBRACKET()) }
  def rightcurly    = positioned { "}"             ^^ (_ => RIGHTCURLY()) }
  def rightsquare   = positioned { "]"             ^^ (_ => RIGHTSQUARE()) }
  def rightbracket  = positioned { ")"             ^^ (_ => RIGHTBRACKET()) }
  def in            = positioned { "in\\b".r            ^^ (_ => IN()) }
  def out           = positioned { "out\\b".r           ^^ (_ => OUT()) }
  def xorequals     = positioned { "^="                 ^^ (_ => XOREQUALS()) }
  def xor           = positioned { "^"                  ^^ (_ => XOR()) }
  def const         = positioned { "const\\b".r         ^^ (_ => CONST()) }
  def fence         = positioned { "fence\\b".r         ^^ (_ => FENCE()) }
  def truetok       = positioned { "true\\b".r          ^^ (_ => TRUE()) }
  def falsetok      = positioned { "false\\b".r         ^^ (_ => FALSE()) }
  def void          = positioned { "void\\b".r          ^^ (_ => VOID()) }
  def uint          = positioned { "uint\\b".r          ^^ (_ => UINT()) }
  def inttok        = positioned { "int\\b".r           ^^ (_ => INT()) }
  def whiletok      = positioned { "while\\b".r         ^^ (_ => WHILE()) }
  def dotok         = positioned { "do\\b".r            ^^ (_ => DO()) }
  def fortok        = positioned { "for\\b".r           ^^ (_ => FOR()) }
  def iftok         = positioned { "if\\b".r            ^^ (_ => IF()) }
  def goto          = positioned { "goto\\b".r          ^^ (_ => GOTO()) }
  def elsetok       = positioned { "else\\b".r          ^^ (_ => ELSE()) }
  def break         = positioned { "break\\b".r         ^^ (_ => BREAK()) }
  def returntok     = positioned { "return\\b".r        ^^ (_ => RETURN()) }
  def casetok       = positioned { "case\\b".r          ^^ (_ => CASE()) }
  def defaulttok    = positioned { "default\\b".r       ^^ (_ => DEFAULT()) }
  def verilog       = positioned { "verilog\\b".r       ^^ (_ => VERILOG()) }

  def literal: Parser[LITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }
}
