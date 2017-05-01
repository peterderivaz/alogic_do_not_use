package alogic.parser

import alogic.compiler.{Location, AlogicParserError}
import alogic.lexer._

import scala.util.parsing.combinator._
import scala.util.parsing.input._

import scala.collection._

// This defines parser functions for alogic

/* TODO

Figure out how to handle int and uint datatypes
	Perhaps need our types to contain an optional expression?
	Perhaps combine with original types?
	How to keep the position - may happen anyway?

Add file input

Consider header files somehow

Add file output

Figure out how to handle verilog functions - perhaps a special regexp?

Should statements and expressions be based on different data types?  (May make recursing a bit less generic)
	Feels like making control statements and sequential into different types helps
	Don't do it as part of backtracking as this obscures errors, but return different types after checking subtypes.
	
Understand when better to have new base types, or new case classes?  Or fields that we can switch on?
	Perhaps lots of base classes good, avoid any switch statements.
	Only use common class when forced (because identifier can return either type!)
	(This should catch most problems with the compiler)
	
Add hierarchical variable definitions?  Could be done during the parsing?

Add #if

Add #define replacements

Seems to struggle with parsing some parts?  constrained_intra and trans_info_t?
Seems due to const: Need to add \b for word boundary!

Now have problems with uses of state, ready...  These seem fairly reasonable.
Perhaps with ready we should treat it as an identifier? (Feels a bit odd)
Or match the sync ready string as a single unit!
Perhaps still an issue if want an out sync ready!

*/
object AlogicParser extends Parsers {
	override type Elem = AlogicToken
	
	val typedefs = mutable.Map[String,AlogicType]()
	
	class AlogicTokenReader(tokens: Seq[AlogicToken]) extends Reader[AlogicToken] {
		override def first: AlogicToken = tokens.head
		override def atEnd: Boolean = tokens.isEmpty
		override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)  // TODO understand this line
		override def rest: Reader[AlogicToken] = new AlogicTokenReader(tokens.tail)
	}

	def apply(tokens: Seq[AlogicToken]): Either[AlogicParserError, AlogicAST] = {
		val reader = new AlogicTokenReader(tokens)
		start(reader) match {
		  case NoSuccess(msg, next) => Left(AlogicParserError(Location(next.pos.line, next.pos.column, next.pos.longString), msg))
		  case Success(result, next) => Right(result)
		}
	}

	def start : Parser[Program] = positioned {
		phrase(rep(typedef)~>rep(entity)) ^^ ( cmds => Program(cmds) )
	}
	
	def entity : Parser[Task] = positioned {
		rep(typedef) ~> task
	}
	
	// Useful to keep the IDENTIFIER token as this contains an error location
	// typedefs are handled during the initial parsing
	def typedef : Parser[Positional] = positioned {
		(TYPEDEF() ~> known_type ~! identifier ^? ( 
		  { case t ~ n if (!(typedefs contains n.str)) => typedefs(n.str) = t; t } , 
		  { case t ~ n => s"Redefinition of ${n.str}"}
		) ) <~ SEMICOLON() // Move semicolon to end so error appears in appropriate location
	}
	
	def tasktype : Parser[TaskType] = positioned (
		FSM() ^^ (_=>Fsm()) |
		PIPELINE() ^^ (_=>Pipeline()) |
		VERILOG() ^^ (_=>Verilog()) |
		(NETWORK() withFailureMessage "'fsm' or 'pipeline' or 'network' expected") ^^ (_=>Network())
	)
	
	/* Parse these in an inefficient way to allow the same words to appear as identifiers elsewhere */
	def sync_type : Parser[SyncType] = positioned (
		ident_sync~ident_sync~ident_sync ^? ( {
			case IdentSync()~IdentReady()~IdentBubble() => SyncReadyBubble()
			case IdentWire()~IdentSync()~IdentAccept() => WireSyncAccept()
		} , {_ => s"Unknown sync specification"} ) |
		ident_sync~ident_sync ^? ( {
			case IdentSync()~IdentReady() => SyncReady()
			case IdentWire()~IdentSync() => WireSync()
			case IdentSync()~IdentAccept() => SyncAccept()
		} , {_ => s"Unknown sync specification"} ) |
		ident_sync ^? ( {
			case IdentSync() => Sync()
			case IdentWire() => Wire()
		} , {_ => s"Unknown sync specification"} )
	)
	
	def initializer : Parser[AlogicAST] = positioned {
		EQUALS() ~> expr
	}
	
	def declaration : Parser[VarDeclaration] = positioned (
		           known_type~!primary_expr~!initializer.? <~ SEMICOLON() ^^ 
		{case      typ       ~id          ~init          => VarDeclaration(typ,id,init)}
	)
	
	def task_declaration : Parser[Declaration] = positioned (
		OUT() ~> sync_type.? ~ known_type ~ id <~ SEMICOLON() ^^ 
		{case    syn      ~ typ        ~ id => OutDeclaration(syn,typ,id)} |
		IN()  ~> sync_type.? ~ known_type ~ id <~ SEMICOLON() ^^ 
		{case    syn      ~ typ        ~ id => InDeclaration(syn,typ,id)} |
		CONST()~>known_type~primary_expr~initializer.? <~ SEMICOLON() ^^ 
		{case    typ       ~id          ~init            => ConstDeclaration(typ,id,init)} |
		VERILOG()~>known_type~primary_expr<~SEMICOLON() ^^ 
		{case      typ       ~id                         => VerilogDeclaration(typ,id)} |
		declaration
	)
	
	def task : Parser[Task] = positioned (
		      tasktype ~! identifier ~ LEFTCURLY() ~! task_declaration.* ~ task_content.* ~! RIGHTCURLY() ^^ 
		{case typ      ~  nam        ~ _           ~  decls              ~ content        ~ _             => Task(typ,nam,decls,content)}
	)
	
	def known_type : Parser[AlogicType] = positioned (
		inttype ^^ (x => Integer(x)) |
		identifier ^? (
			{ case n if (typedefs contains n.str) => typedefs(n.str)
              case n if (n.str=="state") => State() },
			{ case n => s"Unknown type ${n.str}" }
		) |
		STRUCT() ~> LEFTCURLY() ~> rep(field) <~ RIGHTCURLY() ^^ (Struct(_)) |
		INT() ~> LEFTBRACKET() ~> expr <~ RIGHTBRACKET() ^^ (x=>IntegerExpr(true,x)) |
		UINT() ~> LEFTBRACKET() ~> expr <~ RIGHTBRACKET() ^^ (x=>IntegerExpr(false,x))
		// TODO int and uint
	)
	
	def task_content : Parser[TaskContent] = positioned (
		VOID()~>identifier~LEFTBRACKET()~RIGHTBRACKET()~statement ^^ 
		{case   name      ~ _           ~ _            ~ s => Function(name,s)} |
		VOID()~>FENCE()~LEFTBRACKET()~RIGHTBRACKET()~statement ^^ 
		{case   _      ~ _           ~ _            ~ s => FenceFunction(s)} |
		verilogstmt
	)
	
	// We use a simple expr definition to make the LL parser efficient
	// Note that this does not extract the correct parse tree based on precedence, but this shouldn't matter as we reconstitute the expression at the end!
	def expr : Parser[AlogicAST] = positioned (
		binary_expr ~ opt( QUESTIONMARK()~expr~COLON()~expr ) ^^
		{ case b    ~ None => b
		  case b    ~ Some( _            ~left~ _     ~right) => TernaryOp(b,left,right)
		} 
	)
	
	def binary_op : Parser[AlogicToken] = positioned (
		PLUS() | MINUS() | AMPERSAND() | PIPE() | PIPEPIPE() | LESSTHAN() | GREATERTHAN() | STAR() | RIGHTSHIFT() | LEFTSHIFT() |
		ANDAND() | AND() | EQUALSEQUALS() | LESSEQUAL() | GREATEREQUAL() | NOTEQUAL() // TODO
	)
	
	def binary_expr : Parser[AlogicAST] = positioned (
		unary_expr~opt(binary_op~expr) ^^ 
		{ case e  ~None                => e
		  case e  ~Some(op      ~f   ) => BinaryOp(e,op,f)
		}
	)
	
	def unary_op : Parser[AlogicToken] = positioned (
		NOT() | UNARYTILDA() | PIPE() | MINUS() | AND() | PIPE()
	)
	
	def unary_expr : Parser[AlogicAST] = positioned (
		unary_op~primary_expr ^^
		{case op~e => UnaryOp(op,e)} |
		primary_expr	
	)
	
	def primary_expr : Parser[AlogicAST] = positioned (
		(secondary_expr~LEFTSQUARE()~expr~RIGHTSQUARE() ^^ 
		{case p     ~ _          ~ind ~ _            => ArrayLookup(p,ind)} ) |
		(secondary_expr~LEFTSQUARE()~expr~arrayop~expr~RIGHTSQUARE() ^^ 
		{case p     ~ _          ~lhs ~ op    ~rhs ~_            => BinaryArrayLookup(p,lhs,op,rhs)} ) |
		secondary_expr
	)
	
	def arrayop : Parser[ArrayOp] = positioned (
		COLON() ^^ (_=>BitRange()) |
		MINUSCOLON() ^^ (_=>MsbSelect()) |
		PLUSCOLON() ^^ (_=>LsbSelect())
	)
	
	def secondary_expr : Parser[AlogicAST] = positioned (
		TRUE() ^^ (_=>Num("1'b1")) |
		FALSE() ^^ (_=>Num("1'b0")) |
		LEFTBRACKET()~>expr<~RIGHTBRACKET() ^^ (e=>Bracket(e)) | 
		constant |
		LEFTCURLY()~expr~LEFTCURLY()~expr~RIGHTCURLY()~RIGHTCURLY() ^^
		{case _    ~e   ~_          ~e2  ~_           ~_            => BitRep(e,e2) } | 
		LEFTCURLY()~>repsep(expr,COMMA())<~RIGHTCURLY() ^^
		{x => BitCat(x) } | 
		dotted_name~LEFTBRACKET()~repsep(expr,COMMA())~ RIGHTBRACKET() ^^ {
		 case p     ~ _          ~args                ~ _              if (p.names.last==IDENTIFIER("read")) => ReadCall(p,args)
		 case p     ~ _          ~args                ~ _              if (p.names.last==IDENTIFIER("write")) => WriteCall(p,args)
		 case p     ~ _          ~args                ~ _              => FunCall(p,args)
		} |
		DOLLAR()~>identifier~LEFTBRACKET()~repsep(expr,COMMA())~ RIGHTBRACKET() ^^ {
		case      fn        ~_            ~args                ~_               => DollarFun(fn,args) } |
		dotted_name
	)
	
	
	def dotted_name : Parser[DottedName] = positioned (
		identifier~DOT()~repsep( identifier, DOT() ) ^^ {case x~_~ys=>DottedName(x::ys)} |
		identifier ^^(x=>DottedName(List(x)))
	)
	
	def field : Parser[FieldType] = positioned (
		(known_type ~! identifier ^^ {case t ~ id => Field(t,id)} ) <~ SEMICOLON()
	)
	
	def id : Parser[Name] = positioned {
		identifier ^^ {x => Name(x)}
	}
	
	def assign_op : Parser[AlogicToken] = positioned {
		EQUALS() | PLUSEQUALS() | MINUSEQUALS() | ANDEQUAL() | PIPEEQUAL() | LEFTSHIFTEQUAL() | RIGHTSHIFTEQUAL()
	}
	
	
	def is_control_stmt(cmd: AlogicAST) : Boolean = { // println(cmd); 
		cmd match {
		case FenceStmt() => true
		case BreakStmt() => true
		case ReturnStmt() => true
		case GotoStmt(target) => true
		case ControlBlock(s) => true
		case ControlIf(cond,body,elsebody) => true
		case WhileLoop(cond,body) => true
		case ControlFor(_,_,_,_) => true
		case ControlDo(_,_) => true
		case ControlCaseStmt(_,_) => true
		case FunCall(_,_) => true
		case _ => false
	} }
	
	def is_control_label(cmd: CaseLabel) : Boolean = cmd match {
		case ControlCaseLabel(_,_) => true
		case _ => false
	}
	
	def case_stmt : Parser[CaseLabel] = positioned {
		DEFAULT() ~> COLON() ~> statement ^^ {
			case x if (is_control_stmt(x)) => ControlCaseLabel(List(),x) 
			case x => CombinatorialCaseLabel(List(),x)
			} |
		repsep(expr,COMMA()) ~ COLON() ~ statement ^^ {
		case e ~ _ ~ body if (is_control_stmt(body)) => ControlCaseLabel(e,body)
		case e ~ _ ~ body => CombinatorialCaseLabel(e,body)
		}
	}
	
	// TODO do we support {a,b} = {c,d} ?
	// worried that declaration will activate before primary_expr, but ^? returns failure (not error!) so will backtrack to find good solution
	// worried that may want a single expression as well?  Handle with Option
	def statement : Parser[AlogicAST] = positioned (
		LEFTCURLY()~>rep(statement)<~RIGHTCURLY() ^? ( {
			case s if (s.length>0 && is_control_stmt(s.last)) => ControlBlock(s) 
			case s if (s.forall(x => !is_control_stmt(x))) => CombinatorialBlock(s)
			}, {s=>s"A control block must end with a control statement (This ends with ${s.last})"}) |
		declaration ^^ (d=>DeclarationStmt(d)) |
		WHILE() ~! LEFTBRACKET() ~! expr ~! RIGHTBRACKET() ~! statement ^? ( {
		case _  ~ _             ~ e    ~ _              ~ s if (is_control_stmt(s)) => WhileLoop(e,s)
		} , { _=>"A while loop must be followed by a control statement" } ) |
		IF()    ~! LEFTBRACKET() ~! expr ~! RIGHTBRACKET() ~! statement ~ opt( ELSE() ~ statement ) ^? ( {
		case _  ~ _              ~ e     ~ _               ~  s         ~ None if (is_control_stmt(s)) => ControlIf(e,s,None)
		case _  ~ _              ~ e     ~ _               ~  s         ~ None                         => CombinatorialIf(e,s,None)
		case _  ~ _              ~ e     ~ _               ~  s         ~ Some( _ ~ elsebody) if (is_control_stmt(s) && is_control_stmt(elsebody)) => ControlIf(e,s,Some(elsebody))
		case _  ~ _              ~ e     ~ _               ~  s         ~ Some( _ ~ elsebody) if (!is_control_stmt(s) && !is_control_stmt(elsebody)) => CombinatorialIf(e,s,Some(elsebody))
		} , { _=>"Both branches of an if must be control statements, or both must be combinatorial statements" } ) |
		CASE()~!LEFTBRACKET()~!expr~!RIGHTBRACKET()~!LEFTCURLY()~!rep(case_stmt)~RIGHTCURLY() ^? ( {
		case _~_             ~ value~_             ~_           ~stmts          ~_            if (stmts.forall(x=> is_control_label(x)))  => ControlCaseStmt(value,stmts)   
		case _~_             ~ value~_             ~_           ~stmts          ~_            if (stmts.forall(x=> !is_control_label(x))) => CombinatorialCaseStmt(value,stmts) },
		{ _ => "Either all or none of the case items must be control statements" } ) |
		FOR() ~! LEFTBRACKET() ~! single_statement ~! SEMICOLON() ~! expr ~! SEMICOLON() ~! single_statement ~! RIGHTBRACKET() ~! LEFTCURLY() ~! rep(statement) ~! RIGHTCURLY() ^^ {
		case _~  _             ~ inite             ~ _            ~  cond ~  _           ~  incr             ~  _              ~  _           ~  s              ~ _ => 
			ControlFor(inite,cond,incr,s)
		} |
		 DO() ~! LEFTCURLY() ~! rep(statement) ~! RIGHTCURLY() ~! WHILE() ~! LEFTBRACKET() ~! expr ~! RIGHTBRACKET() ~! SEMICOLON()  ^^ {
		case _~  _           ~  s              ~ _             ~  _       ~  _             ~  cond ~  _              ~  _ => 
			ControlDo(cond,s)
		} |
		single_statement <~ SEMICOLON()
	)
	
	// Single statement contains statements that are followed by a semicolon
	def single_statement : Parser[AlogicAST] = positioned (
		primary_expr<~PLUSPLUS() ^^ {x=> Plusplus(x)} |
		primary_expr<~MINUSMINUS() ^^ {x=> Minusminus(x)} |
		primary_expr~!opt(assign_op~!expr) ^^ {
			case lhs~Some(assign_op~rhs) => Assign(lhs,assign_op,rhs)
			case lhs~None => lhs
		} |
		FENCE() ^^ (_=>FenceStmt())  |
		BREAK() ^^ (_=>BreakStmt())  |
		RETURN() ^^ (_=>ReturnStmt())  |
		DOLLAR()~>LEFTBRACKET()~>literal<~RIGHTBRACKET() ^^ (s=>AlogicComment(s)) |
		GOTO()~identifier ^^ {case _~target =>GotoStmt(target)}
	)
	
	// Provide ways of accepting tokens with parameters
	
	// Need to return types that support the positional trait (for error reporting)
	// This can be AST types or simply tokens if they already contain the same information.
	
	// The accept below will print out the string as the expected token in the event of a failure
	// The accept takes a function, a token is matched if it is in the domain of the function!	
	private def identifier: Parser[IDENTIFIER] = positioned {  // TODO what should this return?
		accept("identifier", { case id @ IDENTIFIER(name) => id })
	}
	
	private def inttype: Parser[INTTYPE] = positioned {
		accept("integer type", { case id @ INTTYPE(signed,size) => id })
	}
	
	private def constant: Parser[Num] = positioned {
		accept("constant", { case CONSTANT(str) => Num(str) })
	}
	
	private def literal: Parser[LITERAL] = positioned {
		accept("string", { case id @ LITERAL(s) => id })
	}
	
	private def ident_sync: Parser[SyncSpec] = positioned {
		accept("sync specification", { 
			case id @ IDENTIFIER("sync") => IdentSync() 
			case id @ IDENTIFIER("wire") => IdentWire() 
			case id @ IDENTIFIER("bubble") => IdentBubble()
			case id @ IDENTIFIER("accept") => IdentAccept()
			case id @ IDENTIFIER("ready") => IdentReady()
		})
	}
	
	private def verilogstmt: Parser[VerilogFunction] = positioned {
		accept("Verilog function", { case id @ VERILOGSTMT(s) => VerilogFunction(id) })
	}
	
}

