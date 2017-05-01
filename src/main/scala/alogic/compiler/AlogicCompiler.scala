package alogic.compiler

import alogic.lexer.AlogicLexer
import alogic.parser.{AlogicParser, AlogicAST, FenceStmt}
import scala.io.Source

object AlogicCompiler {

  def loadFile(filename:String):String = {
	val bufferedSource = Source.fromFile(filename)
	val code = bufferedSource.mkString
	bufferedSource.close
	code
  }
  
  def parseFile(filename:String): AlogicAST = {
  
	// This is an example of using Either for exception handling
	// http://danielwestheide.com/blog/2013/01/02/the-neophytes-guide-to-scala-part-7-the-either-type.html
	// Errors will get passed down.
	// The right call marks this as a right projection - so further operations only occur if we are a Right
	{
		for {
		  tokens <- AlogicLexer(loadFile(filename)).right
		  ast <- AlogicParser(tokens).right
		} yield {
			ast
		} 
	} match {
		case Left(err) => {
			println(s"Failure $filename: $err")
			System.exit(-1)
			FenceStmt()
		}
		case Right(result) => result
	}
  }

  def apply(headerFiles:Array[String], codeFile: String): AlogicAST = {
    for {
      hdr <- headerFiles
	} parseFile(hdr)
	parseFile(codeFile)
  }
}

object Alogic extends App {
	if (args.length<1) {
		println("Syntax: alogic [Header_files*] source_file")
		System.exit(-1)
	}
	val result = AlogicCompiler(args.init, args.last)
    //println(s"Success: $result")
    println(s"Successfully compiled!")
}


