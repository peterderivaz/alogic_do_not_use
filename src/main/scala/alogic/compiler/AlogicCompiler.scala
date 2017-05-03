package alogic.compiler

import alogic.lexer.AlogicLexer
import alogic.parser.{AlogicParser, AlogicAST, FenceStmt}
import scala.io.Source

//  for file in ../build/*.alogic ; do sbt "run ../build/interfaces.h $file"; done

import java.io.File

object AlogicCompiler {

  def loadFile(filename:String):String = {
	val bufferedSource = Source.fromFile(filename)
	val code = bufferedSource.mkString
	bufferedSource.close
	code
  }
  
  def parseFile(filename:String,lexer:AlogicLexer,parser:AlogicParser): AlogicAST = {
	// This is an example of using Either for exception handling
	// http://danielwestheide.com/blog/2013/01/02/the-neophytes-guide-to-scala-part-7-the-either-type.html
	// Errors will get passed down.
	// The right call marks this as a right projection - so further operations only occur if we are a Right
	{
		println(s"Loading $filename")
		for {
		  tokens <- lexer(loadFile(filename)).right
		  ast <- parser(tokens).right
		} yield {
			ast
		}
	} match {
		case Left(err) => {
			println(s"Failure $filename: $err")
			System.exit(-1)
			FenceStmt()
		}
		case Right(result) => {
			println(s"Finished $filename")
			result
		}
	}
  }
  
  def getListOfFiles(dir: File): List[File] = {
    dir.listFiles.filter(_.isFile).toList.filter { s => s.getName.endsWith("alogic") }
  }

  def apply(headerFiles:Array[String], codeFile: String): AlogicAST = {
	val baselexer = new AlogicLexer()
	val baseparser = new AlogicParser()

    for {
      hdr <- headerFiles
	} parseFile(hdr,baselexer,baseparser)
	
	val d = new File(codeFile)
	if (d.exists && d.isDirectory) {
		val lst = getListOfFiles(d)
		// Start threads
		val threads = for {f <- lst} yield {
		  // make copies of the Lexer and Parser to avoid conflicts
		  val thread = new Thread {
			override def run {
				parseFile(f.getPath,new AlogicLexer(baselexer.defines),new AlogicParser(baseparser.typedefs))
			}
		  }
		  thread.start
		  thread
		}
		// Join threads
		for {t <- threads} t.join()
		FenceStmt()
	} else {
		parseFile(codeFile,baselexer,baseparser)
	}
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


