package alogic.compiler

sealed trait AlogicCompilationError

case class AlogicLexerError(location: Location, msg: String) extends AlogicCompilationError
case class AlogicParserError(location: Location, msg: String) extends AlogicCompilationError

case class Location(line: Int, column: Int, longString: String) {
  override def toString = s"$line:$column\n$longString"
}
