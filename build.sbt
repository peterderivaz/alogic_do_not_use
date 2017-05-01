import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.parser",
      scalaVersion := "2.12.1",
      version      := "0.1.1",
	  scalacOptions := Seq("-feature")
    )),
    name := "Parser",
	libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    libraryDependencies += scalaTest % Test
    
  )
