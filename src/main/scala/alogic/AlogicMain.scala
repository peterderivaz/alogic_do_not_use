
package alogic
import java.io.File

import akka.actor.ActorSystem
import com.beachape.filemanagement.MonitorActor
import com.beachape.filemanagement.RegistryTypes._
import com.beachape.filemanagement.Messages._

import java.io.{FileWriter, BufferedWriter}

import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds._

object AlogicMain extends App {

  val multiThreaded = false;

  def getListOfFiles(dir: File): List[File] = {
    dir.listFiles.filter(_.isFile).toList.filter { s => s.getName.endsWith("alogic") }
  }
  
  val useMonitor = args.length>1 && args(0)=="-m"
  val args2 = if (useMonitor) args.tail else args
  
  if (args2.length>2 || args2.length<1) {
      println("Syntax: alogic [-m] [header file]* (source_file|source_dir)")
      println("-m tells alogic to recompile whenever the source changes.")
      System.exit(-1)
  }
  go
  go
  if (useMonitor) {
    implicit val system = ActorSystem("actorSystem")
    val fileMonitorActor = system.actorOf(MonitorActor(concurrency = 2))
    println(s"Waiting for ${args(1)} to be modified (press return to quit)...") 
    fileMonitorActor ! RegisterCallback(
      event = ENTRY_MODIFY,
      path = Paths get args(1),
      callback =  {_=>go}
      )
    io.StdIn.readLine()
    println("Quitting")
    system.terminate()
  } 
  
  def go() {
    val t0 = System.nanoTime()
    val codeFile = args.last
    // Parse header files
    val parser = new AParser()
    for (f <- args2.init) parser(f)

    val d = new File(codeFile)
    if (d.exists && d.isDirectory) {
        val lst = getListOfFiles(d)
        if (multiThreaded) {
          // Start threads
          val threads = for {f <- lst} yield {
            // make copies of the Lexer and Parser to avoid conflicts
            val thread = new Thread {
              override def run {
                  val parser2 = new AParser(parser)
                  parser2(f.getPath)
              }
            }
            thread.start
            thread
          }
          // Join threads
          for {t <- threads} t.join()
        } else {
          for {f <- lst} {
            val parser2 = new AParser(parser)
            parser2(f.getPath)
          }
        }
    } else {
        val s = parser(codeFile)
        // println(s.parseTree)
    }
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000000.0 + "s")
    
  }
}