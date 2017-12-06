package preo

import preo.DSL._
import preo.backend.{ReoGraph, Springy, Graph}
import preo.frontend.Eval

object Main extends App {

  args.toList match {
    case file::_ =>
      val conn = parse2(readFromFile(file))
      println("connector: "+conn)
      println("reduced: "+Eval.reduce(conn))
    case _ =>
      val conn = parse2(scala.io.StdIn.readLine())
      println("connector: "+conn)
      println("reduced: "+Eval.reduce(conn))
      println(ReoGraph(Eval.reduce(conn)))
      println(Graph(reduce(conn)))

  }


  /**
    * To use: printToFile(new File("abc")) { p => p.println("content...") }
    * @param f
    * @param op
    */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def readFromFile(filename: String): String = {
    val source = scala.io.Source.fromFile(filename)
    try source.mkString finally source.close()
  }



}
