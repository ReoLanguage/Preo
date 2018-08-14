package preo

import preo.DSL._
import preo.backend.{Graph, ReoGraph, Springy}
import preo.frontend.Eval
import preo.frontend.mcrl2.{Model}//, Mcrl2Program, Mcrl2FamilyModel}

object Main extends App {

  args.toList match {
    case file::_ =>
      val conn = parse(readFromFile(file))
      println("connector: "+conn)
      println("reduced: "+Eval.reduce(conn))
    case _ =>
      val conn = parse(scala.io.StdIn.readLine())
      println("connector: "+conn)
      println("reduced: "+Eval.reduce(conn))
      val model = Model(Eval.reduce(conn))
      println(model.generateLPS)
//      println(Model(Eval.reduce(conn)))

    //      println(ReoGraph.toGraph(Eval.reduce(conn)))
//      println(Mcrl2Program(Eval.reduce(conn)))
//      println(Mcrl2Model(Eval.reduce(conn)))
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
