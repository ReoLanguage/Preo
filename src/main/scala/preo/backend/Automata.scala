package preo.backend

import preo.ast.CoreConnector
import preo.backend.ReoGraph._

/**
  * Representation of an automata, aimed at being generated from a [[ReoGraph]].
  * @tparam A is the specific automata typs
  */
trait Automata[A <: Automata[A]] {

  /** Set of states of the automata, represented as integers */
  def getStates: Set[Int]
  // states: ints, transitions: maps from states to (new state,ports fired, primitives involved)

  /** Returns the initial state */
  def getInit: Int

  /** Returns the transitions to be displayed */
  def getTrans: Set[(Int,Any,String,Int)] // from, label, id, to

  /**
    * Automata composition - combining every possible transition,
    * and including transitions that can occur in parallel.
    * @param other automata to be composed
    * @return composed automata
    */
  def ++(other:A): A

  /** An easier to read representation */
  def show: String
}

object Automata {

  private var seed = 0

  def apply[A<:Automata[A]](str:String)
                           (implicit builder: AutomataBuilder[A]): A = {
    val c = preo.DSL.parse(str)
    val cc = preo.DSL.reduce(c)
    apply(cc)
  }

  def apply[A<:Automata[A]](cc:CoreConnector)
                           (implicit builder: AutomataBuilder[A]): A = {
    seed = 0
    val gr = ReoGraph.toGraphWithoutSimpl(cc)
//    println("about to create automata from\n"+gr)
    buildAutomata[A](gr)
  }


  /**
    * Build an automaton by starting at a random edge, and follow neighbours.
    * @param g graph to be converted into an automaton
    * @return
    */
  private def buildAutomata[A<:Automata[A]](g: ReoGraph)
                                           (implicit builder:AutomataBuilder[A]): A = {
    val (ins,outs) = collectInsOuts(g)
    def getNeighbours(e:Edge): List[Edge] =
      (for (i <- e.ins)  yield outs.getOrElse(i,Set())).flatten ++
      (for (o <- e.outs) yield ins.getOrElse(o,Set())).flatten


    if (g.edges.nonEmpty) {
      var prev = g.edges.head
      var missing = g.edges.toSet - prev
//      println(s"next: ${prev.prim.name} ${prev.ins} ${prev.outs} ${prev.priority}")
      val aut2 = builder.buildAutomata(prev,seed)
      var aut = aut2._1
      seed = aut2._2
      var next = getNeighbours(prev)
      //    var next = if (g.ins.nonEmpty) ins(g.ins.head)
      //    for (in <- g.ins.headOption; set <- ins.get(in); e <- )

      while (missing.nonEmpty) {
        while (next.nonEmpty) {
          // pop "prev" from "next"
          prev = next.head
          next = next.tail
          val prim = builder.buildAutomata(prev,seed)
          seed = prim._2
//          println(s"next: ${prev.prim.name} ${prev.ins} ${prev.outs} ${prev.priority}")
          aut = aut ++ prim._1 // update automata with "prev"
          missing -= prev // add "prev" to known edges
        }
        if (missing.nonEmpty) {
          prev = missing.head
//          println(s"next: ${prev.prim.name} ${prev.ins} ${prev.outs} ${prev.priority}")
          val prim = builder.buildAutomata(prev,seed)
          seed = prim._2
          aut = aut ++ prim._1
          next = getNeighbours(prev)
          missing = missing.tail
        }
      }
      aut
    }
    else
      builder.emptyAutomata
  }
}

trait AutomataBuilder[A<:Automata[A]] {
  def buildAutomata(e:Edge,seed:Int): (A,Int)
  def emptyAutomata: A
}
