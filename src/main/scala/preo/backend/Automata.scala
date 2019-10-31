package preo.backend

import preo.ast.CoreConnector
import preo.backend.Network._

/**
  * Representation of an automata, aimed at being generated from a [[Network]].
  */
trait Automata {

  /** Set of states of the automata, represented as integers */
  def getStates: Set[Int]
  // states: ints, transitions: maps from states to (new state,ports fired, primitives involved)

  /** Returns the initial state */
  def getInit: Int

  /** Returns the transitions to be displayed */
  def getTrans(fullName:Boolean = false): Automata.Trans // set of (from, label, id, to)

//  /**
//    * Automata composition - combining every possible transition,
//    * and including transitions that can occur in parallel.
//    * @param other automata to be composed
//    * @return composed automata
//    */
//  def ++(other:A): A

  /** An easier to read representation */
  def show: String
}

object Automata {

  /**
    * Set of transitions (from:Int, label:Any, id:String, to:Int)
    */
  type Trans = Set[(Int,Any,String,Int)] // from, label, id, to

  private var seed = 0

  def apply[A<:Automata](str:String, mirrors: Mirrors)
                        (implicit builder: AutomataBuilder[A]): A = {
    val c = preo.DSL.parse(str)
    val cc = preo.DSL.reduce(c)
    apply(cc,mirrors)(builder)
  }

  def apply[A<:Automata](cc:CoreConnector, mirrors: Mirrors = new Mirrors())
                        (implicit builder: AutomataBuilder[A]): A = {
    seed = 0
    val gr = Network(cc,hideClosed = false,mirrors)
    buildAutomata[A](gr)(builder)
  }

  def toAutWithRedundandy[A<:Automata](cc:CoreConnector, mirrors: Mirrors = new Mirrors())
                                      (implicit builder: AutomataBuilder[A]): A = {
    seed = 0
    val gr = Network.toNetwWithRedundancy(cc,hideClosed = false, mirrors)
    buildAutomata[A](gr)(builder)
  }


  //  def fromOneToOneSimple[A<:Automata](str:String)
//                        (implicit builder: AutomataBuilder[A]): A = {
//    val c = preo.DSL.parse(str)
//    val cc = preo.DSL.reduce(c)
//    fromOneToOneSimple(cc)(builder)
//  }
//
//  def fromOneToOneSimple[A<:Automata](cc:CoreConnector)
//                        (implicit builder: AutomataBuilder[A]): A = {
//    seed = 0
//    val gr = Network.toGraphOneToOneSimple(cc,hideClosed = false)
//    // println("about to create automata from\n"+gr)
//    buildAutomata[A](gr)(builder)
//  }


  /**
    * Build an automaton by starting at a random edge, and follow neighbours.
    * @param g graph to be converted into an automaton
    * @return
    */
  private def buildAutomata[A<:Automata](g: Network)
                                        (implicit builder:AutomataBuilder[A]): A = {
    val (ins,outs) = collectInsOuts(g)
    def getNeighbours(e:Prim): List[Prim] =
      ((for (i <- e.ins)  yield outs.getOrElse(i,Set())).flatten ++
      (for (o <- e.outs) yield ins.getOrElse(o,Set())).flatten).distinct //to remove duplicates


    if (g.prims.nonEmpty) {
      var prev = g.prims.head
      var missing = g.prims.toSet - prev
      // println(s"- next (1): ${prev.prim.name} ${prev.ins} ${prev.outs} ")
      val aut2 = builder.buildAutomata(prev,seed)
      var aut = aut2._1
      seed = aut2._2
      var next = getNeighbours(prev)
      //    var next = if (g.ins.nonEmpty) ins(g.ins.head)
      //    for (in <- g.ins.headOption; set <- ins.get(in); e <- )

      // TODO: call "hide" when relevant
      while (missing.nonEmpty) {
        while (next.nonEmpty) {
          // pop "prev" from "next"
          prev = next.head
          next = next.tail
          val prim = builder.buildAutomata(prev,seed)
          seed = prim._2
          // println(s"- next (2): ${prev.prim.name} ${prev.ins} ${prev.outs} ")
          aut = builder.join(aut , prim._1) // update automata with "prev"
          missing -= prev // add "prev" to known edges
        }
        if (missing.nonEmpty) {
          prev = missing.head
          // println(s"- next (t): ${prev.prim.name} ${prev.ins} ${prev.outs}")
          val prim = builder.buildAutomata(prev,seed)
          seed = prim._2
          aut = builder.join (aut , prim._1)
          next = getNeighbours(prev).toSet.intersect(missing).toList // otherwise it can add an already visited edge (as alternator example)
          missing = missing.tail
        }
      }
      aut
    }
    else
      builder.emptyAutomata
  }
}

trait AutomataBuilder[A<:Automata] {
  def buildAutomata(e:Prim, seed:Int): (A,Int)
  def emptyAutomata: A
  /**
    * Automata composition - combining every possible transition,
    * and including transitions that can occur in parallel.
    * @param a1 1st automaton to be composed
    * @param a2 1st automaton to be composed
    * @return composed automata
    */
  def join(a1:A,a2:A): A

  // TODO: add "hide" construct

}
