package preo.backend

import preo.ast.CPrim
import preo.backend.ReoGraph.Edge
import preo.common.{GenerationException, TypeCheckException}

/**
  * Representation of an automata, aimed at being generated from a [[ReoGraph]].
  * @param ports Represent the possible labels (actions)
  * @param init Initial state
  * @param trans Transitions - Relation between input and output states, with associated
  *              sets of actions and of edges (as in [[ReoGraph.Edge]]).
  */
case class PortAutomata(ports:Set[Int],init:Int,trans:Set[(Int,(Int,Set[Int],Set[Edge]))])
  extends Automata[PortAutomata] {


  /** Collects all states, seen as integers */
  override def getStates: Set[Int] = (for((x,(y,_,_)) <- trans) yield Set(x,y)).flatten
  // states: ints, transitions: maps from states to (new state,ports fired, primitives involved)

  /** Returns the initial state */
  override def getInit: Int = init

  /** Returns the transitions to be displayed */
  override def getTrans: Set[(Int, Any, String, Int)] =
    for ((from, (to, fire, es)) <- trans)
      yield (from, es.map(_.prim.name).filterNot(_ == "sync").mkString("."), (fire,es).hashCode().toString, to)

  private def printPrim(edge: Edge):String = {
    s"""${edge.prim.name}-${edge.prim.i.ports}-${edge.prim.j.ports}-${edge.ins.mkString(".")}-${edge.outs.mkString(".")}"""
  }

  /**
    * Automata composition - combining every possible transition,
    * and including transitions that can occur in parallel.
    * @param other automata to be composed
    * @return composed automata
    */
  def ++(other:PortAutomata): PortAutomata = {
    //     println(s"combining ${this.show}\nwith ${other.show}")
    var seed = 0
    val shared = other.ports intersect ports
    var restrans = Set[(Int,(Int,Set[Int],Set[Edge]))]()
    var newStates = Map[(Int,Int),Int]()
    def mkState(i1:Int,i2:Int) = if (newStates.contains((i1,i2)))
      newStates((i1,i2))
    else {
      seed +=1
      newStates += (i1,i2) -> seed
      seed
    }
    def ok(toFire:Set[Int]): Boolean = toFire.intersect(shared).isEmpty
    def ok2(toFire1:Set[Int],toFire2:Set[Int]): Boolean =
      toFire1.intersect(other.ports) == toFire2.intersect(ports)

    // just 1
    for ((from1,(to1,fire1,es1)) <- trans; p2 <- other.getStates)
      if (ok(fire1))
        restrans += mkState(from1,p2) -> (mkState(to1,p2),fire1,es1)
    // just 2
    for ((from2,(to2,fire2,es2)) <- other.trans; p1 <- getStates)
      if (ok(fire2))
        restrans += mkState(p1,from2) -> (mkState(p1,to2),fire2,es2)
    // communication
    for ((from1,(to1,fire1,es1)) <- trans; (from2,(to2,fire2,es2)) <- other.trans) {
      if (ok2(fire1,fire2))
        restrans += mkState(from1,from2) -> (mkState(to1,to2),fire1++fire2,es1++es2)
    }
    // println(s"ports: $newStates")
    val a = PortAutomata(ports++other.ports,mkState(init,other.init),restrans)
    //    println(s"got ${a.show}")
    val a2 = a.cleanup
    //    println(s"cleaned ${a2.show}")
    a2
  }


  private type Trans = Set[(Int,(Int,Set[Int],Set[Edge]))]

  private def cleanup: PortAutomata = {
    var missing = Set(init)
    var done = Set[Int]()
    var ntrans: Trans = Set()
    while (missing.nonEmpty) {
      val next = missing.head
      missing = missing.tail
      done += next
      for (t@(from, (to, _, _)) <- trans if from == next) {
        ntrans += t
        if (!(done contains to)) missing += to
      }
    }
    PortAutomata(ports, init, ntrans)
  }

  def show: String =
    s"$init:\n"+trans.map(x=>s" - ${x._1}->${x._2._1} "+
      s"${x._2._2.mkString("[",",","]")} "+
      s"${x._2._3.map(_.prim.name).mkString("(",",",")")}").mkString("\n")


}

object PortAutomata {

  /** How to build basic Port automata */
  implicit object PortAutomataBuilder extends AutomataBuilder[PortAutomata] {

    def buildAutomata(e: Edge, seed: Int): (PortAutomata, Int) = e match {
      case Edge(CPrim("sync", _, _, _), List(a), List(b)) =>
        (PortAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Set(e)))), seed + 1)
      case Edge(CPrim("id", _, _, _), List(a), List(b)) =>
        (PortAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Set(e)))), seed + 1)
      case Edge(CPrim("lossy", _, _, _), List(a), List(b)) =>
        (PortAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Set(e)), seed -> (seed, Set(a), Set(e)))), seed + 1)
      case Edge(CPrim("fifo", _, _, _), List(a), List(b)) =>
        (PortAutomata(Set(a, b), seed - 1, Set(seed - 1 -> (seed, Set(a), Set(e)), seed -> (seed - 1, Set(b), Set(e)))), seed + 2)
      case Edge(CPrim("fifofull", _, _, _), List(a), List(b)) =>
        (PortAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Set(e)), seed -> (seed - 1, Set(b), Set(e)))), seed + 2)
      case Edge(CPrim("drain", _, _, _), List(a, b), List()) =>
        (PortAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Set(e)))), seed + 1)
      case Edge(CPrim("merger", _, _, _), List(a, b), List(c)) =>
        (PortAutomata(Set(a, b, c), seed, Set(seed -> (seed, Set(a, c), Set(e)), seed -> (seed, Set(b, c), Set(e)))), seed + 1)
      case Edge(CPrim("dupl", _, _, _), List(a), List(b, c)) =>
        (PortAutomata(Set(a, b, c), seed, Set(seed -> (seed, Set(a, b, c), Set(e)))), seed + 1)
      case Edge(CPrim("writer", _, _, _), List(), List(a)) =>
        (PortAutomata(Set(a), seed, Set(seed -> (seed, Set(a), Set(e)))), seed + 1)
      case Edge(CPrim("reader", _, _, _), List(a), List()) =>
        (PortAutomata(Set(a), seed, Set(seed -> (seed, Set(a), Set(e)))), seed + 1)

      case Edge(p, _, _) =>
        throw new GenerationException(s"Unknown port automata for primitive $p")
    }

    def emptyAutomata = PortAutomata(Set(), 0, Set())
  }

}

