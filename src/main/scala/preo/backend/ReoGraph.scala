package preo.backend

import preo.ast._
import preo.frontend.Show
import preo.common.TypeCheckException

/**
  * Created by jose on 21/07/16.
  * A graph is a list of edges, a list on input ports (identified with Ints), and a list of output ports.
  * In turn, each edge is a [[Prim]] with its list of inputs and outputs.
  */
case class ReoGraph(edges:List[ReoGraph.Edge], ins:List[Int], outs:List[Int]) {
  def ++(other:ReoGraph) = ReoGraph(edges++other.edges,ins++other.ins,outs++other.outs)
}


object ReoGraph {
  /** Represents a primitive of [[Prim]] from a list of input nodes to a list of output nodes.
    */
  case class Edge(prim: CPrim, ins:List[Int], outs:List[Int])

  private var seed:Int = 0 // global variable

  /**
    * Calculates and reduces a graph representation of a (instantiated and simplified) connector.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  def apply(prim:CoreConnector): ReoGraph = redGraph(toGraph(prim))

  /**
    * Calculates a graph representation of a (instantiated and simplified) connector.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  private def toGraph(prim:CoreConnector): ReoGraph = prim match {
    case CSeq(c1, c2) =>
      val (g1,g2) = (toGraph(c1),toGraph(c2))
      val g2b = subst(g2, g2.ins.zip(g1.outs).toMap )
      ReoGraph(g1.edges++g2b.edges, g1.ins,g2b.outs)
    case CPar(c1, c2) => toGraph(c1) ++ toGraph(c2)
    case CId(CoreInterface(v)) =>  //mkGrSyncs(v)
      val i = seed until seed+v
      val j = seed+v until seed+2*v
      seed += (2*v)
      ReoGraph(mkGrSyncs(i,j),i.toList,j.toList)
    case CSymmetry(CoreInterface(i), CoreInterface(j)) =>
      val ins   = seed until seed+i+j
      seed += (i+j)
      val outs1 = seed until seed+i+j
      val outs2 = (seed+i until seed+j+i)++(seed until seed+i)
      seed += (i+j)
      ReoGraph(mkGrSyncs(ins,outs1),ins.toList,outs2.toList)
    case CTrace(CoreInterface(i), c) =>
      val gc = toGraph(c)
      val ins =  gc.ins.takeRight(i)
      val outs = gc.outs.takeRight(i)
      val loop = mkGrSyncs(outs,ins)
      ReoGraph(gc.edges++loop,gc.ins.dropRight(i),gc.outs.dropRight(i))
    //      gc ++ Graph(mkGrSyncs(outs,ins),outs,ins)
    case p@CPrim(name, CoreInterface(pi), CoreInterface(pj), extra) =>
      val (i,j) = ((seed until seed+pi).toList,(seed+pi until seed+pi+pj).toList)
      seed += (pi+pj)
      ReoGraph(List(Edge(p,i,j)),i,j)
    case _ =>
      throw new TypeCheckException("Failed to compile a non-instantiated connector "+Show(prim))
  }

  private def subst(l:List[Int],m:Map[Int,Int]):List[Int] =
    l.map(x => if (m contains x) m(x) else x)
  private def subst(edge:Edge,m:Map[Int,Int]):Edge =
    Edge(edge.prim,subst(edge.ins,m),subst(edge.outs,m))
  private def subst(g:ReoGraph, m:Map[Int,Int]): ReoGraph =
    ReoGraph(g.edges.map(subst(_,m)),subst(g.ins,m),subst(g.outs,m))

  private def mkGrSyncs(i:Iterable[Int],j:Iterable[Int]): List[Edge] = {
    (for ((i,j) <- i.zip(j)) yield
      Edge(CPrim("sync", CoreInterface(1), CoreInterface(1)), List(i), List(j))).toList
  }

  private def redGraph(graph: ReoGraph): ReoGraph = {
    seed = 0
    val (es,m) = redGraphAux(graph.edges,List(),graph.ins.toSet)
    var res = ReoGraph(es,graph.ins,graph.outs)
    var map = m
    while (map.nonEmpty) {
      val (f,t) = map.head
      res = subst(res,Map(f->t))
      map = map.tail.map{
        case (a,`f`) => (a,t)
        case (`f`,b) => (t,b)
        case x => x
      }
    }
    res
  }

  def redGraphAux(es:List[Edge],m:List[(Int,Int)],ins:Set[Int]): (List[Edge],List[(Int,Int)]) = es match {
    case Nil => (es,m)
    case Edge(CPrim("sync",_,_,_),List(in),List(out))::tl if !ins.contains(in)=>
      val pair = out->in
      val (es2,m2) = redGraphAux(tl,m,ins)
      (es2,pair::m2)
    //    case Edge(CPrim("merger",_,_,_),List(in1,in2),List(out))::tl if !(ins(in1) || ins(in2))=>
    //      val pair1 = out->in1
    //      val pair2 = out->in2
    //      val (es2,m2) = redGraphAux(tl,m,ins)
    //      (es2,pair1::pair2::m2)
    case Edge(CPrim("dupl",_,_,_),List(in),List(out1,out2))::tl => // if !ins(in) =>
      val pair1 = out1->in
      val pair2 = out2->in
      val (es2,m2) = redGraphAux(tl,m,ins)
      (es2,pair1::pair2::m2)
    case edge::tl =>
      val (es2,m2) = redGraphAux(tl,m,ins)
      (edge::es2,m2)
  }
}
