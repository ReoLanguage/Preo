package preo.backend

import preo.ast._
import preo.frontend.Show
import preo.common.TypeCheckException

import scala.collection.immutable

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
  def apply(prim:CoreConnector): ReoGraph = {
    seed=0
    //    reduceGraph(toGraph(prim))
    simplifyGraph(toGraph(prim))
  }

  def toGraphWithoutSimpl(prim:CoreConnector): ReoGraph = {
    seed=0
    toGraph(prim)
  }

  /**
    * Calculates a graph representation of a (instantiated and simplified) connector.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  def toGraph(prim:CoreConnector): ReoGraph = prim match {
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
    case CSubConnector(name, sub) => toGraph(sub)
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


  /**
    * Simplifies a graph, by:
    *  - removing sync channels;
    *  - converting some mergers and duplicators into (reo) nodes.
    * @param g graph to be reduced
    * @return reduced graph
    */
  def simplifyGraph(g: ReoGraph): ReoGraph = {
    val (es,remap) = dropSyncs(g)
    val g2 = applyRemap(ReoGraph(es,g.ins,g.outs),remap)
    val (inmap,outmap) = collectInsOuts(g2)
    val (es2,remap2) = dropReplDupl(g2,inmap,outmap)
    val g3 = applyRemap(ReoGraph(es2,g2.ins,g2.outs),remap2)
    val g4 = applyRemap(ReoGraph(g3.edges,g3.ins,g3.outs),remap2)
    val g5 = applyRemap(ReoGraph(g4.edges,g4.ins,g4.outs),remap2)
    g5
  }

  /**
    * Replaces ports from one identifier to another
    * @param graph graph to be simplified
    * @param m remap to be applied
    * @return a modified graph
    */
  private def applyRemap(graph:ReoGraph,m:Map[Int,Int]): ReoGraph = {
    var res = graph
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

  /**
    * maps ports to their edges in a graph
    */
  def collectInsOuts(graph: ReoGraph): (Map[Int,Set[Edge]],Map[Int,Set[Edge]]) =
    collectInsOuts(graph.edges)
  private def collectInsOuts(edges: List[Edge]): (Map[Int,Set[Edge]],Map[Int,Set[Edge]]) = edges match {
    case Nil => (Map(),Map())
    case (e@Edge(_,eins,eouts)) :: rest =>
      var (ins,outs) = collectInsOuts(rest)
      for (i <- eins)
        ins = ins.updated(i,ins.getOrElse(i,Set()) + e)
      for (o <- eouts)
        outs = outs.updated(o, outs.getOrElse(o,Set()) + e)
      (ins,outs)
  }


  /**
    * Removes sync channels in a graph, assuming 1 to 1 connections
    * @param graph to be simplified
    * @return the new list of edges, and a remap of ports that need to be replaced.
    */
  private def dropSyncs(graph: ReoGraph): (List[Edge],Map[Int,Int]) = graph.edges match {
    case Nil => (graph.edges,Map())
    case Edge(CPrim("sync",_,_,_),List(in),List(out))::tl
      if (!graph.ins.contains(in)) && (!graph.outs.contains(out)) => // do not remove if connected to some boundary
      val (e,m) = dropSyncs(ReoGraph(tl,graph.ins,graph.outs))
      (e,m + (out -> in))
    case e::tl =>
      val (es,m) = dropSyncs(ReoGraph(tl,graph.ins,graph.outs))
      (e::es,m)
  }

  /**
    * Removes duplicators and mergers, in a smart way
    * @param g graph to be simplified
    * @param inmap map from input ports to edges (by collectInsOuts)
    * @param outmap map from output ports to edges (by collectInsOuts)
    * @return simplified graph
    */
  private def dropReplDupl(g:ReoGraph,inmap:Map[Int,Set[Edge]],outmap:Map[Int,Set[Edge]])
      : (List[Edge],Map[Int,Int]) = g.edges match {
    case Nil => (Nil,Map())
    case (edge@Edge(CPrim("dupl",_,_,_),List(i1),eo@List(o1,o2)))::tl =>
      val (e,m) = dropReplDupl(ReoGraph(tl,g.ins,g.outs),inmap,outmap)
      var syncs: List[Edge] = List()
      var binds: Map[Int,Int] = Map()
      for (o <- eo) {
        getEdge("merger", o, inmap) match {
          case Some(oa) =>
            syncs :::= mkGrSyncs(List(i1), List(oa))
            binds += o -> i1
          case None => if (allHaveEdges(List(o), inmap))
            binds += o -> i1 else syncs :::= mkGrSyncs(List(i1), List(o))
        }
      }
      (e:::syncs,m++binds)
    case Edge(CPrim("merger",_,_,_),ei@List(i1,i2),List(o1))::tl
        if allHaveEdges(ei,outmap)=>
      val (e,m) = dropReplDupl(ReoGraph(tl,g.ins,g.outs),inmap,outmap)
      val m2 = if (getEdge("dupl",i1,outmap).isDefined) m  else m  + (i1 -> o1)
      val m3 = if (getEdge("dupl",i2,outmap).isDefined) m2 else m2 + (i2 -> o1)
      (e,m3)
    case e::tl =>
      val (es,m) = dropReplDupl(ReoGraph(tl,g.ins,g.outs),inmap,outmap)
      (e::es,m)
  }

  private def allHaveEdges(ports: List[Int], m: Map[Int, Set[ReoGraph.Edge]]): Boolean =
    ports.forall(p => (m contains p) && m(p).nonEmpty)
  private def getEdge(name:String,port:Int,m:Map[Int, Set[ReoGraph.Edge]]): Option[Int] = {
    for (set <- m.get(port);
         e <- set.find(_.prim.name == name);
         h <- e.outs.headOption)
      yield h
  }

}
