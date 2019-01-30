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
  case class Edge(prim: CPrim, ins:List[Int], outs:List[Int], parents:List[String])

  private var seed:Int = 0 // global variable
  private var prioritySeed:Int = 0 // measure to assign priority to edges

  /**
    * Calculates and reduces a graph representation of a (instantiated and simplified) connector.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  def apply(prim:CoreConnector,hideClosed:Boolean = true): ReoGraph = {
    //    simplifyGraph(toGraphOneToOne(prim,hideClosed)) // original
    //    simplifyGraph2(toGraphOneToOne(prim,hideClosed)) // under development (not working)
        simplifyGraph3(toGraphOneToOne(prim,hideClosed)) // templ: replacing dupls/mergers by nodes and dropping sync
  }

  /**
    * Calculates a graph representation of a Core connector, where each node has at most one in and 1 out to-1.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  def toGraphOneToOne(prim:CoreConnector,hideClosed:Boolean = true): ReoGraph = {
    seed=0
    toGraph(prim,hideClosed)
  }

  /**
    * Calculates a graph representation of a (instantiated and simplified) connector.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  private def toGraph(prim:CoreConnector, hideSome:Boolean): ReoGraph = prim match {
    case CSeq(c1, c2) =>
      val (g1,g2) = (toGraph(c1,hideSome),toGraph(c2,hideSome))
      val g2b = subst(g2, g2.ins.zip(g1.outs).toMap )
      ReoGraph(g1.edges++g2b.edges, g1.ins,g2b.outs)
    case CPar(c1, c2) =>
      toGraph(c1,hideSome) ++ toGraph(c2,hideSome)
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
      val gc = toGraph(c,hideSome)
      val ins =  gc.ins.takeRight(i)
      val outs = gc.outs.takeRight(i)
      val loop = mkGrSyncs(outs,ins)
      val g = ReoGraph(gc.edges++loop,gc.ins.dropRight(i),gc.outs.dropRight(i))
      g
    //      gc ++ Graph(mkGrSyncs(outs,ins),outs,ins)
    case p@CPrim(_, CoreInterface(pi), CoreInterface(pj), _) =>
      val (i,j) = ((seed until seed+pi).toList,(seed+pi until seed+pi+pj).toList)
      seed += (pi+pj)
      ReoGraph(List(Edge(p,i,j,Nil)),i,j)
    case CSubConnector(name, sub, anns)
        if hideSome && Annotation.hidden(anns) =>
      val (t,_) = preo.DSL.unsafeTypeOf(sub.toConnector)
      toGraph(CPrim(s"$name",preo.frontend.Eval.reduce(t.i),preo.frontend.Eval.reduce(t.j),Set("box")),hideSome)
//      toGraph(CPrim(s"[$name]",preo.frontend.Eval.reduce(preo.frontend.Simplify(t.i)),preo.frontend.Eval.reduce(preo.frontend.Simplify(t.j))))
    case CSubConnector(name, sub, _)=>
//      prioritySeed += 1
      val g = toGraph(sub,hideSome)
//      prioritySeed -=1
      addParent(name,g)
    case _ =>
      throw new TypeCheckException("Failed to compile a non-instantiated connector "+Show(prim))
  }

  private def subst(l:List[Int],m:Map[Int,Int]):List[Int] =
    l.map(x => if (m contains x) m(x) else x)
  private def subst(edge:Edge,m:Map[Int,Int]):Edge =
    Edge(edge.prim,subst(edge.ins,m),subst(edge.outs,m),edge.parents)
  private def subst(g:ReoGraph, m:Map[Int,Int]): ReoGraph =
    ReoGraph(g.edges.map(subst(_,m)),subst(g.ins,m),subst(g.outs,m))

  private def addParent(dad:String,g:ReoGraph): ReoGraph =
    ReoGraph(g.edges.map(addParent(dad,_)),g.ins,g.outs)
  private def addParent(dad:String,e:Edge): Edge =
    Edge(e.prim,e.ins,e.outs,dad::e.parents)

  private def mkGrSyncs(i:Iterable[Int],j:Iterable[Int]): List[Edge] = {
    (for ((i,j) <- i.zip(j)) yield
      mkSync(i,j)).toList
//      Edge(CPrim("sync", CoreInterface(1), CoreInterface(1)), List(i), List(j), Nil)).toList
  }

  private def mkSync(i:Int,j:Int): Edge =
    Edge(CPrim("sync", CoreInterface(1), CoreInterface(1)), List(i), List(j), Nil)



  //////////////////
  type IOMap = Map[Int,Set[Edge]]

  def simplifyGraph3(g: ReoGraph): ReoGraph = {
    val (es,remap) = dropSyncs(g)
    val g2 = applyRemap(ReoGraph(es,g.ins,g.outs),remap)
    ReoGraph(g2.edges.map(toNode),g2.ins,g2.outs)

  }

  def simplifyGraph2(g: ReoGraph): ReoGraph = {
    val boundary = g.ins++g.outs
    val (inmap,outmap) = collectInsOuts(g)
    val fringe:Set[Edge] = g.edges.headOption.map(Set(_)).getOrElse(Set())
//      getNeighbours(g.ins, inmap) ++
//      getNeighbours(g.outs,outmap)
    val newEdges = traverse(fringe,Set(),g.edges.toSet -- fringe,inmap,outmap)
    ReoGraph(newEdges.toList,g.ins,g.outs)
  }

  def traverse(fringe: Set[Edge], done: Set[Edge], missing:Set[Edge], inmap:IOMap, outmap:IOMap): Set[Edge] = {
    println(s"fringe ${fringe.mkString(",")}")
    fringe.headOption match {
      case None =>
        missing.headOption match {
          case Some(edge) => traverse(Set(edge),Set(),missing-edge,inmap,outmap)
          case None => Set()
        }
      case Some(e1@Edge(prim, ins, outs, pars)) =>
        val nexts = (getNeighbours(ins, outmap) ++ getNeighbours(outs, inmap)) -- done
        var newedges = Set[Edge]()
        var tofringe = Set[Edge]()
        val e1n = toNode(e1)
        nexts.headOption match {
          case Some(e2) =>
            val e2n = toNode(e2)
            println(s"joining $e1n + $e2n")
            val e12 = join(e1n, e2n)
            println(s"joined ${e12.mkString(" - ")}")
            if (e12 contains e1n) {
              newedges += e1n
              tofringe ++= (e12 - e1n)
            }
            else
              tofringe ++= e12
          case None =>
            newedges += e1
        }
        newedges ++ traverse((tofringe ++ fringe) - e1, done + e1, missing--tofringe, inmap, outmap)
    }
  }

  private def getNeighbours(io: List[Int], map: IOMap): Set[Edge] =
    io.toSet.flatMap(map.getOrElse(_:Int,Set[Edge]()))

  private def toNode(e:Edge): Edge = e match {
    case Edge(CPrim("dupl",i,j,e),ins1,outs1,ps1) =>
      Edge(CPrim("node",i,j,e+"dupl"),ins1,outs1,ps1)
    case Edge(CPrim("xor",i,j,e),ins1,outs1,ps1) =>
      Edge(CPrim("node",i,j,e+"xor"),ins1,outs1,ps1)
    case Edge(CPrim("merger",i,j,e),ins1,outs1,ps1) =>
      Edge(CPrim("node",i,j,e),ins1,outs1,ps1)
    case e => e
  }
  private def join(e1:Edge,e2:Edge): Set[Edge] = (e1,e2) match {
    case (Edge(CPrim("sync",_,_,_),List(i),List(o),ps1)
         ,Edge(p2,ins,outs,ps2)) =>
      if (ins contains o)
           Set(Edge(p2,ins.filterNot(_==o),outs,ps2))
      else Set(Edge(p2,ins,outs.filterNot(_==o),ps2))
    case (e1,e2@Edge(CPrim("sync",_,_,_),_,_,_)) =>
      join(e2,e1) // infinite loop if sync has wrong interfaces
    case (Edge(CPrim("node",CoreInterface(is1),CoreInterface(js1),ex1),ins1,outs1,ps1)
         ,Edge(CPrim("node",CoreInterface(is2),CoreInterface(js2),ex2),ins2,outs2,ps2))
         if nodeCompat(ex1,ex2)  =>
      val newpars = if (ps2.length>ps1.length) ps2 else ps1
      val ins  = ins1.toSet ++ ins2.toSet -- outs1.toSet -- outs2.toSet
      val outs = outs1.toSet ++ outs2.toSet -- ins1.toSet -- ins2.toSet
      Set(Edge(CPrim("node",CoreInterface(ins.size),CoreInterface(outs.size),ex1++ex2)
        ,ins.toList,outs.toList,newpars))
    case _ => Set(e1,e2)
  }

  private def nodeCompat(ex1: Set[Any], ex2: Set[Any]): Boolean =
    !(((ex1 contains "xor") && (ex2 contains "dupl")) ||
      ((ex2 contains "xor") && (ex1 contains "dupl")))


  /**
    * Simplifies a graph, by:
    *  - removing sync channels;
    *  - converting some mergers and duplicators into (reo) nodes.
    * @param g graph to be reduced
    * @return reduced graph
    */
  def simplifyGraph(g: ReoGraph): ReoGraph = {
    // remove useless Sync channels
    val (es,remap) = dropSyncs(g)
    val g2 = applyRemap(ReoGraph(es,g.ins,g.outs),remap)
    // remove replicators and mergers when possible
    val (inmap,outmap) = collectInsOuts(g2)
    val (es2,remap2) = dropDuplMerg(g2,inmap,outmap)
    val g3 = applyRemap(ReoGraph(es2,g2.ins,g2.outs),remap2)
    val g4 = applyRemap(g3,remap2)
    val g5 = applyRemap(g4,remap2)
    // add syncs to border mergers and replicators
    val g6 = fixLoops(g5)
    val g7 = addBorderSyncs(g6)
    g7
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
    case (e@Edge(_,eins,eouts,_)) :: rest =>
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
    case Edge(CPrim("sync",_,_,_),List(in),List(out),_)::tl
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
  private def dropDuplMerg(g:ReoGraph, inmap:Map[Int,Set[Edge]], outmap:Map[Int,Set[Edge]])
      : (List[Edge],Map[Int,Int]) = g.edges match {
    case Nil => (Nil,Map())
    case (edge@Edge(CPrim("dupl",_,_,_),List(i1),eo@List(_,_),_))::tl =>
      val (e,m) = dropDuplMerg(ReoGraph(tl,g.ins,g.outs),inmap,outmap)
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
    case Edge(CPrim("merger",_,_,_),ei@List(i1,i2),List(o1),_)::tl
        if allHaveEdges(ei,outmap)=>
      val (e,m) = dropDuplMerg(ReoGraph(tl,g.ins,g.outs),inmap,outmap)
      val m2 = if (getEdge("dupl",i1,outmap).isDefined) m  else m  + (i1 -> o1)
      val m3 = if (getEdge("dupl",i2,outmap).isDefined) m2 else m2 + (i2 -> o1)
      (e,m3)
    case e::tl =>
      val (es,m) = dropDuplMerg(ReoGraph(tl,g.ins,g.outs),inmap,outmap)
      (e::es,m)
  }

  private def fixLoops(graph: ReoGraph): ReoGraph = {
    val (inmap,outmap) = collectInsOuts(graph)
    var res = graph
    for ((p,es) <- inmap; e <- es) {
      // found a direct loop
      if (e.outs contains p) {
        val e1 = mkSync(p,seed)
        val e2 = Edge(e.prim,e.ins.map(x=>if(x==p)seed else p),e.outs.map(x=>if(x==p)seed+1 else p),e.parents)
        val e3 = mkSync(seed+1,p)
        res = ReoGraph(e1::e2::e3:: res.edges.diff(List(e)),res.ins,res.outs)
        seed += 2
      }
      // found a 2-way loop
      for ( pj <- e.outs;
            ej <- inmap.getOrElse(pj,Set())
            if pj!=p && ej.outs.contains(p)) {
          val e1 = Edge(e.prim,e.ins,e.outs.map(x=>if(x==pj)seed else p),e.parents)
          val e2 = mkSync(seed,pj)
          res = ReoGraph(e1::e2:: res.edges.diff(List(e)),res.ins,res.outs)
          seed += 1
        }
      // found 2 edges going to the same place
      for ( pj <- e.outs;
            ej <- outmap.getOrElse(pj,Set())
            if (ej!=e || res.edges.count(_==e)>1) && ej.ins.contains(p)) {
        for (_ <- 1 to res.edges.count(_==e)) {
          val e1 = Edge(e.prim, e.ins, e.outs.map(x => if (x == pj) seed else p), e.parents)
          val e2 = mkSync(seed, pj)
          res = ReoGraph(e1 :: e2 :: res.edges.diff(List(e)), res.ins, res.outs)
          seed += 1
        }
      }
    }
    res
  }

  /**
    * Add extra Sync channels when having mergers or dupls in the borders
    * @param graph
    * @return
    */
  private def addBorderSyncs(graph: ReoGraph): ReoGraph = {
    val (inmap,outmap) = collectInsOuts(graph)
    var res = graph
    for (in <- graph.ins) {
      if ((inmap contains in) &&
          (inmap(in).size > 1 || inmap(in).exists(_.prim.name=="dupl"))) {
        val e = mkSync(seed,in)
        res = ReoGraph(e :: res.edges, seed :: res.ins.filterNot(_==in), res.outs)
        seed += 1
      }
    }
    for (out <- graph.outs) {
      if ((outmap contains out) &&
          (outmap(out).size > 1 || outmap(out).exists(_.prim.name=="merger"))) {
        val e = mkSync(out,seed)
        res = ReoGraph(e :: res.edges, res.ins, seed :: res.outs.filterNot(_==out))
        seed += 1
      }
    }
    res
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
