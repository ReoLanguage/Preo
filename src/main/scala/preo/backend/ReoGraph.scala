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
  case class Edge(prim: CPrim, ins:List[Int], outs:List[Int], parents:List[String]) {
    override def toString: String = s"{(${ins.mkString(",")})-${prim.name}/${prim.extra.mkString(",")}->(${outs.mkString(",")})}"
  }

  private var seed:Int = 0 // global variable
  private var hiddenSeed:Int = 5000
  private var prioritySeed:Int = 0 // measure to assign priority to edges

  /**
    * Calculates and reduces a graph representation of a (instantiated and simplified) connector.
    * Nodes are not guaranteed to have at most one input edge and one output edge.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  def apply(prim:CoreConnector,hideClosed:Boolean = true): ReoGraph = { // used by circuit
//    toGraphOneToOne(prim,hideClosed) //
    simplifyGraph(toGraphOneToOne(prim,hideClosed)) // original
//    simplifyGraph2(toGraphOneToOne(prim,hideClosed)) // replacing dupls/mergers by nodes and dropping sync
  }

  /**
    * Same as `apply`, but nodes have at most one input/output edge.
    * Dupl, merger, and xor are transformed into "node" primitives.
    * @param prim connector to be converted.
    * @param hideClosed replace hidden subconnectors by a "box"
    * @return simplified graph
    */
  def toGraphOneToOneSimple(prim:CoreConnector,hideClosed:Boolean = true): ReoGraph = {
    simplifyGraph2(toGraphOneToOne(prim,hideClosed)) // replacing dupls/mergers by nodes and dropping sync
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

      // HIDING subonnector
    case CSubConnector(name, sub, anns)
        if hideSome && Annotation.hidden(anns) =>
      val (t,_) = preo.DSL.unsafeTypeOf(sub.toConnector)
      val oldseed = seed
      if (oldseed<5000)
        seed = hiddenSeed
      val res = toGraph(CPrim(s"$name",preo.frontend.Eval.reduce(t.i),preo.frontend.Eval.reduce(t.j),Set("box")),hideSome)
      if (oldseed<5000) {
        hiddenSeed = seed
        seed = oldseed
      }
      res
//      toGraph(CPrim(s"[$name]",preo.frontend.Eval.reduce(preo.frontend.Simplify(t.i)),preo.frontend.Eval.reduce(preo.frontend.Simplify(t.j))))

      // NOT HIDING subconnector
    case CSubConnector(name, sub, _)=>
      val oldseed = seed
      if (oldseed<5000)
        seed = hiddenSeed
      val g = toGraph(sub,hideSome)
      if (oldseed<5000) {
        hiddenSeed = seed
        seed = oldseed
      }
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
  type IOMapF = Int => Set[Edge]

  /**
    * Simplify a graph by dropping syncs, converting dupl/merger/xor to nodes,
    * and merging compatible nodes.
    * @param g graph to be simplified
    * @return simplified graph
    */
  def simplifyGraph2(g: ReoGraph): ReoGraph = {
    //println("ReoGraph before: "+g)
//    val (es,remap) = dropSyncs(g,false)
//    val g2 = applyRemap(ReoGraph(es,g.ins,g.outs),remap)
//    println("ReoGraph no syncs: "+g2)
    val g3 = ReoGraph(g.edges.map(toNode),g.ins,g.outs)

    val (inmap,outmap) = collectInsOuts(g3)
    val maps = joinMap(inmap,outmap)

    //println("ReoGraph after toNode: "+g3)
    val edg4 = traverse(Set(),Set(),g3.edges,maps)
    val g4 = ReoGraph(edg4,g3.ins,g3.outs)
    //println("ReoGraph after traversal: "+g4)

    val g5 = fixLoops(g4)
//    val g6 = addBorderSyncs(g5)
    //println("ReoGraph after border syncs: "+g6)
    g5
  }

  private def joinMap(m1:IOMap,m2:IOMap): IOMapF = i =>
    m1.getOrElse(i,Set()) ++ m2.getOrElse(i,Set())
//    m2.headOption match {
//    case Some((k,v)) =>
//      val rest = joinMap(m1,m2.tail)
//      rest + (k -> (rest.getOrElse(k,Set())++v))
//  }

  def traverse(fringe: Set[Edge], done:Set[Edge], rest: List[Edge], maps: IOMapF): List[Edge] = {
    val (mbEdge,fringe2,rest2) = getNext(fringe,rest)
    mbEdge match {
      case Some(edge: Edge) =>
        //println(s"## traversing $edge")
        val neighbours = getNeighb(edge,done,maps)
        if (neighbours.isEmpty)
          edge :: traverse(fringe2,done+edge,rest2,maps)
        else {
          //println(s"## joining with ${neighbours.mkString("/")}")
          val newEdge = joinAll(edge,neighbours)
          //println(s"## joined  ${newEdge}")
          traverse(fringe2+newEdge--neighbours,(done+edge)++neighbours,rest2.filterNot(neighbours),maps)
        }
      case _ => Nil
    }
  }
  private def getNext(fringe: Set[Edge], rest: List[Edge])
     :(Option[Edge],Set[Edge],List[Edge]) = {
    fringe.headOption match {
      case Some(e) => (Some(e),fringe.tail,rest)
      case _ => rest match {
        case e::rest2 => (Some(e),fringe,rest2)
        case Nil => (None,fringe,rest)
      }
    }
  }
  private def getNeighb(edge: Edge, done: Set[Edge], m: IOMapF): Set[Edge] = {
    val around: Set[Edge] = (edge.ins.toSet++edge.outs.toSet).flatMap(m) - edge -- done
    //println(s"## around: ${around.mkString(",")}")
    //println(s"## compat: ${around.filter(edgeCompat(edge,_)).mkString(",")}")
    around.filter(edgeCompat(edge,_))
  }
  private def edgeCompat(e1: Edge, e2: Edge): Boolean = {
     //print(s"## compat? $e1 vs. $e2 ")
    (e1,e2) match {
      // two compatible nodes
      case (Edge(CPrim("node", _, _, ex1), i1, o1, _)
      , Edge(CPrim("node", _, _, ex2), i2, o2, _)) =>
        val xordupl = !(((ex1 contains "xor") && (ex2 contains "dupl")) ||
          ((ex2 contains "xor") && (ex1 contains "dupl")))
        lazy val onelink =
          if ((i1.toSet intersect o2.toSet).nonEmpty)
            i1.size <= 1 || o2.size <= 1
          else i2.size <= 1 || o1.size <= 1
        // println(s"$xordupl /\\ $onelink")
        xordupl && onelink
      // one is a port/sync/id
      case (Edge(CPrim(name1, _, _, _), _, _, _)
           ,Edge(CPrim(name2, _, _, _), _, _, _)) =>
        // println(Set("port","id","sync").intersect(Set(name1,name2)).nonEmpty)
        Set("port","id","sync").intersect(Set(name1,name2)).nonEmpty
      case _ => {/*println("NO");*/ false}
    }}
  private def joinAll(e:Edge,es: Set[Edge]): Edge = {
    es.fold[Edge](e)((e1,e2) => {
      val res = joinNodes(e1, e2)
      // println(s"joining $e1+$e2 = $res")
      res
    })
  }
  private def joinNodes(e1:Edge,e2:Edge): Edge = (e1,e2) match {
    // two compatible nodes
    case (Edge(CPrim("node",CoreInterface(is1),CoreInterface(js1),ex1),ins1,outs1,ps1)
         ,Edge(CPrim("node",CoreInterface(is2),CoreInterface(js2),ex2),ins2,outs2,ps2)) =>
      val newpars = if (ps2.length>ps1.length) ps2 else ps1
      val ins  = ins1.toSet ++ ins2.toSet -- outs1.toSet -- outs2.toSet
      val outs = outs1.toSet ++ outs2.toSet -- ins1.toSet -- ins2.toSet
      // println(s"$ins $outs $ins1 $outs1 $ins2 $outs2")
      if (ins1.size+outs1.size+ins2.size+outs2.size - ins.size - outs.size <= 2)
        Edge(CPrim("node",CoreInterface(ins.size),CoreInterface(outs.size),ex1++ex2)
                  ,ins.toList,outs.toList,newpars)
      else throw new RuntimeException(s"Failed to combine nodes $e1 and $e2 - more than one shared end.")
    // one is a port/sync/id
    case (Edge(CPrim(name1,ip1,op1,e1),i1,o1,ps1)
         ,Edge(CPrim(name2,ip2,op2,e2),i2,o2,ps2)) =>
      val newpars = if (ps2.length>ps1.length) ps2 else ps1
      val (newname,ip,op) =
         if (Set("sync","id","port") contains name1)
           (if (Set("sync","id") contains name2) (name1,ip1,op1) else (name2,ip2,op2))
         else (name1,ip1,op1)
      val (i,o) = if (Set("sync","id","port") contains name1)
          (replace(i2,o1.head->i1.head),replace(o2,i1.head->o1.head))
        else
          (replace(i1,o2.head->i2.head),replace(o1,i2.head->o2.head))
      Edge(CPrim(newname,ip,op,e1++e2),i,o,newpars)
//      if (o1.intersect(i2).nonEmpty)
//        Edge(CPrim(newname,ip,op,e1++e2),,o1+o2-i1-i2,newpars)
//      else
//        Edge(CPrim(newname,ip,op,e1++e2),i2,o1,newpars)
    case _ => throw new RuntimeException(s"Failed to combine edges $e1 and $e2.")
  }

  private def replace(ints: List[Int], mp: (Int, Int)): List[Int] =
    ints.map(x => if (x==mp._1) mp._2 else x )

  private def toNode(e:Edge): Edge = e match {
    case Edge(CPrim("dupl",i,j,e),ins1,outs1,ps1) =>
      Edge(CPrim("node",i,j,e+"dupl"),ins1,outs1,ps1)
    case Edge(CPrim("xor",i,j,e),ins1,outs1,ps1) =>
      Edge(CPrim("node",i,j,e+"xor"),ins1,outs1,ps1)
    case Edge(CPrim("merger",i,j,e),ins1,outs1,ps1) =>
      Edge(CPrim("node",i,j,e+"mrg"),ins1,outs1,ps1)
    case e => e
  }

  // Original approach, quite ad hoc.

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
//    val g7 = addBorderSyncs(g6)
    g6
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
  private def dropSyncs(graph: ReoGraph,keepBoundary:Boolean=true): (List[Edge],Map[Int,Int]) = graph.edges match {
    case Nil => (graph.edges,Map())
    case (e@Edge(CPrim("sync",_,_,_),List(in),List(out),_))::tl =>
      val (es,m) = dropSyncs(ReoGraph(tl,graph.ins,graph.outs),keepBoundary)
      if (!keepBoundary) {
        if (graph.outs.contains(out)) (es,m+(in->out))
        else (es,m+(in->out))
      }
      // do not remove if connected to some boundary
      else if (graph.ins.contains(in) || graph.outs.contains(out))
        (e::es,m)
      else
        (es,m+(out->in))
    case e::tl =>
      val (es,m) = dropSyncs(ReoGraph(tl,graph.ins,graph.outs),keepBoundary)
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
