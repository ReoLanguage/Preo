package preo.backend

import preo.ast._
import preo.common.TypeCheckException
import preo.frontend.{Show, TreoLite}

import scala.collection.mutable

/**
  * Created by jose on 21/07/16.
  * A Network is a list of primitivies, a list on input ports (identified with Ints), and a list of output ports.
  * In turn, each primitive is a [[Prim]] with its list of inputs and outputs.
  *
  * Can be simplified, and reused by other tools, e.g., visualiser ([[Circuit]]) or automata semantics ([[PortAutomata]]).
  */
case class Network(prims:List[Network.Prim], ins:List[Network.Port], outs:List[Network.Port]) {
  def ++(other:Network) = Network(prims++other.prims,ins++other.ins,outs++other.outs)
}


object Network {
  /** Represents a primitive of [[Prim]] from a list of input nodes to a list of output nodes.
    */
  case class Prim(prim: CPrim, ins:List[Port], outs:List[Port], parents:List[String]) {
    override def toString: String = s"{(${ins.mkString(",")})-${prim.name}/${prim.extra.mkString(",")}->(${outs.mkString(",")})}"
  }
  /** A port is a shared integer between atmost 2 [[Prim]]s. */
  type Port = Int

  class Mirrors {
    private val mirrors = mutable.Map[Int,Set[Int]]()
    def +=(pair:(Int,Int)): Unit =
      mirrors(pair._2) = mirrors.getOrElse(pair._2,Set()) + pair._1

    def apply(src:Int): Set[Int] = {
      var res: Set[Int] = mirrors.getOrElse(src,Set())
      var more: Set[Int] = res
      while (more.nonEmpty) {
        val next: Set[Int] = mirrors.getOrElse(more.head,Set()) -- res
        more = more.tail
        res ++= next
        more ++= next
      }
      res
    }

    override def toString: String =
      mirrors.map(p => s"${p._1}->[${p._2.mkString(",")}]").mkString("  ")
  }

  private var seed:Int = 0 // global variable

  /**
    * Calculates and reduces a graph representation of a (instantiated and simplified) connector.
    * Nodes are not guaranteed to have at most one input edge and one output edge.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  def apply(prim:CoreConnector,
            hideClosed:Boolean = true, mirrors: Mirrors = new Mirrors()): Network = { // used by circuit
    val g = toGraphWithoutSimplification(prim,hideClosed)
    simplifyGraph(g, mirrors) // replacing dupls/mergers by nodes and dropping sync
  }

  def toNetwWithRedundancy(prim: CoreConnector,
                           hideClosed: Boolean=true,
                           mirrors: Mirrors = new Mirrors()): Network = {
    val n1 = apply(prim,hideClosed,mirrors)
    addRedundancy(n1,mirrors)
  }

//  /**
//    * Same as `apply`, but nodes have at most one input/output edge.
//    * Dupl, merger, and xor are transformed into "node" primitives.
//    * @param prim connector to be converted.
//    * @param mergeNodes replace hidden subconnectors by a "box"
//    * @return simplified graph
//    */
//  def toGraphOneToOneSimple(prim:CoreConnector,mergeNodes:Boolean = true): Network = {
//    simplifyGraph2(toGraphOneToOne(prim,mergeNodes)) // replacing dupls/mergers by nodes and dropping sync
//  }

  /**
    * Calculates a graph representation of a Core connector, where each node has at most one in and 1 out to-1.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  def toGraphWithoutSimplification(prim:CoreConnector, hideClosed:Boolean = true): Network = {
    seed=0
    toGraph(prim,hideClosed)
  }

  /**
    * Calculates a graph representation of a (instantiated and simplified) connector.
    *
    * @param prim connector to be converted to a graph
    * @return graph representation
    */
  private def toGraph(prim:CoreConnector, hideSome:Boolean): Network = prim match {
    case CSeq(c1, c2) =>
      val (g1,g2) = (toGraph(c1,hideSome),toGraph(c2,hideSome))
      val g2b = subst(g2, g2.ins.zip(g1.outs).toMap )
      Network(g1.prims++g2b.prims, g1.ins,g2b.outs)
    case CPar(c1, c2) =>
      toGraph(c1,hideSome) ++ toGraph(c2,hideSome)
    case CId(CoreInterface(v)) =>  //mkGrSyncs(v)
      val i = seed until seed+v
      val j = seed+v until seed+2*v
      seed += (2*v)
      Network(mkGrSyncs(i,j),i.toList,j.toList)
    case CSymmetry(CoreInterface(i), CoreInterface(j)) =>
      val ins   = seed until seed+i+j
      seed += (i+j)
      val outs1 = seed until seed+i+j
      val outs2 = (seed+i until seed+j+i)++(seed until seed+i)
      seed += (i+j)
      Network(mkGrSyncs(ins,outs1),ins.toList,outs2.toList)
    case CTrace(CoreInterface(i), c) =>
      val gc = toGraph(c,hideSome)
      val ins =  gc.ins.takeRight(i)
      val outs = gc.outs.takeRight(i)
      val loop = mkGrSyncs(outs,ins)
      val g = Network(gc.prims++loop,gc.ins.dropRight(i),gc.outs.dropRight(i))
      g
    //      gc ++ Graph(mkGrSyncs(outs,ins),outs,ins)
    case CTreo(treo) => toGraph(TreoLite.treo2preo(treo,"dupl"), hideSome)
      
    case p@CPrim(_, CoreInterface(pi), CoreInterface(pj), _) =>
      val (i,j) = ((seed until seed+pi).toList,(seed+pi until seed+pi+pj).toList)
      seed += (pi+pj)
      Network(List(Prim(p,i,j,Nil)),i,j)

      // HIDING subonnector
    case CSubConnector(name, sub, anns)
        if hideSome && Annotation.hidden(anns) =>
      val (t,_) = preo.DSL.unsafeTypeOf(sub.toConnector)
      val dummy = toGraph(sub,hideSome = false) // generating everything, but using only the port names
      val (i,j) = (dummy.ins,dummy.outs)
      val allPorts = dummy.prims.flatMap(prim => prim.ins:::prim.outs).toSet
//      val portNames = dummy.prims.flatMap(prim => prim.prim.extra.filter(e=> e.isInstanceOf[List[TVar]]))//.asInstanceOf[TVar]
      var portNames1:List[String] = dummy.prims.flatMap(prim=> prim.prim.extra.filter(e=> e.isInstanceOf[String])).asInstanceOf[List[String]]
      portNames1 = portNames1.filter(e=>e.startsWith("portName:")).map(e=>e.drop(9))
      val p = CPrim(s"$name",preo.frontend.Eval.reduce(t.i),preo.frontend.Eval.reduce(t.j),
                   if(anns.exists(e=>e.name=="task")) Set("box","task",("ports",allPorts),/*portNames.asInstanceOf[List[List[TVar]]].flatten,*/portNames1)
                   else Set("box",("ports",allPorts)))
      Network(List(Prim(p,i,j,Nil)),i,j)

//      toGraph(CPrim(s"[$name]",preo.frontend.Eval.reduce(preo.frontend.Simplify(t.i)),preo.frontend.Eval.reduce(preo.frontend.Simplify(t.j))))

      // NOT HIDING subconnector
    case CSubConnector(name, sub, _)=>
      val g = toGraph(sub,hideSome)
      addParent(name,g)

    case _ =>
      throw new TypeCheckException("Failed to compile a non-instantiated connector "+Show(prim))
  }

  private def subst(l:List[Int],m:Map[Int,Int]):List[Int] =
    l.map(x => if (m contains x) m(x) else x)
  private def subst(edge:Prim, m:Map[Int,Int]):Prim =
    Prim(edge.prim,subst(edge.ins,m),subst(edge.outs,m),edge.parents)
  private def subst(g:Network, m:Map[Int,Int]): Network =
    Network(g.prims.map(subst(_,m)),subst(g.ins,m),subst(g.outs,m))

  private def addParent(dad:String,g:Network): Network =
    Network(g.prims.map(addParent(dad,_)),g.ins,g.outs)
  private def addParent(dad:String,e:Prim): Prim =
    Prim(e.prim,e.ins,e.outs,dad::e.parents)

  private def mkGrSyncs(i:Iterable[Port],j:Iterable[Port]): List[Prim] = {
    (for ((i,j) <- i.zip(j)) yield
      mkSync(i,j)).toList
//      Edge(CPrim("sync", CoreInterface(1), CoreInterface(1)), List(i), List(j), Nil)).toList
  }

  private def mkSync(i:Port,j:Port): Prim =
    Prim(CPrim("sync", CoreInterface(1), CoreInterface(1)), List(i), List(j), Nil)



  //////////////////
  type IOMap = Map[Port,Set[Prim]]
  type IOMapF = Port => Set[Prim]

  /**
    * Simplify a graph by dropping syncs, converting dupl/merger/xor to nodes,
    * and merging compatible nodes.
    * @param g graph to be simplified
    * @return simplified graph
    */
  def simplifyGraph(g: Network, ms: Mirrors): Network = {
    //println("ReoGraph before: "+g+s"\nmirrors: $ms")
    val g3 = Network(g.prims.map(toNode),g.ins,g.outs)

    val (inmap,outmap) = collectInsOuts(g3)
    val maps = joinMap(inmap,outmap)

    val edg4 = traverse(Set(),Set(),g3.prims,maps, ms)
    val g4 = Network(edg4,g3.ins,g3.outs)

    g4
  }


  ///
  /**
    * add new sync channels to make the graph prettier when drawing in [[Circuit]]
    * @param g Network to be extended
    * @return new network
    */
  def addRedundancy(g:Network,ms:Mirrors,newseed:Option[Int]=None): Network = {
    newseed match {
      case Some(value) => seed = value
      case None =>
    }
    //println("Mirrors after traversal: "+ms)
    val g2  = fixLoops(g,ms)
    //println("ReoGraph after loops: "+g2+s"\nmirrors: $ms")
    val g3 = addBorderSyncs(g2,ms)
    //println("Mirrors after redundancy: "+ms)
    //println("ReoGraph after adding redundancy: "+g3+s"\nmirrors: $ms")
    g3
  }

  private def joinMap(m1:IOMap,m2:IOMap): IOMapF = i =>
    m1.getOrElse(i,Set()) ++ m2.getOrElse(i,Set())

  def traverse(fringe: Set[Prim], done:Set[Prim], rest: List[Prim], maps: IOMapF,
               ms:Mirrors): List[Prim] = {
    val (mbEdge,fringe2,rest2) = getNext(fringe,rest)
    mbEdge match {
      case Some(edge: Prim) =>
        //println(s"## traversing $edge")
        val neighbour = getCompatNeighb(edge,done,maps)
        if (neighbour.isEmpty)
          edge :: traverse(fringe2,done+edge,rest2,maps,ms)
        else {
          //println(s"## joining with ${neighbours.mkString("/")}")
//          val newEdge = joinAll(edge,neighbours,ms)
          //println(s"## joined  ${newEdge}")
//          traverse(fringe2+newEdge--neighbours,(done+edge)++neighbours,rest2.filterNot(neighbours),
//                   maps,ms)
          val neighb = neighbour.get
          //println(s"## joining with $neighb")
          val newEdge = joinPrimtitives(edge,neighb,ms)
          //println(s"## joined  $newEdge")
          traverse((fringe2+newEdge)-neighb,done+edge+neighb,rest2.filterNot(_ == neighb),
            maps,ms)
        }
      case _ => Nil
    }
  }
  private def getNext(fringe: Set[Prim], rest: List[Prim])
     :(Option[Prim],Set[Prim],List[Prim]) = {
    fringe.headOption match {
      case Some(e) => (Some(e),fringe.tail,rest)
      case _ => rest match {
        case e::rest2 => (Some(e),fringe,rest2)
        case Nil => (None,fringe,rest)
      }
    }
  }
  private def getCompatNeighb(edge: Prim, done: Set[Prim], m: IOMapF): Option[Prim] = {
    val around: Set[Prim] = (edge.ins.toSet++edge.outs.toSet).flatMap(m) - edge -- done
    //println(s"## around: ${around.mkString(",")}")
    //println(s"## compat: ${around.filter(edgeCompat(edge,_)).mkString(",")}")
    around.find(edgeCompat(edge,_))
  }
  private def edgeCompat(e1: Prim, e2: Prim): Boolean = {
     //print(s"## compat? $e1 vs. $e2 -- ")
    (e1,e2) match {
      // two compatible nodes
      case (Prim(CPrim("node", _, _, ex1), i1, o1, _)
      , Prim(CPrim("node", _, _, ex2), i2, o2, _)) =>
        val xordupl = !(((ex1 contains "xor") && (ex2 contains "dupl")) ||
          ((ex2 contains "xor") && (ex1 contains "dupl")))
        val duplvdupl = !(((ex1 contains "vdupl") && (ex2 contains "dupl")) ||
          ((ex2 contains "vdupl") && (ex1 contains "dupl")))
        val mergervmerger = !(((ex1 contains "vmrg") && (ex2 contains "mrg")) ||
          ((ex2 contains "vmrg") && (ex1 contains "mrg")))
        val xorvdupl = !(((ex1 contains "vdupl") && (ex2 contains "xor")) ||
          ((ex2 contains "vdupl") && (ex1 contains "xor")))
        lazy val onelink =
          if ((i1.toSet intersect o2.toSet).nonEmpty)
            i1.size <= 1 || o2.size <= 1
          else i2.size <= 1 || o1.size <= 1
        // println(s"$xordupl /\\ $onelink")
        //println(s"one link (ok)? $onelink")
        xordupl && onelink && xorvdupl && duplvdupl && mergervmerger
      // one is a simple node (1 in, 1 out)
      case (Prim(CPrim("node",CoreInterface(1),CoreInterface(1),_),_,_,_),_) =>
        true
      case (_,Prim(CPrim("node",CoreInterface(1),CoreInterface(1),_),_,_,_)) =>
        true
      // one is a port/sync/id
      case (Prim(CPrim(name1, _, _, _), _, _, _)
           ,Prim(CPrim(name2, _, _, _), _, _, _)) =>
        //println(Set("port","id","sync").intersect(Set(name1,name2)).nonEmpty)
        Set("port","id","sync").intersect(Set(name1,name2)).nonEmpty
      case _ =>
        println("NO")
        false
    }}

//  private def joinAll(e:Prim, es: Set[Prim], ms: Mirrors): Prim = {
//    es.fold[Prim](e)((e1, e2) => {
//      val res = joinPrimtitives(e1, e2, ms)
//      println(s"joining $e1+$e2 = $res")
//      res
//    })
//  }

  @scala.annotation.tailrec
  private def joinPrimtitives(e1:Prim, e2:Prim, ms: Mirrors): Prim = (e1,e2) match {
    // two compatible nodes
    case (Prim(CPrim("node",CoreInterface(_),CoreInterface(_),ex1),ins1,outs1,ps1)
         ,Prim(CPrim("node",CoreInterface(_),CoreInterface(_),ex2),ins2,outs2,ps2)) =>
      val newpars = if (ps2.length>ps1.length) ps2 else ps1
      val ins  = ins1.toSet ++ ins2.toSet -- outs1.toSet -- outs2.toSet
      val outs = outs1.toSet ++ outs2.toSet -- ins1.toSet -- ins2.toSet
      // println(s"$ins $outs $ins1 $outs1 $ins2 $outs2")
      //// shared port will reflect any of the inputs of the first node
      val shared = (ins1.toSet++outs2.toSet++ins2.toSet++outs1.toSet) -- (ins++outs) // should be 1
      for (s <- shared; p <- ins1.toSet if outs1 contains s)
        ms += s -> p
      for (s <- shared; p <- ins2.toSet if outs2 contains s)
        ms += s -> p
//      for (old <- (ins1.toSet intersect outs2.toSet) ++ (ins2.toSet intersect( outs1.toSet))) {}
      if (ins1.size+outs1.size+ins2.size+outs2.size - ins.size - outs.size <= 2)
        Prim(CPrim("node",CoreInterface(ins.size),CoreInterface(outs.size),ex1++ex2)
                  ,ins.toList,outs.toList,newpars)
      else throw new RuntimeException(s"Failed to combine nodes $e1 and $e2 - more than one shared end.")
    // 2nd is a port/sync/id/(simple-)node
    case (Prim(CPrim(name1,ip1,op1,ex1),is1,os1,ps1)
         ,Prim(CPrim(name2,CoreInterface(1),CoreInterface(1),ex2),List(i2),List(o2),_))
        if Set("sync","id","port","node").contains(name2) =>
      if (is1 contains o2) {
        ms += o2 -> i2
        Prim(CPrim(name1,ip1,op1,ex1++ex2),replace(is1,o2->i2),os1,ps1)
      }
      else {
        ms += i2 -> o2
        Prim(CPrim(name1,ip1,op1,ex1++ex2),is1,replace(os1,i2->o2),ps1)
      }
    // 1st is a port/sync/id/(simple-)node --> swap arguments
    case (Prim(CPrim(name,CoreInterface(1),CoreInterface(1),_),List(_),List(_),_),_)
         if Set("sync","id","port","node").contains(name) =>
      joinPrimtitives(e2,e1,ms)
    //
    case _ =>
//      println("e1:\n"+e1)
//      println("e2:\n"+e2)
      throw new RuntimeException(s"Failed to combine edges $e1 and $e2.")
  }

  private def replace(ints: List[Int], mp: (Int, Int)): List[Int] =
    ints.map(x => if (x==mp._1) mp._2 else x )

  private def toNode(e:Prim): Prim =
    if (e.prim.extra contains "box") e
    else e match {
      case Prim(CPrim("dupl", i, j, ex), ins1, outs1, ps1) =>
        Prim(CPrim("node", i, j, ex + "dupl"), ins1, outs1, ps1)
      case Prim(CPrim("vdupl", i, j, ex), ins1, outs1, ps1) =>
        Prim(CPrim("node", i, j, ex + "vdupl"), ins1, outs1, ps1)
      case Prim(CPrim("xor", i, j, ex), ins1, outs1, ps1) =>
        Prim(CPrim("node", i, j, ex + "xor"), ins1, outs1, ps1)
      case Prim(CPrim("merger", i, j, ex), ins1, outs1, ps1) =>
        Prim(CPrim("node", i, j, ex + "mrg"), ins1, outs1, ps1)
      case Prim(CPrim("vmerger", i, j, ex), ins1, outs1, ps1) =>
        Prim(CPrim("node", i, j, ex + "vmrg"), ins1, outs1, ps1)
      case _ => e
    }

  // Original approach, quite ad hoc.

//  /**
//    * Simplifies a graph, by:
//    *  - removing sync channels;
//    *  - converting some mergers and duplicators into (reo) nodes.
//    * @param g graph to be reduced
//    * @return reduced graph
//    */
//  @deprecated
//  def simplifyGraphOld(g: Network): Network = {
//    // remove useless Sync channels
//    val (es,remap) = dropSyncs(g)
//    val g2 = applyRemap(Network(es,g.ins,g.outs),remap)
//    // remove replicators and mergers when possible
//    val (inmap,outmap) = collectInsOuts(g2)
//    val (es2,remap2) = dropDuplMerg(g2,inmap,outmap)
//    val g3 = applyRemap(Network(es2,g2.ins,g2.outs),remap2)
//    val g4 = applyRemap(g3,remap2)
//    val g5 = applyRemap(g4,remap2)
//    // add syncs to border mergers and replicators
//    val g6 = fixLoops(g5)
////    val g7 = addBorderSyncs(g6)
//    g6
//  }
//
//  /**
//    * Replaces ports from one identifier to another
//    * @param graph graph to be simplified
//    * @param m remap to be applied
//    * @return a modified graph
//    */
//  private def applyRemap(graph:Network, m:Map[Int,Int]): Network = {
//    var res = graph
//    var map = m
//    while (map.nonEmpty) {
//      val (f,t) = map.head
//      res = subst(res,Map(f->t))
//      map = map.tail.map{
//        case (a,`f`) => (a,t)
//        case (`f`,b) => (t,b)
//        case x => x
//      }
//    }
//    res
//  }

  /**
    * maps ports to their edges in a graph
    */
  def collectInsOuts(graph: Network): (Map[Int,Set[Prim]],Map[Int,Set[Prim]]) =
    collectInsOuts(graph.prims)
  private def collectInsOuts(edges: List[Prim]): (Map[Int,Set[Prim]],Map[Int,Set[Prim]]) = edges match {
    case Nil => (Map(),Map())
    case (e@Prim(_,eins,eouts,_)) :: rest =>
      var (ins,outs) = collectInsOuts(rest)
      for (i <- eins)
        ins = ins.updated(i,ins.getOrElse(i,Set()) + e)
      for (o <- eouts)
        outs = outs.updated(o, outs.getOrElse(o,Set()) + e)
      (ins,outs)
  }


//  /**
//    * Removes sync channels in a graph, assuming 1 to 1 connections
//    * @param graph to be simplified
//    * @return the new list of edges, and a remap of ports that need to be replaced.
//    */
//  private def dropSyncs(graph: Network, keepBoundary:Boolean=true): (List[Prim],Map[Int,Int]) = graph.prims match {
//    case Nil => (graph.prims,Map())
//    case (e@Prim(CPrim("sync",_,_,_),List(in),List(out),_))::tl =>
//      val (es,m) = dropSyncs(Network(tl,graph.ins,graph.outs),keepBoundary)
//      if (!keepBoundary) {
//        if (graph.outs.contains(out)) (es,m+(in->out))
//        else (es,m+(in->out))
//      }
//      // do not remove if connected to some boundary
//      else if (graph.ins.contains(in) || graph.outs.contains(out))
//        (e::es,m)
//      else
//        (es,m+(out->in))
//    case e::tl =>
//      val (es,m) = dropSyncs(Network(tl,graph.ins,graph.outs),keepBoundary)
//      (e::es,m)
//  }
//
//  /**
//    * Removes duplicators and mergers, in a smart way
//    * @param g graph to be simplified
//    * @param inmap map from input ports to edges (by collectInsOuts)
//    * @param outmap map from output ports to edges (by collectInsOuts)
//    * @return simplified graph
//    */
//  private def dropDuplMerg(g:Network, inmap:Map[Int,Set[Prim]], outmap:Map[Int,Set[Prim]])
//      : (List[Prim],Map[Int,Int]) = g.prims match {
//    case Nil => (Nil,Map())
//    case (edge@Prim(CPrim("dupl",_,_,_),List(i1),eo@List(_,_),_))::tl =>
//      val (e,m) = dropDuplMerg(Network(tl,g.ins,g.outs),inmap,outmap)
//      var syncs: List[Prim] = List()
//      var binds: Map[Int,Int] = Map()
//      for (o <- eo) {
//        getEdge("merger", o, inmap) match {
//          case Some(oa) =>
//            syncs :::= mkGrSyncs(List(i1), List(oa))
//            binds += o -> i1
//          case None => if (allHaveEdges(List(o), inmap))
//            binds += o -> i1 else syncs :::= mkGrSyncs(List(i1), List(o))
//        }
//      }
//      (e:::syncs,m++binds)
//    case Prim(CPrim("merger",_,_,_),ei@List(i1,i2),List(o1),_)::tl
//        if allHaveEdges(ei,outmap)=>
//      val (e,m) = dropDuplMerg(Network(tl,g.ins,g.outs),inmap,outmap)
//      val m2 = if (getEdge("dupl",i1,outmap).isDefined) m  else m  + (i1 -> o1)
//      val m3 = if (getEdge("dupl",i2,outmap).isDefined) m2 else m2 + (i2 -> o1)
//      (e,m3)
//    case e::tl =>
//      val (es,m) = dropDuplMerg(Network(tl,g.ins,g.outs),inmap,outmap)
//      (e::es,m)
//  }

  private def fixLoops(graph: Network, mirrors: Mirrors): Network = {
    //println("$$ fixing loops "+graph)
    var (inmap, outmap) = collectInsOuts(graph)
    //println(s"(fixloops) io: ${inmap.mkString(",")}\n   ${outmap.mkString(",")}")
    //println(s"-- inmap: ${inmap.mkString(",")}\n-- outmap: ${outmap.mkString(",")}")
    //    var newPorts = Map[Port,Port]()
    var missing = graph.prims.toSet
    var done = List[Network.Prim]()
    while (missing.nonEmpty) {
      val next = missing.head
      missing = missing.tail
      var updated = false
      // collect neighbour nodes
      var nbNodes = Map[Network.Prim,Set[Port]]()
      // for all neighbours of "next"
      //println(s"next: $next :: ${missing.mkString(",")}")
      for ((port,nbs) <- getConnectedPrims(next,inmap,outmap);
           nb         <- nbs) {
        // get neighbour nodes
        if (next.prim.name == "node")
          for ((_,prims) <- getConnectedPrims(nb,inmap,outmap);
                prim <-prims if prim!=next && prim.prim.name=="node") {
            nbNodes += prim -> (nbNodes.getOrElse(prim,Set[Port]()) + port)
          }
        if (nb.prim.name=="node")
          nbNodes += nb -> (nbNodes.getOrElse(nb,Set[Port]())+port)
        print("c ")
        // case 1: self-loop
        if (!updated && nb==next) {
          val nextExpanded = makeSelfLoop(next, mirrors)
          //println(s"(1) adding prims: ${nextExpanded.mkString(",")}")
          missing ++= nextExpanded
          updated = true
        }
        // case 2: more than 1 shared ports (and not visited)
        else {
          val shared = (next.ins ++ next.outs).toSet intersect (nb.ins ++ nb.outs).toSet
          if (!updated && shared.size > 1 && missing.contains(nb)) {
            //println(s"expanding pair via ${shared.mkString(",")} : ${next} -- ${nb} ")
            val (nextExpanded,io) = expandShared(next,shared,inmap,outmap,mirrors)
            inmap  = io._1
            outmap = io._2
            //println(s"(2) adding prims: ${nextExpanded.mkString(",")}")
            //println(s"(2) io: ${inmap.mkString(",")}\n   ${outmap.mkString(",")}")
            missing ++= nextExpanded
            updated = true
          }
        }
      } // end for loop for neighbours

      // case 3: next is a node connected with more than 1 port to another node
      if (!updated) for ((node,ports) <- nbNodes) {
        //println(s"node $node")
        if (!updated && ports.size>1 && missing.contains(node)) {
          //println(s"expanding $ports")
          val (nextExpanded,io) = expandShared(next,ports,inmap,outmap,mirrors)
          inmap  = io._1
          outmap = io._2
          //println(s"(3) adding prims: ${nextExpanded.mkString(",")}")
          //println(s"(3) io: ${inmap.mkString(",")}\n   ${outmap.mkString(",")}")
          missing ++= nextExpanded
          updated = true
        }
      }
      //println(s"updated $updated")

      if (!updated) {
        //println(s"nothing more to expand with ${next}.")
        done ::= next
      }
    }// end while loop
    Network(done,graph.ins,graph.outs)
  }

  private def getConnectedPrims(p:Prim,inmap:IOMap,outmap:IOMap):List[(Port,Set[Prim])] =
    p.ins.map(port => (port,outmap.getOrElse(port,Set[Network.Prim]()))) ++
    p.outs.map(port => (port,inmap.getOrElse(port,Set[Network.Prim]())))

  private def makeSelfLoop(prim: Network.Prim, mirrors: Network.Mirrors): List[Network.Prim] = {
    println(s"making self loop $prim")
    var res = List[Network.Prim]()
    var ins = prim.ins
    var outs = prim.outs
    for (p <- prim.ins) {
      val e1 = mkSync(p,seed)
      val e2 = mkSync(seed+1,p)
      ins  = replace(ins, p-> seed)//ins.map( x=>if(x==p)seed else x)
      outs = replace(outs,p->(seed+1)) //outs.map(x=>if(x==p)seed+1 else x)
      mirrors += seed -> p
      mirrors += seed+1 -> p
      res = e1 :: e2 :: res
      seed += 2
    }
    Prim(prim.prim,ins,outs,prim.parents) :: res
  }

  private def expandShared(from: Network.Prim, ports: Set[Network.Port],
                           inmap:IOMap, outmap:IOMap,
                           mirrors: Network.Mirrors) : (List[Network.Prim],(IOMap,IOMap)) = {
    var syncs = List[Network.Prim]()
    var (ins,outs)        = (from.ins,from.outs) // new input and output ports
    var (inmap2,outmap2)  = (inmap,outmap)       // updated in/out-maps
    for (p <- ports) {
      if (from.ins contains p) {
        val syn = mkSync(p,seed)
        inmap2   = inmap2 + (p->Set(syn)) + (seed->Set(from))
        outmap2 += (seed->Set(syn))
        ins = replace(ins,p->seed)//ins.map(x => if(x==p)seed else x)
        mirrors += seed -> p
        syncs ::= syn
      }
      else {
        val syn = mkSync(seed,p)
        outmap2   = outmap2 + (p->Set(syn)) + (seed->Set(from))
        inmap2 += seed->Set(syn)
        outs = replace(outs,p->seed) //outs.map(x => if(x==p)seed else x)
        mirrors += seed -> p
        syncs ::= syn
      }
      seed += 1
    }
    val newFrom = Prim(from.prim,ins,outs,from.parents)
    inmap2  =  inmap2.mapValues(set => if (set contains from) set-from+newFrom else set)
    outmap2 = outmap2.mapValues(set => if (set contains from) set-from+newFrom else set)
    (newFrom :: syncs, (inmap2,outmap2))
  }

//  private def fixLoops(graph: Network, mirrors: Mirrors): Network = {
//    //println("$$ fixing loops "+graph)
//    val (inmap,outmap) = collectInsOuts(graph)
//    //println(s"-- inmap: ${inmap.mkString(",")}\n-- outmap: ${outmap.mkString(",")}")
////    var newPorts = Map[Port,Port]()
//    var res = graph
//    for ((p,es) <- inmap; e <- es) {
//      // found a direct loop
//      if (e.outs contains p) {
//        val e1 = mkSync(p,seed)
//        val e2 = Prim(e.prim,e.ins.map(x=>if(x==p)seed else p),e.outs.map(x=>if(x==p)seed+1 else p),e.parents)
//        val e3 = mkSync(seed+1,p)
//        mirrors += seed -> p
//        mirrors += seed+1 -> p
//        res = Network(e1::e2::e3:: res.prims.diff(List(e)),res.ins,res.outs)
//        seed += 2
//      }
//      // found a 2-way loop
//      for ( pj <- e.outs;
//            ej <- inmap.getOrElse(pj,Set())
//            if pj!=p && ej.outs.contains(p)) {
//          val e1 = Prim(e.prim,e.ins,e.outs.map(x=>if(x==pj)seed else p),e.parents)
//          val e2 = mkSync(seed,pj)
//          mirrors += seed -> pj
//          res = Network(e1::e2:: res.prims.diff(List(e)),res.ins,res.outs)
//          seed += 1
//        }
//      // found 2 edges going to the same place
//      for ( pj <- e.outs;
//            ej <- outmap.getOrElse(pj,Set())
//            if (ej!=e || res.prims.count(_==e)>1) && ej.ins.contains(p)) {
//        for (_ <- 1 to res.prims.count(_==e)) {
//          val e1 = Prim(e.prim, e.ins, e.outs.map(x => if (x == pj) seed else p), e.parents)
//          val e2 = mkSync(seed, pj)
//          mirrors += seed -> pj
//          res = Network(e1 :: e2 :: res.prims.diff(List(e)), res.ins, res.outs)
//          seed += 1
//        }
//      }
//    }
//    //println("fixed loops "+res)
//    res
//  }

  /**
    * Add extra Sync channels when having mergers or dupls in the borders
    */
  private def addBorderSyncs(graph: Network, mirrors: Mirrors): Network = {
    val (inmap,outmap) = collectInsOuts(graph)
//    var newPorts2 = newPorts
    var res = graph
    for (in <- graph.ins) {
      if ((inmap contains in) &&
          (inmap(in).size > 1 || inmap(in).exists(_.prim.name=="dupl") || inmap(in).exists(_.prim.name=="vdupl")
            || inmap(in).exists(_.prim.name=="node"))) {
        val e = mkSync(seed,in)
        mirrors += seed -> in
        res = Network(e :: res.prims, seed :: res.ins.filterNot(_==in), res.outs)
        seed += 1
      }
    }
    for (out <- graph.outs) {
      if ((outmap contains out) &&
          (outmap(out).size > 1 || outmap(out).exists(_.prim.name=="merger")
            || outmap(out).exists(_.prim.name=="node"))) {
        val e = mkSync(out,seed)
        mirrors += seed -> out
        res = Network(e :: res.prims, res.ins, seed :: res.outs.filterNot(_==out))
        seed += 1
      }
    }
    res
  }

//  private def allHaveEdges(ports: List[Int], m: Map[Int, Set[Network.Prim]]): Boolean =
//    ports.forall(p => (m contains p) && m(p).nonEmpty)
//  private def getEdge(name:String,port:Int,m:Map[Int, Set[Network.Prim]]): Option[Int] = {
//    for (set <- m.get(port);
//         e <- set.find(_.prim.name == name);
//         h <- e.outs.headOption)
//      yield h
//  }

}
