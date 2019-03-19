package preo.backend

import preo.ast.{CPrim, CoreConnector}
import preo.backend.Network.{Port, Prim}

import scala.collection.mutable

/**
  * Created by jose on 07/07/2017.
  */

/**
New simplified graph - to be visualied
  */
case class Circuit(edges: List[ReoChannel], nodes:List[ReoNode])

/**
  * Channels are links from a single source end to a single sink end
  * @param src source
  * @param trg sink
  * @param srcType which arrow to draw in the source end
  * @param trgType which arrow to draw in the sink end
  * @param name
  * @param extra extra properties of the channel
  */
case class ReoChannel(src:Int, trg:Int, srcType:EndType, trgType:EndType, name:String, extra:Set[Any]) {
  def inputs:Set[Int] = {
    var ins:Set[Int] = Set()
    srcType match {
      case ArrowIn => ins = ins ++ Set(src)
      case NoArrow => ins = ins ++ Set(src)
      case _ => ins
    }
    trgType match {
      case ArrowIn => ins = ins ++ Set(trg)
      case NoArrow => ins = ins ++ Set(trg)
      case _ => ins
    }
    ins
  }

  def outputs:Set[Int] = {
    var outs:Set[Int] = Set()
    srcType match {
      case ArrowOut => outs = outs ++ Set(src)
      case _ => outs
    }
    trgType match {
      case ArrowOut => outs = outs ++ Set(trg)
      case _ => outs
    }
    outs
  }
}
sealed abstract class EndType
case object ArrowIn  extends EndType
case object ArrowOut extends EndType
case object NoArrow  extends EndType


/**
  * Nodes are vertices in the graph: mixed/source/sink nodes, components, or boxes
  * @param id identifier (int)
  * @param name
  * @param nodeType Source, sink, or mixed
  * @param extra if it is a component or a box, and other properties
  */
case class ReoNode(id:Int, name:Option[String], nodeType:NodeType, extra:Set[Any],ports:Set[Port])  {
  def similar(n:ReoNode): Boolean = n.id==id && n.name==name // && n.nodeType==nodeType
}
//case class ReoNode(id:Int, name:Option[String], nodeType:NodeType)
sealed abstract class NodeType     { def dual:NodeType}
case object Source extends NodeType {def dual = Sink  }
case object Sink   extends NodeType {def dual = Source}
case object Mixed  extends NodeType {def dual = Mixed }


object Circuit {


  def apply(c:CoreConnector,hideClosed: Boolean = true): Circuit = {
    //    val g = Automata.toAutomata(ReoGraph(c)).toGraph
    val (n1,_) = Network.toNetwWithRedundancy(c, hideClosed)
    apply(n1)
  }

//  def connToNodeGraph(c:CoreConnector,hideClosed: Boolean = true): Graph = {
//    //    val g = Automata.toAutomata(ReoGraph(c)).toGraph
//    apply(Network.toGraphOneToOneSimple(c, hideClosed))
//  }

  def connToVirtuosoGraph(c:CoreConnector,hideClose:Boolean = true): Circuit = {
    toVirtuoso(Network(c, hideClose))
  }

  def apply(g:Network): Circuit = {
    var seed:Int = (0::(g.ins ++ g.outs ++ g.prims.flatMap(x => x.ins ++ x.outs))).max

    val nodes  = scala.collection.mutable.Set[ReoNode]()
    var edges  = Set[ReoChannel]()
    val toRemap = mutable.Map[Int,Int]()


    // update the "nodes" set
    /** Add a new node to the set of known nodes, before remapping */
    def addNode(id:Int,name:Option[String],nType:NodeType,extra:Set[Any],ports:Set[Port]): Unit =
      nodes += ReoNode(id,name,nType,extra,ports)
    /** Update the set of nodes, applying the current remap */
    def reAddNode(id1:Int,name:Option[String],nType:NodeType,extra:Set[Any],ports:Set[Port]): Unit = {
      val id = toRemap.getOrElse(id1,id1)
      def update(n1: ReoNode, n2: ReoNode): Unit = {
        nodes -= n1
        nodes += n2
      }
      def joinType(nt1: NodeType, nt2: NodeType): NodeType = (nt1,nt2) match {
        case (Mixed,_) => Mixed
        case (_,Mixed) => Mixed
        case (Sink,Source) => Mixed
        case (Source,Sink) => Mixed
        case _ => nt1
      }
     //print(s"adding node $id $name $nType $extra, knowing ${nodes.mkString(",")}")
      nodes.find(_ similar ReoNode(id,name,null,null,null)) match {
        case Some(rn) => update(rn,ReoNode(id,name,joinType(rn.nodeType,nType),rn.extra++extra,rn.ports++ports))
        case _ => nodes += ReoNode(id,name,nType,extra,ports)
      }
      //println(s" - nodes ${nodes.mkString(",")}")
    }

    /** Add a new edge to the set of known edges, before remapping */
    def addReoChannel(i: Int, o: Int, a1: EndType, a2: EndType, str: String, extra: Set[Any]): Unit =
      edges += ReoChannel(i,o,a1,a2,str,extra)
    /** Update the set of edges, applying the current remap */
    def reAddReoChannel(i1: Int, o1: Int, a1: EndType, a2: EndType, str: String, extra: Set[Any]): Unit = {
      val i = toRemap.getOrElse(i1,i1)
      val o = toRemap.getOrElse(o1,o1)
      //println(s"adding ReoChannel $i $o $str $extra, knowing ${edges.mkString(",")}")
      edges += ReoChannel(i,o,a1,a2,str,extra)
    }

    def addRemap(pair: (Int, Int),srcs:List[Int]): Unit = {
      //print(s"adding remap ${pair._1}->${pair._2}\n")
      toRemap.get(pair._1) match {
        case Some(newPort) => // orig already existed
          if (srcs contains pair._1)
               edges += ReoChannel(newPort,pair._2,NoArrow,ArrowOut,"",Set())
               //; println(s"sync $newPort>${pair._2}")}
          else edges += ReoChannel(pair._2,newPort,NoArrow,ArrowOut,"",Set())
              //; println(s"sync ${pair._2}>$newPort")}
        case _ => toRemap.get(pair._2) match {
          case Some(newPort) => // replacement already existed
            toRemap -= pair._2
            toRemap += pair
            //print(s"replaced ${pair._2}>$newPort by new remap\n")
            if (srcs contains pair._2)
                 edges += ReoChannel(newPort,pair._2,NoArrow,ArrowOut,"",Set())
                 //; println(s"sync $newPort>${pair._2}")}
            else edges += ReoChannel(pair._2,newPort,NoArrow,ArrowOut,"",Set())
                 //; println(s"sync ${pair._2}>$newPort")}
          case _ =>
            //println("just adding the pair")
            toRemap += pair
        }
      }
    }

    // For every ReoGraph edge, update the 'edges' and 'nodes'.
    for (e <- g.prims) {
      val extra = e.prim.extra

      // start by looking at nodes (some ports may have to be remapped)
      if (e.prim.name == "node") {
        e.outs:::e.ins match {
          case somePort::rest =>
            addNode(somePort,None,Sink,e.prim.extra,(e.ins++e.outs).toSet)
            for (p <- rest) addRemap(p -> somePort, e.ins)
            toRemap += somePort->somePort // adding self loop in the end on purpose
          case _ =>
        }
      } else

      // continue by checking the extra field
      // found a box (closed container) or a component //
      ///////////////////////////////////////////////////
      if ((extra contains "component") || (extra contains "box")) {
        val isComp = extra contains "component"
        val typ = if (isComp) {
          if (e.ins.isEmpty) Source else Sink
        } else Mixed
        val inArrow = if (isComp) ArrowOut else NoArrow
        seed += 1
        addNode(seed, Some(e.prim.name), typ, extra,(e.ins++e.outs).toSet) // Main node: preserve box/component
        for (i <- e.ins) {
          addReoChannel(i, seed, NoArrow, inArrow, "", Set()) // extras are in the main node
          addNode(i, None, Source, Set(),Set(i))
        }
        for (o <- e.outs) {
          addReoChannel(seed, o, NoArrow, ArrowOut, "", Set()) // extras are in the main node
          addNode(o, None, Sink, Set(),Set(o))
        }
      } else {
      // Normal channel //
      ////////////////////
        // from every input to every output
        for (i <- e.ins; o <- e.outs) {
          addReoChannel(i, o, NoArrow, ArrowOut, e.prim.name, extra)
          addNode(i, None, Source, Set(),Set(i))
          addNode(o, None, Sink, Set(),Set(o))
        }
        // between all outputs if no input end
        if (e.ins.isEmpty && e.outs.nonEmpty) {
          for (i <- e.outs; o <- e.outs; if e.outs.indexOf(i) < e.outs.indexOf(o)) {
            addReoChannel(i, o, ArrowOut, ArrowOut, e.prim.name, extra)
            addNode(i, None, Sink, Set(),Set(i))
          }
          addNode(e.outs.last, None, Sink, Set(),Set(e.outs.last)) // last one also needs to be added
        }
        // between all inputs if no output end
        if (e.outs.isEmpty && e.ins.nonEmpty) {
          for (i <- e.ins; o <- e.ins; if e.ins.indexOf(i) < e.ins.indexOf(o)) {
            addReoChannel(i, o, ArrowIn, ArrowIn, e.prim.name, extra)
            addNode(i, None, Source, Set(),Set(i))
          }
          addNode(e.ins.last, None, Source, Set(),Set(e.ins.last)) // last one also needs to be added
        }
      }
//      // Create a writer component if exactly one output and no intput
//      if (e.ins.isEmpty && e.outs.size == 1) {
//        seed += 1
//        edges ::= ReoChannel(seed,e.outs.head,NoArrow,ArrowOut,"",extra)
//        addNode(seed,Some(e.prim.name),Source,None)
//        addNode(e.outs.head,None,Sink,None)
////        bounds += (e.prim.name + "_" + e.outs.head)
////        nodes += "n"++e.outs.head.toString
//      }
//      // Create a reader component if exactly one input and no output
//      if (e.outs.isEmpty && e.ins.size==1) {
//        seed += 1
//        edges ::= ReoChannel(e.ins.head,seed,NoArrow,ArrowOut,"",None) //  s"n${e.ins.head}, ${e.prim.name + "_" + e.ins.head}"
//        addNode(seed,Some(e.prim.name),Sink,extra)
//        addNode(e.ins.head,None,Source,None)
////        bounds += (e.prim.name + "_" + e.ins.head)
//      }
    }

    // re-adding, to remap some missing nodes
    val nodesOld = nodes.toList
    val edgesOld = edges
    nodes.clear()
    edges = Set()
    for(n<-nodesOld) reAddNode(n.id,n.name,n.nodeType,n.extra,n.ports)
    for(e<-edgesOld) reAddReoChannel(e.src,e.trg,e.srcType,e.trgType,e.name,e.extra)

    //println(s"edges: ${edges.mkString(",")}\nnodes: ${nodes.mkString(",")}\nremap: ${toRemap.mkString(",")}")
    Circuit(edges.toList,nodes.toList)
  }


  private val hubs = Set("semaphore","fifo","resource","dataEvent","blackboard","event","port",
    "eventFull","dataEventFull","fifoFull","blackboardFull")
  private def isAHub(n:String): Boolean = {
//    val hubs = Set("semaphore","fifo","resource","dataEvent","blackboard","event","port")
    hubs.contains(n)
  }

  /** checks if any of the given name is from a hub. */
  private def isAHub(set: Set[Any]):Boolean = {
    var res = false
    for (h <- hubs ) res = res || set.contains(h)
    res
  }


  // todo: fix drains
  private def toVirtuoso(g:Network): Circuit = {
    var seed:Int = (0::(g.ins ++ g.outs ++ g.prims.flatMap(x => x.ins ++ x.outs))).max

    val nodes  = scala.collection.mutable.Set[ReoNode]()
    var edges  = List[ReoChannel]()

    var remap: Map[Int,Set[Int]] = Map()
    var nodeEdges: Map[Int,(List[Int],List[Int])] = Map()
//    def mkPort(i:Int): Int = remap.getOrElse(i,i)
    // update the "nodes" set
    /**
      * Update the set of nodes, preserving the extra parameters
      * @param id    of the node to be added or updated
      * @param name  of the node to be added or updated
      * @param nType of the node to be added or updated
      * @param extra of the node to be added or updated
      */
    def addNode(id:Int,name:Option[String],nType:NodeType,extra:Set[Any],ports:Set[Port]): Unit = {
      //      print(s"adding node $id $name $nType $extra")
      if (!nodes.exists(_ similar ReoNode(id,name,Mixed,Set(),ports))) {
        nodes.find(_ similar ReoNode(id,name,nType.dual,Set(),ports)) match {
          case Some(rn) =>
            nodes -= rn
            nodes += ReoNode(id, name, Mixed, rn.extra++extra,rn.ports++ports)
          case None =>
            nodes += ReoNode(id, name, nType, extra,ports)
        }
      }
      //      println(s" - nodes ${nodes.mkString(",")}")
    }


    // For every ReoGraph edge, update the 'edges' and 'nodes'.
    for (e <- g.prims) {
      val extra = e.prim.extra
      // start by checking the extra field
      // found a box (closed container) or a component //
      ///////////////////////////////////////////////////
      if ((extra contains "component") || (extra contains "box")) {
        val isComp = extra contains "component"
        val typ = if (isComp) {
          if (e.ins.isEmpty) Source else Sink
        } else Mixed
        val inArrow = if (isComp) ArrowOut else NoArrow
        seed += 1
        addNode(seed, Some(e.prim.name), typ, extra,(e.ins++e.outs).toSet) // Main node: preserve box/component
        for (i <- e.ins) {
          edges ::= ReoChannel(i, seed, NoArrow, inArrow, "", Set()) // extras are in the main node
          addNode(i, None, Source, Set(),Set(i))
        }
        for (o <- e.outs) {
          edges ::= ReoChannel(seed, o, NoArrow, ArrowOut, "", Set()) // extras are in the main node
          addNode(o, None, Sink, Set(),Set(o))
        }
      } else
      // check if it is dupl or xor or merger //
      ///////////////////////
        if ((extra contains "dupl") || (extra contains "xor") || (extra contains "mrg")) {
          seed += 1
          addNode(seed,Some(e.prim.name), Mixed,extra,(e.ins++e.outs).toSet)
          remap++=  e.outs.map(o => o -> (remap.getOrElse(o,Set()) ++ Set(seed))) ++ e.ins.map(i => i -> (remap.getOrElse(i,Set()) ++ Set(seed)))
          nodeEdges += seed -> ((e.ins, e.outs))
      } else
        if (isAHub(e.prim.name)) {
          seed +=1
          addNode(seed,Some(e.prim.name), Mixed,Set(e.prim.name),(e.ins++e.outs).toSet)
          remap++=  e.outs.map(o => o -> (remap.getOrElse(o,Set()) ++ Set(seed))) ++ e.ins.map(i => i -> (remap.getOrElse(i,Set()) ++ Set(seed)))
          nodeEdges += seed -> ((e.ins, e.outs))
        } else{
        // Normal channel //
        ////////////////////
        //  from every input to every output
        for (i <- e.ins; o <- e.outs) {
          edges ::= ReoChannel(i,o, NoArrow, ArrowOut, e.prim.name, extra)
          addNode(i, None, Source, Set(),Set(i))
          addNode(o, None, Sink, Set(),Set(o))
        }
        // between all outputs if no input end
        if (e.ins.isEmpty && e.outs.nonEmpty) {
          for (i <- e.outs; o <- e.outs; if e.outs.indexOf(i) < e.outs.indexOf(o)) {
            edges ::= ReoChannel(i, o, ArrowOut, ArrowOut, e.prim.name, extra)
            addNode(i, None, Sink, Set(),Set(i))
          }
          addNode(e.outs.last, None, Sink, Set(),Set(e.outs.last)) // last one also needs to be added
        }
        // between all inputs if no output end
        if (e.outs.isEmpty && e.ins.nonEmpty) {
          for (i <- e.ins; o <- e.ins; if e.ins.indexOf(i) < e.ins.indexOf(o)) {
            edges ::= ReoChannel(i, o, ArrowIn, ArrowIn, e.prim.name, extra)
            addNode(i, None, Source, Set(),Set(i))
          }
          addNode(e.ins.last, None, Source, Set(),Set(e.ins.last)) // last one also needs to be added
        }
      }
    }

    var g1 = changeDrains(remapGraph(Circuit(edges,nodes.toList),remap,nodeEdges,seed))
    addBorderSyncs(g1,remap,nodeEdges)
  }

  private def remapGraph(g:Circuit, remap:Map[Int,Set[Int]], nodeEdges:Map[Int,(List[Int],List[Int])], currentSeed:Int):Circuit = {
    var seed = currentSeed
    var newEdges = Set[ReoChannel]()
    var newNodes = List[ReoNode]()
    var nodesToRm = Set[Int]()


    // remap edges to correspnding nodes
    newEdges = g.edges.map(e => e match {
      case ReoChannel(src, trg, sT, tT, name, extra) =>
        //&& g.nodes.contains(remap(src)) && !g.nodes.find(_.id == remap(src).head).get.extra.contains("drain")
        var ns = if (remap.contains(src)) {nodesToRm ++= Set(src); remap(src).head} else src
        var nt = if (remap.contains(trg)) {nodesToRm ++= Set(trg); remap(trg).head} else trg
        // remap.head should always be one in this case
        ReoChannel(ns, nt,sT,tT,name,extra)
    }).toSet

    // add extra links between dupl/xor/mrg nodes and hubs (don't merge two nodes into one) //todo box and commponent
    for (n <- g.nodes; if (n.extra.contains("xor") || n.extra.contains("dupl") || n.extra.contains("mrg") || isAHub(n.extra) || n.extra.contains("component") || n.extra.contains("box") )) {
      val (ins, outs) = nodeEdges.getOrElse(n.id,(List(),List()))
      for (i <- ins ) {
        if (remap.contains(i) && remap(i).size >1){ // is mixed
          var src = (remap(i) - n.id).head
          newEdges += ReoChannel(src,n.id, NoArrow,ArrowOut,"",Set())//n.extra)
        } //else //if (remap.contains(i) ) { //has one only
//          newEdges::= ReoChannel(i,n.id, NoArrow,ArrowOut,"",n.extra)
        //}
      }
      for (o <- outs) {
        if (remap.contains(o) && remap(o).size >1){ // is mixed
          var trg = (remap(o) - n.id).head
          newEdges += ReoChannel(n.id,trg, NoArrow,ArrowOut,"",Set())//n.extra)
        } //else if (remap.contains(o)) {
        //}
      }
      //}
    }
    // remove unused nodes
    newNodes = g.nodes.filterNot(n => nodesToRm.contains(n.id))

    //add border syncs to xor, dupl, mrg
//    for (n <- newNodes; if (n.extra.contains("xor") || n.extra.contains("dupl") || n.extra.contains("mrg") || isAHub(n.extra))) {
//      var currentIns = newEdges.filter(e => e.outputs.contains(n.id))
//      var currentOuts = newEdges.filter(e => e.inputs.contains(n.id))
//      if (n.extra.contains("xor") || n.extra.contains("dupl")) {
//
//      }
//      for (missing <- 1 to (nodeEdges(n.id)._1.size - currentIns.size)){
//        seed += 1
//        newEdges += ReoChannel(seed, n.id, NoArrow, ArrowOut, "", Set())
//        newNodes ::= ReoNode(seed, None, Source, Set())
//      }
//      for ( missing <- 1 to (nodeEdges(n.id)._2.size) - (currentOuts.size)){
//        seed += 1
//        newEdges += ReoChannel(n.id, seed, NoArrow, ArrowOut, "", Set())
//        newNodes ::= ReoNode(seed, None, Sink, Set())
//      }
//    }
    Circuit(newEdges.toList,newNodes)
  }

  private def changeDrains(g:Circuit):Circuit = {
    var drains = g.edges.filter(e => e.name == "drain")
    var newNodes = g.nodes.toSet
    var newEdges = g.edges.toSet //Set[ReoChannel]()
    //todo: fix writers when connected to drains

    var seed:Int = (Set(0) ++ g.nodes.map(_.id) ++ g.edges.flatMap(e => e.inputs ++ e.outputs)).max

    for (d <- drains) d match {
      case c@ReoChannel(src, trg, srcType, trgType, name, extra) if (src == trg) =>
        seed += 3
        newNodes += ReoNode(seed-2, None, Mixed, Set("hidden"),Set(src)) //hiden1
        newNodes += ReoNode(seed-1, None, Mixed, Set("hidden"),Set(src)) //hiden 2
        newNodes += ReoNode(seed, None, Mixed, Set("drain"),Set(src)) //hiden 2
        newEdges += ReoChannel(src,seed-2,NoArrow,NoArrow,"",Set()) // src to h1
        newEdges += ReoChannel(src,seed-1,NoArrow,NoArrow,"",Set()) // src to h2
        newEdges += ReoChannel(seed-2,seed,NoArrow,ArrowOut,"",Set()) // h1 to drain
        newEdges += ReoChannel(seed-1,seed,NoArrow,ArrowOut,"",Set()) // h2 to drain
        // remove drain channel
        newEdges -= c
      case c@ReoChannel(src, trg, srcType, trgType, name, extra) if (src != trg) =>
        seed += 1
        newEdges += ReoChannel(src,seed,NoArrow,ArrowOut,name,Set()) // src to drain
        newEdges += ReoChannel(trg,seed,NoArrow,ArrowOut,name,Set()) // trg to drain
        newNodes += ReoNode(seed, None, Mixed, Set("drain"),Set(src,trg)) //hiden 2
        // remove drain channel
        newEdges -= c
    }
    Circuit(newEdges.toList,newNodes.toList)
    //g
  }

  private def addBorderSyncs(g:Circuit, remap:Map[Int,Set[Int]], nodeEdges:Map[Int,(List[Int],List[Int])]):Circuit = {
    var newNodes = g.nodes
    var newEdges = g.edges.toSet
    var seed:Int = (Set(0) ++ g.nodes.map(_.id) ++ g.edges.flatMap(e => e.inputs ++ e.outputs)).max
     //add border syncs to xor, dupl, mrg
    for (n <- newNodes; if (n.extra.contains("xor") || n.extra.contains("dupl") || n.extra.contains("mrg") || isAHub(n.extra))) {
      var currentIns = newEdges.filter(e => e.outputs.contains(n.id))
      var currentOuts = newEdges.filter(e => e.inputs.contains(n.id))
//      if (n.extra.contains("xor") || n.extra.contains("dupl")) {
//
//      }
      for (missing <- 1 to (nodeEdges(n.id)._1.size - currentIns.size)){
        seed += 1
        newEdges += ReoChannel(seed, n.id, NoArrow, ArrowOut, "", Set())
        newNodes ::= ReoNode(seed, None, Source, Set(),Set(n.id)) // todo: find correct port
      }
      for ( missing <- 1 to (nodeEdges(n.id)._2.size) - (currentOuts.size)){
        seed += 1
        newEdges += ReoChannel(n.id, seed, NoArrow, ArrowOut, "", Set())
        newNodes ::= ReoNode(seed, None, Sink, Set(),Set(n.id)) // todo: find correct port
      }
    }
    Circuit(newEdges.toList,newNodes)
  }

}
