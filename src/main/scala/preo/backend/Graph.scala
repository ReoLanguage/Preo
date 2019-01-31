package preo.backend

import preo.ast.CoreConnector

/**
  * Created by jose on 07/07/2017.
  */

/**
New simplified graph - to be visualied
  */
case class Graph(edges: List[ReoChannel], nodes:List[ReoNode])

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
case class ReoNode(id:Int, name:Option[String], nodeType:NodeType, extra:Set[Any])  {
  def similar(n:ReoNode): Boolean = n.id==id && n.name==name && n.nodeType==nodeType
}
//case class ReoNode(id:Int, name:Option[String], nodeType:NodeType)
sealed abstract class NodeType     { def dual:NodeType}
case object Source extends NodeType {def dual = Sink  }
case object Sink   extends NodeType {def dual = Source}
case object Mixed  extends NodeType {def dual = Mixed }


object Graph {

  def apply(c:CoreConnector,hideClosed: Boolean = true): Graph = {
//    val g = Automata.toAutomata(ReoGraph(c)).toGraph
    val g = ReoGraph(c,hideClosed)
    var seed:Int = (0::(g.ins ++ g.outs ++ g.edges.flatMap(x => x.ins ++ x.outs))).max

    val nodes  = scala.collection.mutable.Set[ReoNode]()
    var edges  = List[ReoChannel]()

    // update the "nodes" set
    /**
      * Update the set of nodes, preserving the extra parameters
      * @param id    of the node to be added or updated
      * @param name  of the node to be added or updated
      * @param nType of the node to be added or updated
      * @param extra of the node to be added or updated
      */
    def addNode(id:Int,name:Option[String],nType:NodeType,extra:Set[Any]): Unit = {
//      print(s"adding node $id $name $nType $extra")
      if (!nodes.exists(_ similar ReoNode(id,name,Mixed,Set()))) {
        nodes.find(_ similar ReoNode(id,name,nType.dual,Set())) match {
          case Some(rn) =>
            nodes -= rn
            nodes += ReoNode(id, name, Mixed, rn.extra++extra)
          case None =>
            nodes += ReoNode(id, name, nType, extra)
        }
      }
//      println(s" - nodes ${nodes.mkString(",")}")
    }
//    def addNode(id:Int,name:Option[String],nType:NodeType): Unit = {
//      if (!nodes.contains(ReoNode(id,name,Mixed))) {
//        if (nodes.contains(ReoNode(id, name, nType.dual))) {
//          nodes -= ReoNode(id, name, Mixed)
//          nodes += ReoNode(id, name, Mixed)
//        } else {
//          nodes += ReoNode(id, name, Mixed)
//        }
//      }
//    }

//    def replaceNodes(from: Set[Int], to: Int, edges: Set[ReoGraph.Edge]): Set[ReoGraph.Edge] = {
//      nodes.map(n => if (from contains n.id) ReoNode(to,n.name,n.nodeType,n.extra)
//                     else n)
//      edges.map(e => ReoGraph.Edge(e.prim,e.ins.map(i => if (from contains i) to else i),
//                                          e.outs.map(i => if (from contains i) to else i),e.parents))
//    }
//    var missing = g.edges.toSet
//    var toReplace = Set[ReoGraph.Edge]()
//
//    for (e<-g.edges) {
//      if (e.prim.name == "node") {
//        missing -= e
//        toReplace += e
//      }
//    }
//    while (toReplace.nonEmpty) {
//      val e = toReplace.head
//      toReplace = toReplace.tail
//      seed += 1
//      val srcs = e.ins.intersect(g.ins)
//      val snks = e.outs.intersect(g.outs)
//      //        val nType =
//      //          if (e.ins == srcs) Sink else if (e.outs.isEmpty) Source else Mixed
//      println(s"replacing ${e} by ${seed} type mixed")
//      missing   = replaceNodes(((e.ins.toSet ++ e.outs.toSet) -- g.ins.toSet) -- g.outs.toSet, seed, missing)
//      toReplace = replaceNodes(((e.ins.toSet ++ e.outs.toSet) -- g.ins.toSet) -- g.outs.toSet, seed, toReplace)
//      addNode(seed, None, Mixed, e.prim.extra)
//      for (e <- srcs) {
//        addNode(e, None, Source, Set())
//        edges ::= ReoChannel(e, seed, NoArrow, ArrowOut, "", Set())
//      }
//      for (e <- snks) {
//        addNode(e, None, Sink, Set())
//        edges ::= ReoChannel(seed, e, NoArrow, ArrowOut, "", Set())
//      }
//    }


    // For every ReoGraph edge, update the 'edges' and 'nodes'.
    for (e <- g.edges) {
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
        addNode(seed, Some(e.prim.name), typ, extra) // Main node: preserve box/component
        for (i <- e.ins) {
          edges ::= ReoChannel(i, seed, NoArrow, inArrow, "", Set()) // extras are in the main node
          addNode(i, None, Source, Set())
        }
        for (o <- e.outs) {
          edges ::= ReoChannel(seed, o, NoArrow, ArrowOut, "", Set()) // extras are in the main node
          addNode(o, None, Sink, Set())
        }
      } else {
      // Normal channel //
      ////////////////////
        // from every input to every output
        for (i <- e.ins; o <- e.outs) {
          edges ::= ReoChannel(i, o, NoArrow, ArrowOut, e.prim.name, extra)
          addNode(i, None, Source, Set())
          addNode(o, None, Sink, Set())
        }
        // between all outputs if no input end
        if (e.ins.isEmpty && e.outs.nonEmpty) {
          for (i <- e.outs; o <- e.outs; if e.outs.indexOf(i) < e.outs.indexOf(o)) {
            edges ::= ReoChannel(i, o, ArrowOut, ArrowOut, e.prim.name, extra)
            addNode(i, None, Sink, Set())
          }
          addNode(e.outs.last, None, Sink, Set()) // last one also needs to be added
        }
        // between all inputs if no output end
        if (e.outs.isEmpty && e.ins.nonEmpty) {
          for (i <- e.ins; o <- e.ins; if e.ins.indexOf(i) < e.ins.indexOf(o)) {
            edges ::= ReoChannel(i, o, ArrowIn, ArrowIn, e.prim.name, extra)
            addNode(i, None, Source, Set())
          }
          addNode(e.ins.last, None, Source, Set()) // last one also needs to be added
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

    Graph(edges,nodes.toList)
  }

}
