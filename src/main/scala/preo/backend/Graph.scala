package preo.backend

import preo.ast.CoreConnector

/**
  * Created by jose on 07/07/2017.
  */

/**
New simplified graph - to be visualied
  */
case class Graph(edges: List[ReoChannel], nodes:List[ReoNode])

case class ReoChannel(src:Int,trg:Int, srcType:EndType, trgType:EndType, name:String, style:Option[String])
sealed abstract class EndType
case object ArrowIn  extends EndType
case object ArrowOut extends EndType
case object NoArrow  extends EndType



case class ReoNode(id:Int, name:Option[String], nodeType:NodeType, style:Option[String])
sealed abstract class NodeType     { def dual:NodeType}
case object Source extends NodeType {def dual = Sink  }
case object Sink   extends NodeType {def dual = Source}
case object Mixed  extends NodeType {def dual = Mixed }


object Graph {

  def apply(c:CoreConnector): Graph = {
//    val g = Automata.toAutomata(ReoGraph(c)).toGraph
    val g = ReoGraph(c)
    var seed:Int = (0::(g.ins ++ g.outs ++ g.edges.flatMap(x => x.ins ++ x.outs))).max

    val nodes  = scala.collection.mutable.Set[ReoNode]()
    var edges  = List[ReoChannel]()
    // update the "nodes" set
    def addNode(id:Int,name:Option[String],nType:NodeType,style:Option[String]): Unit = {
      if (!(nodes contains ReoNode(id,name,Mixed,style))) {
        if (nodes contains ReoNode(id, name, nType.dual, None)) {
          nodes -= ReoNode(id, name, nType.dual, style)
          nodes += ReoNode(id, name, Mixed, style)
        }
        else
          nodes += ReoNode(id, name, nType, style)
      }
    }

    // For every ReoGraph edge, update the 'edges' and 'nodes'.
    for (e <- g.edges) {
      val style = e.prim.extra.map(_.toString)

      style match {
        // found a box (closed container) or a container
        case Some(s) =>
          val isComp = s contains "component"
          val typ = if (isComp) {
            if (e.ins.isEmpty) Source else Sink
          } else Mixed
          val inArrow = if (isComp) ArrowOut else NoArrow
          seed += 1
          addNode(seed, Some(e.prim.name), typ, style)
          for (i <- e.ins) {
            edges ::= ReoChannel(i, seed, NoArrow, inArrow, "", None)
            addNode(i, None, Source, None)
          }
          for (o <- e.outs) {
            edges ::= ReoChannel(seed, o, NoArrow, ArrowOut, "", None)
            addNode(o, None, Sink, None)
          }
        // Normal channel
        case _ =>
          // from every input to every output
          for (i <- e.ins; o <- e.outs) {
            edges ::= ReoChannel(i, o, NoArrow, ArrowOut, e.prim.name, style)
            addNode(i, None, Source, None)
            addNode(o, None, Sink, None)
          }
          // between all outputs if no input end
          if (e.ins.isEmpty && e.outs.size > 1) {
            for (i <- e.outs; o <- e.outs; if e.outs.indexOf(i) < e.outs.indexOf(o)) {
              edges ::= ReoChannel(i, o, ArrowOut, ArrowOut, e.prim.name, style)
              addNode(i, None, Sink, None)
            }
            addNode(e.outs.last, None, Sink, None) // last one also needs to be added
          }
          // between all inputs if no output end
          if (e.outs.isEmpty && e.ins.size > 1) {
            for (i <- e.ins; o <- e.ins; if e.ins.indexOf(i) < e.ins.indexOf(o)) {
              edges ::= ReoChannel(i, o, ArrowIn, ArrowIn, e.prim.name, style)
              addNode(i, None, Source, None)
            }
            addNode(e.ins.last, None, Source, None) // last one also needs to be added
          }
      }
//      // Create a writer component if exactly one output and no intput
//      if (e.ins.isEmpty && e.outs.size == 1) {
//        seed += 1
//        edges ::= ReoChannel(seed,e.outs.head,NoArrow,ArrowOut,"",style)
//        addNode(seed,Some(e.prim.name),Source,None)
//        addNode(e.outs.head,None,Sink,None)
////        bounds += (e.prim.name + "_" + e.outs.head)
////        nodes += "n"++e.outs.head.toString
//      }
//      // Create a reader component if exactly one input and no output
//      if (e.outs.isEmpty && e.ins.size==1) {
//        seed += 1
//        edges ::= ReoChannel(e.ins.head,seed,NoArrow,ArrowOut,"",None) //  s"n${e.ins.head}, ${e.prim.name + "_" + e.ins.head}"
//        addNode(seed,Some(e.prim.name),Sink,style)
//        addNode(e.ins.head,None,Source,None)
////        bounds += (e.prim.name + "_" + e.ins.head)
//      }
    }

    Graph(edges,nodes.toList)
  }

}
