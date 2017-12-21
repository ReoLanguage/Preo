package preo.modelling

import org.junit.Test
import org.junit.Assert._
import preo.DSL.parse
import preo.Main.readFromFile
import preo.ast.CPrim
import preo.backend.ReoGraph
import preo.backend.ReoGraph.Edge
import preo.frontend.Eval

class TestProgram {
  //by inserting the cases in out language I wouldn't know
  @Test
  def testWithReoGraph(): Unit = {
    //graph creation
    val reoGraph1 = ReoGraph.toGraph(Eval.reduce(parse("Tr_(1)(fifo & fifo & fifo)")));
    val reoGraph2 = ReoGraph.toGraph(Eval.reduce(parse("reader")));
    val reoGraph3 = ReoGraph.toGraph(Eval.reduce(parse("writer")));
    val reoGraph4 = ReoGraph.toGraph(Eval.reduce(parse("lossy")));
    val reoGraph5 = ReoGraph.toGraph(Eval.reduce(parse("drain")));
    val reoGraph6 = ReoGraph.toGraph(Eval.reduce(parse("fifo * fifo")));
    val reoGraph7 = ReoGraph.toGraph(Eval.reduce(parse("dupl & (fifo * fifofull)")));
    val reoGraph8 = ReoGraph.toGraph(Eval.reduce(parse("(fifo * fifofull) * merger")));;
    val reoGraph9 = ReoGraph.toGraph(Eval.reduce(parse("writer & fifo & reader")));
    val reoGraph10 = ReoGraph.toGraph(Eval.reduce(parse("(writer & fifo & reader) * (writer & fifo & reader)")));
    val reoGraph11 = ReoGraph.toGraph(Eval.reduce(parse("zip =\n  \\n.Tr_((2*n)*(n-1))\n  (((((id^(n-x))*(sym(1,1)^x))*(id^(n-x)))^(x<--n))&\n   sym((2*n)*(n-1),2*n));\n\nunzip =\n  \\n.Tr_((2*n)*(n - 1))\n  (((((id^(x+1))*(sym(1,1)^((n-x)-1)))*(id^(x+1)))^(x<--n))&\n   sym((2*n)*(n-1),2*n));\n\nsequencer =\n  \\n.(((dupl^n)&unzip(n:I)) *\n    Tr_n(sym(n-1,1)&((fifofull&dupl)*((fifo & dupl)^(n-1)))&\n         unzip(n:I)))&\n    ((id^n)*((zip(n:I)) & (drain^n)));\n\n(writer^3) & sequencer 3 & (reader^3)")))
    val graphs = List(reoGraph1,reoGraph2,reoGraph3,reoGraph4,reoGraph5,reoGraph6,reoGraph7,reoGraph8,reoGraph9,reoGraph10, reoGraph11)

    //model getting
    val model1 = Mcrl2Program(reoGraph1)
    val model2 = Mcrl2Program(reoGraph2)
    val model3 = Mcrl2Program(reoGraph3)
    val model4 = Mcrl2Program(reoGraph4)
    val model5 = Mcrl2Program(reoGraph5)
    val model6 = Mcrl2Program(reoGraph6)
    val model7 = Mcrl2Program(reoGraph7)
    val model8 = Mcrl2Program(reoGraph8)
    val model9 = Mcrl2Program(reoGraph9)
    val model10 = Mcrl2Program(reoGraph10)
    val model11 = Mcrl2Program(reoGraph11)
    val models = List(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11)

    standardTests(graphs, models)


  }

  def getNodes(graph: ReoGraph): List[Int] = graph match{
    case ReoGraph(edges, _, _) => getNodesFromEdge(edges).distinct
    case _ => Nil
  }

  def getNodesFromEdge(edges: List[Edge]): List[Int] = edges match{
    case Edge(prim, ins, outs) :: rest => ins ++ outs ++ getNodesFromEdge(rest)
    case Nil => Nil
  }

  def standardTests(graphs: List[ReoGraph], programs: List[Mcrl2Program]): Unit = (graphs, programs) match {
    case (graph:: otherGraphs, program:: otherPrograms) => {
      val nodes = getNodes(graph)
      //we need to check that the number of nodes in the graph equals the ones on the program
      val mcrl2nodes = program.getNodes
      assertEquals(nodes.length, mcrl2nodes.length)
      //we need to check that the number of channels equals the number of edges minus writers and readers
      val channels = program.getChannels
      assertEquals(graph.edges.count{case Edge(CPrim(name, _, _, _), _, _) => name != "writer" && name != "reader"}, channels.length)
      //we need to check that only the in nodes and the out nodes can have null as action
      val ins = graph.ins
      val outs = graph.outs
      for(act <- program.getActions){
        assert(act.group > 0 && act.group < 7)
        assert(if(act.group == 6) act.number == -1 else true)
      }
      for(node <- mcrl2nodes){
        if(node.getBefore == Action(-1, 6)) assert(ins.contains(node.number))
        if(node.getAfter == Action(-1, 6)) assert(outs.contains(node.number))
      }
      //we need to check that the inits cover all the actions except the ins, outs and nulls
      var acts = program.getActions.toList.filter{case Action(n, g) => g > 3}
      val inits = program.getInits
      for(Mcrl2Init(_, a, _) <- inits){
        acts = acts.filter{case Action(n, _) => n != a.get_number}
      }
      assert(acts.isEmpty)
    }
  }
}
