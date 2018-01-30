//package preo.programling
//
//import org.junit.Test
//import org.junit.Assert._
//import preo.DSL.parse
//import preo.Main.readFromFile
//import preo.ast.CPrim
//import preo.backend.ReoGraph
//import preo.backend.ReoGraph.Edge
//import preo.frontend.Eval
//import preo.modelling._
//
//class TestProgram {
//  //by inserting the cases in out language I wouldn't know
//  @Test
//  def test1(): Unit = {
//    //graph creation
//    val reoGraph1 = ReoGraph.toGraph(Eval.reduce(parse("Tr(1)(fifo ; fifo ; fifo)")));
//
//    //program getting
//    val program1 = Mcrl2Program(reoGraph1)
//
//
//    val model1 = Mcrl2Model(Eval.reduce(parse("Tr(1)(fifo ; fifo ; fifo)")))
//
//
//
//    standardTests(reoGraph1, program1)
//
//    standardTests2(program1, model1)
//  }
//
//  @Test
//  def test2(): Unit = {
//
//    val reoGraph2 = ReoGraph.toGraph(Eval.reduce(parse("reader")));
//
//    val program2 = Mcrl2Program(reoGraph2)
//
//    val model2 = Mcrl2Model(Eval.reduce(parse("reader")))
//
//    standardTests(reoGraph2, program2)
//
//    standardTests2(program2, model2)
//
//  }
//
//  @Test
//  def test3(): Unit = {
//    val reoGraph3 = ReoGraph.toGraph(Eval.reduce(parse("writer")));
//
//    val program3 = Mcrl2Program(reoGraph3)
//
//    val model3 = Mcrl2Model(Eval.reduce(parse("writer")))
//
//    standardTests(reoGraph3, program3)
//
//    standardTests2(program3, model3)
//  }
//
//  @Test
//  def test4(): Unit = {
//    val reoGraph4 = ReoGraph.toGraph(Eval.reduce(parse("lossy")));
//
//    val program4 = Mcrl2Program(reoGraph4)
//
//    val model4 = Mcrl2Model(Eval.reduce(parse("lossy")))
//
//    standardTests(reoGraph4, program4)
//
//    standardTests2(program4, model4)
//  }
//
//  @Test
//  def test5(): Unit = {
//    val reoGraph5 = ReoGraph.toGraph(Eval.reduce(parse("drain")));
//
//    val program5 = Mcrl2Program(reoGraph5)
//
//    val model5 = Mcrl2Model(Eval.reduce(parse("drain")))
//
//    standardTests(reoGraph5, program5)
//
//    standardTests2(program5, model5)
//  }
//
//  @Test
//  def test6(): Unit = {
//    val reoGraph6 = ReoGraph.toGraph(Eval.reduce(parse("fifo * fifo")));
//
//    val program6 = Mcrl2Program(reoGraph6)
//
//    val model6 = Mcrl2Model(Eval.reduce(parse("fifo * fifo")))
//
//    standardTests(reoGraph6, program6)
//
//    standardTests2(program6, model6)
//
//  }
//
//  @Test
//  def test7(): Unit = {
//    val reoGraph7 = ReoGraph.toGraph(Eval.reduce(parse("dupl ; (fifo * fifofull)")));
//
//    val program7 = Mcrl2Program(reoGraph7)
//
//    val model7 = Mcrl2Model(Eval.reduce(parse("dupl ; (fifo * fifofull)")))
//
//    standardTests(reoGraph7, program7)
//
//    standardTests2(program7, model7)
//  }
//
//  @Test
//  def test8(): Unit = {
//    val reoGraph8 = ReoGraph.toGraph(Eval.reduce(parse("(fifo * fifofull) ; merger")));
//
//    val program8 = Mcrl2Program(reoGraph8)
//
//    val model8 = Mcrl2Model(Eval.reduce(parse("(fifo * fifofull) ; merger")))
//
//    standardTests(reoGraph8, program8)
//
//    standardTests2(program8, model8)
//  }
//
//  @Test
//  def test9(): Unit = {
//    val reoGraph9 = ReoGraph.toGraph(Eval.reduce(parse("writer ; fifo ; reader")));
//
//    val program9 = Mcrl2Program(reoGraph9)
//
//    val model9 = Mcrl2Model(Eval.reduce(parse("writer ; fifo ; reader")))
//
//    standardTests(reoGraph9, program9)
//
//    standardTests2(program9, model9)
//  }
//
//  @Test
//  def test10(): Unit = {
//    val reoGraph10 = ReoGraph.toGraph(Eval.reduce(parse("(writer ; fifo ; reader) * (writer ; fifo ; reader)")));
//
//    val program10 = Mcrl2Program(reoGraph10)
//
//    val model10 = Mcrl2Model(Eval.reduce(parse("(writer ; fifo ; reader) * (writer ; fifo ; reader)")))
//
//    standardTests(reoGraph10, program10)
//
//    standardTests2(program10, model10)
//  }
//
//  @Test
//  def test11(): Unit = {
//    val reoGraph11 = ReoGraph.toGraph(Eval.reduce(parse("writer^3 ; sequencer 3 ; reader^3 { zip =   \\n.Tr((2*n)*(n-1))  ((id^(n-x)*sym(1,1)^x*id^(n-x))^x<--n;   sym((2*n)*(n-1),2*n)),unzip =  \\n.Tr((2*n)*(n-1))  (((id^(x+1)*sym(1,1)^((n-x)-1)*id^(x+1))^x<--n);   sym((2*n)*(n-1),2*n)), sequencer =  \\n.((dupl^n;unzip(n:I)) *    Tr(n)(sym(n-1,1);((fifofull;dupl)*((fifo ; dupl)^(n-1)));         unzip(n:I))) ;    (id^n*(zip(n:I) ; drain^n))}")))
//
//    val program11 = Mcrl2Program(reoGraph11)
//
//    val model11 = Mcrl2Model(Eval.reduce(parse("writer^3 ; sequencer 3 ; reader^3 { zip =   \\n.Tr((2*n)*(n-1))  ((id^(n-x)*sym(1,1)^x*id^(n-x))^x<--n;   sym((2*n)*(n-1),2*n)),unzip =  \\n.Tr((2*n)*(n-1))  (((id^(x+1)*sym(1,1)^((n-x)-1)*id^(x+1))^x<--n);   sym((2*n)*(n-1),2*n)), sequencer =  \\n.((dupl^n;unzip(n:I)) *    Tr(n)(sym(n-1,1);((fifofull;dupl)*((fifo ; dupl)^(n-1)));         unzip(n:I))) ;    (id^n*(zip(n:I) ; drain^n))}")))
//
//    standardTests(reoGraph11, program11)
//
//    standardTests2(program11, model11)
//  }
//
//  def getNodes(graph: ReoGraph): List[Int] = graph match{
//    case ReoGraph(edges, _, _) => getNodesFromEdge(edges).distinct
//    case _ => Nil
//  }
//
//  def getNodesFromEdge(edges: List[Edge]): List[Int] = edges match{
//    case Edge(prim, ins, outs) :: rest => ins ++ outs ++ getNodesFromEdge(rest)
//    case Nil => Nil
//  }
//
//  def standardTests(graph: ReoGraph, program: Mcrl2Program): Unit ={
//      val nodes = getNodes(graph)
//      //we need to check that the number of nodes in the graph equals the ones on the program
//      val mcrl2nodes = program.getNodes
//      assertEquals(nodes.length, mcrl2nodes.length)
//      //we need to check that the number of channels equals the number of edges minus writers and readers
//      val channels = program.getChannels
//      assertEquals(graph.edges.count{case Edge(CPrim(name, _, _, _), _, _) => name != "writer" && name != "reader"}, channels.length)
//      //we need to check that only the in nodes and the out nodes can have null as action
//      val ins = graph.ins
//      val outs = graph.outs
//      for(act <- program.getActions){
//        assert(act.group > 0 && act.group < 7)
//        assert(if(act.group == 6) act.number == 0 else true)
//      }
//      for(node <- mcrl2nodes){
//        if(node.getBefore == Action(0, 6)) assert(ins.contains(node.number))
//        if(node.getAfter == Action(0, 6)) assert(outs.contains(node.number))
//      }
//      //we need to check that the inits cover all the actions except the ins, outs and nulls
//    println(program.getActions.toList)
//      var acts = program.getActions.toList.filter{case Action(_, n, g, s) => g < 4 && g > 0  &&  s != 5 }
//      println(acts)
//      val inits = program.getInits
//      for(Mcrl2Init(_, a, _) <- inits){
//        acts = acts.filter{case Action(_,n, _, s ) => n != a.get_number || s != a.state}
//      }
//      println(acts)
//      assert(acts.isEmpty)
//  }
//
//  def standardTests2(p: Mcrl2Program, m: Mcrl2Model): Unit = {
//    assertEquals(p.getInits.length, m.getInits.length)
//    assertEquals(p.getActions.toList.length, m.getActions.toList.length)
//    assertEquals(p.getChannels.length, m.getChannels.length)
//    assertEquals(p.getNodes.length, m.getNodes.length)
//  }
//}
