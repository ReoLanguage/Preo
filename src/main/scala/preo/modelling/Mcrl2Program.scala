//package preo.modelling
//
//import preo.ast.{CPrim, CoreConnector}
//import preo.backend.ReoGraph
//import preo.backend.ReoGraph.Edge
//
//
//@Deprecated
//class Mcrl2Program(act: Set[Action], proc: List[Mcrl2Def], init: Mcrl2Process) {
//  override def toString: String = {
//    val acts = Mcrl2Def.toString(act.toList)
//    var procs = ""
//    for(p <- proc) procs += s"${p.toString};\n"
//    val initProc = init.toString
//    s"""
//       |act
//       |  $acts;
//       |proc
//       |  $procs
//       |init
//       |  $initProc;
//      """.stripMargin
//  }
//
//  def webString: String = {
//    val acts = Mcrl2Def.toString(act.toList)
//    var procs = ""
//    for(p <- proc) procs += s"${p.toString};<br>\n"
//    val initProc = init.toString
//    s"""
//       |act <br>
//       |  $acts;<br>
//       |  <br>
//       |proc<br>
//       |<br>
//       |  $procs<br>
//       |init<br>
//       |<br>
//       |  $initProc;<br>
//      """.stripMargin
//  }
//
//  //testing usefull stuff
//  def getNodes: List[Mcrl2Node] = proc.filter(p => p.isInstanceOf[Mcrl2Node]).asInstanceOf[List[Mcrl2Node]]
//
//  def getChannels: List[Mcrl2Channel] = proc.filter(p => p.isInstanceOf[Mcrl2Channel]).asInstanceOf[List[Mcrl2Channel]]
//
//  def getInits: List[Mcrl2Init] = proc.filter(p => p.isInstanceOf[Mcrl2Init]).asInstanceOf[List[Mcrl2Init]]
//
//  def getActions: Set[Action] = act
//}
//
//object Mcrl2Program{
//
//  var var_count = 1
//  var channel_count = 1
//  var nodes: List[Mcrl2Node] = List[Mcrl2Node]()
//  var last_init: Mcrl2Process = null //maybe obsolete
//  var starterNodes: List[Mcrl2Node] = List[Mcrl2Node]()
//  var to_check: List[Mcrl2Def] = List[Mcrl2Def]()
//  var missingVars: List[Action] = List[Action]()
//
//  def apply(con: CoreConnector): Mcrl2Program = graphToMcrl2(ReoGraph.toGraph(con))
//
//  def apply(reoGraph: ReoGraph): Mcrl2Program = graphToMcrl2(reoGraph)
//
//  def graphToMcrl2(graph: ReoGraph): Mcrl2Program = graph match{
//    case ReoGraph(edges, ins, outs) => {
//      this.starterNodes = makeNodes(ins)
//      nodes = nodes ++ starterNodes ++ makeNodes(outs)
//      val channels = edgetoMcrl2(edges)
//      to_check = channels ++ nodes
//      missingVars = getVars(channels++nodes).toList.filter{case a@Action(name, number, group, state) => state != Nothing && a != Action.nullAction}
//      if(starterNodes.isEmpty) starterNodes = nodes.head :: starterNodes
//      val inits = initsMaker
//      if(last_init == null){
//        last_init = nodes.head.getName
//        for( node <- nodes.tail){
//          last_init = Par(last_init, node.getName)
//        }
//      }
//      val program = new Mcrl2Program(getVars(channels++nodes++inits), channels ++ this.nodes ++ inits, last_init)
//      var_count = 0
//      channel_count = 0
//      nodes= List[Mcrl2Node]()
//      last_init = null
//      starterNodes = List[Mcrl2Node]()
//      missingVars = List[Action]()
//      to_check = List[Mcrl2Def]()
//      program
//    }
//  }
//
//  private def edgetoMcrl2(edges: List[Edge]): List[Mcrl2Channel] = edges match{
//    case Edge(prim: CPrim, ins:List[Int], outs:List[Int]):: rest =>{
//      var nodes_in = List[Mcrl2Node]()
//      for(in <- ins){
//        nodes_in ++= List(getNode(in, nodes))
//      }
//      var nodes_out = List[Mcrl2Node]()
//      for(out <- outs){
//        nodes_out ++= List(getNode(out, nodes))
//      }
//      val channel = primToChannel(prim, nodes_in, nodes_out)
//      if(channel != null) {
//        channel :: edgetoMcrl2(rest)
//      }
//      else{
//        edgetoMcrl2(rest)
//      }
//    }
//    case Nil => Nil
//  }
//
//  //We must have confidence that at this point, the number of ins and outs will coincide
//  private def primToChannel(prim: CPrim, ins: List[Mcrl2Node], outs: List[Mcrl2Node]): Mcrl2Channel = prim match{
//    case CPrim("fifo", _, _, _) => {
//      //nodes
//      val in_node = ins.head
//      val out_node = outs.head
//      in_node.setRight("fifo", channel_count)
//      out_node.setLeft("fifo", channel_count)
//      //channel
//      val firstAction = Action("fifo", channel_count, TwoLine, In1)
//      val secondAction = Action("fifo", channel_count, TwoLine, Out1)
//      val channel = Mcrl2Channel("Fifo", channel_count, List(firstAction), List(secondAction),
//        Seq(firstAction, secondAction), List(in_node), List(out_node))
//      //updating
//      in_node.setNext(channel)
//      out_node.setPrev(channel)
//      channel_count += 1
//      channel
//    }
//    case CPrim("fifofull", _, _, _) => {
//      //nodes
//      val in_node = ins.head
//      val out_node = outs.head
//      in_node.setRight("fifofull", channel_count)
//      out_node.setLeft("fifofull", channel_count)
//      //channel
//      val firstAction = Action("fifofull", channel_count, TwoLine, In1)
//      val secondAction = Action("fifofull", channel_count, TwoLine, Out1)
//      val channel = Mcrl2Channel("FifoFull", channel_count, List(firstAction), List(secondAction),
//        Seq(secondAction, firstAction), List(in_node), List(out_node))
//      //updating
//      in_node.setNext(channel)
//      out_node.setPrev(channel)
//      channel_count += 1
//      channel
//    }
//    case CPrim("lossy",_,_, _ ) => {
//      //nodes
//      val in_node = ins.head
//      val out_node = outs.head
//      in_node.setRight("lossy", channel_count)
//      out_node.setLeft("lossy", channel_count)
//      //channel
//      val firstAction = Action("lossy", channel_count, TwoLine, In1)
//      val secondAction = Action("lossy", channel_count, TwoLine, Out1)
//      val channel = Mcrl2Channel("Lossy", channel_count, List(firstAction), List(secondAction),
//              Choice(firstAction, MultiAction(firstAction, secondAction)), List(in_node), List(out_node))
//      //updating
//      in_node.setNext(channel)
//      out_node.setPrev(channel)
//      channel_count += 1
//      channel
//    }
//    case CPrim("merger",_,_,_) => {
//      //nodes
//      val in_node1 = ins.head
//      val in_node2 = ins.last
//      val out_node = outs.head
//      in_node1.setRight("merger", channel_count)
//      in_node2.setRight("merger", channel_count, In2)
//      out_node.setLeft("merger", channel_count)
//      //channel
//      val firstAction = Action("merger", channel_count, TwoLine, In1)
//      val secondAction = Action("merger", channel_count, TwoLine, In2)
//      val thirdAction = Action("merger", channel_count, TwoLine, Out1)
//      val channel = Mcrl2Channel("Merger", channel_count, List(firstAction, secondAction), List(thirdAction),
//              Choice(MultiAction(List(firstAction, thirdAction)), MultiAction(secondAction, thirdAction)),
//        List(in_node1, in_node2), List(out_node))
//      //updating
//      in_node1.setNext(channel)
//      in_node2.setNext(channel)
//      out_node.setPrev(channel)
//      channel_count += 1
//      channel
//    }
//    case CPrim("dupl",_,_, _) => {
//      //nodes
//      val in_node = ins.head
//      val out_node1 = outs.head
//      val out_node2 = outs.last
//      in_node.setRight("dupl", channel_count)
//      out_node1.setLeft("dupl", channel_count)
//      out_node2.setLeft("dupl", channel_count, Out2)
//      //channel
//      val firstAction = Action("dupl", channel_count, TwoLine, In1)
//      val secondAction = Action("dupl", channel_count, TwoLine, Out1)
//      val thirdAction = Action("dupl", channel_count, TwoLine, Out2)
//
//      val channel = Mcrl2Channel("Dupl", channel_count, List(firstAction), List(secondAction, thirdAction),
//              MultiAction(List(firstAction, secondAction, thirdAction)), List(in_node), List(out_node1, out_node2))
//      //updating
//      in_node.setNext(channel)
//      out_node1.setPrev(channel)
//      out_node2.setPrev(channel)
//      channel_count += 1
//      channel
//    }
//    case CPrim("drain",_,_, _) => {
//      //nodes
//      val in_node1 = ins.head
//      val in_node2 = ins.last
//      in_node1.setRight("drain", channel_count)
//      in_node2.setRight("drain", channel_count, In2)
//      //channel
//      val firstAction = Action("drain", channel_count, TwoLine, In1)
//      val secondAction = Action("drain", channel_count, TwoLine, In2)
//      val channel = Mcrl2Channel("Drain", channel_count, List(firstAction, secondAction),Nil,
//              MultiAction(List(firstAction, secondAction)), List(in_node1, in_node2), Nil)
//      //updating
//      in_node1.setNext(channel)
//      in_node2.setNext(channel)
//      var_count += 2
//      channel_count += 1
//      channel
//    }
//    case CPrim("reader", _, _, _) => {
//      val in_node = ins.head
//      in_node.setRight(Action("reader", var_count, NoLine, Nothing))
//      var_count +=1
//      null
//    }
//    case CPrim("writer", _, _, _) => {
//      val out_node = outs.head
//      out_node.setLeft(Action("writer", var_count, NoLine, Nothing))
//      this.starterNodes = starterNodes ++ List(out_node)
//      var_count += 1
//      null
//    }
//
//    case CPrim(name, _, _, _) => {
//      val in_node = ins.head
//      val out_node = outs.head
//      in_node.setRight(name, channel_count)
//      out_node.setLeft(name, channel_count)
//      val firstAction = Action(name, channel_count, TwoLine, In1)
//      val secondAction = Action(name, channel_count, TwoLine, Out1)
//      val channel = Mcrl2Channel(number = channel_count, before= List(firstAction), after= List(secondAction),
//        operator = MultiAction(firstAction, secondAction), prev = List(in_node), next = List(out_node))
//      in_node.setNext(channel)
//      out_node.setPrev(channel)
//      channel_count += 1
//      channel
//    }
//  }
//
//  private def makeNodes(numbers: List[Int]): List[Mcrl2Node] = numbers match{
//    case n :: rest => Mcrl2Node(n, Action.nullAction, Action.nullAction) :: makeNodes(rest)
//    case Nil => Nil
//  }
//
//  private def getNode(number: Int, nodes: List[Mcrl2Node]): Mcrl2Node = nodes match{
//    case Nil => {val node = makeNodes(List(number)); this.nodes ++= node; node.head}
//    case (m@Mcrl2Node(n, _, _, _, _)) :: rest => if (n == number) m else getNode(number, rest)
//  }
//
//  private def getVars(defs: List[Mcrl2Def]): Set[Action] = defs match{
//    case head :: tail => head.getVars.toSet ++ getVars(tail)
//    case Nil => Set()
//  }
//
//  private def initsMaker: List[Mcrl2Init] =
//    if(starterNodes.nonEmpty){
//      val inits = makeInitsNode(starterNodes.head, null)
//      starterNodes = starterNodes.tail
//      if(inits.nonEmpty) {
//        last_init = if (last_init == null) inits.last.getName else Par(last_init, inits.last.getName)
//      }
//      inits ++ initsMaker
//    }
//    else if(missingVars.nonEmpty) {
//      val inits = makeblockers(missingVars, last_init)
//      last_init = inits.last.getName
//      inits
//    }
//    else{
//      Nil
//    }
//
//
//  private def check(element: Mcrl2Def): Unit = to_check = to_check.filter(x => x != element)
//
//  private def notMissing(action: Action): Unit = missingVars = missingVars.filter(x=> x.get_number !=  action.get_number || x.state != action.state)
//
//  private def makeblockers(actions: List[Action], last: Mcrl2Process): List[Mcrl2Init] = actions match{
//    case Action(name, number, group, state) :: rest => {
//      val filtered_rest = rest.filter{case Action(_, n, g,_) => n != number}
//      val m = Mcrl2Init(channel_count, name, number,state, last)
//      channel_count += 1
//      m :: makeblockers(filtered_rest, m.getName)
//    }
//    case Nil => Nil
//  }
//
//  //we have to prepare this to the spout in case we had it
//  private def makeInitsNode(current: Mcrl2Node, last: Mcrl2Def, backwards: Boolean = false): List[Mcrl2Init] = {
//    if(to_check.contains(current)) {
//      //in this case we know the node doesn't have prev
//      //we can also assure that in this case backwards = false
//      check(current)
//      if (last == null) {
//        makeInitsChannel(current.getNext, current, current)
//      }
//      else {
//        if (!backwards) {
//          notMissing(current.before)
//          val Action(name, number, group, state) = current.before
//          val init = Mcrl2Init(channel_count, name, number, state, current.getName, last.getName)
//          channel_count += 1
//          val rest = makeInitsChannel(current.next, init, current)
//          init :: rest
//        }
//        else{
//          notMissing(current.getAfter)
//          val Action(name, number, group, state) = current.after
//          var init = Mcrl2Init(channel_count, name, number, state, current.getName, last.getName)
//          channel_count +=1
//          val rest = makeInitsChannel(current.prev, init, current, true)
//          init::rest
//        }
//      }
//    }
//    else{
//      Nil
//    }
//  }
//
//  private def makeInitsChannel(current: Mcrl2Channel, last: Mcrl2Def, last_node: Mcrl2Node, backwards: Boolean = false): List[Mcrl2Init] = {
//    if(to_check.contains(current)) {
//      check(current)
//      //this will never happen but oh well
//      if (last == null) {
//        makeInitsNode(current.getNext.head, current)
//      }
//      else {
//        if (!backwards) {
//          notMissing(last_node.getAfter)
//          val Action(name, number, group, state) = last_node.getAfter
//          var inits = List(Mcrl2Init(channel_count, name, number, state, current.getName, last.getName))
//          channel_count += 1
//          for(n <- current.getNext){
//            val last = inits.last
//            inits ++= makeInitsNode(n, last)
//          }
//          for(n <- current.getPrev){
//            if(n != last_node) {
//              val last = inits.last
//              inits ++= makeInitsNode(n, last, true)
//            }
//          }
//          inits
//        }
//        else{
//          notMissing(last_node.getBefore)
//          val Action(name, number, group, state) = last_node.getBefore
//          var inits = List(Mcrl2Init(channel_count, name, number, state, current.getName, last.getName))
//          channel_count += 1
//          for(n <- current.getPrev){
//            val last = inits.last
//            inits ++= makeInitsNode(n, last, true)
//          }
//          for(n <- current.getNext){
//            if(n != last_node) {
//              val last = inits.last
//              inits ++= makeInitsNode(n, last)
//            }
//          }
//          inits
//        }
//      }
//    }
//    else{
//      Nil
//    }
//  }
//
//}
