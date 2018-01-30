package preo.modelling

import preo.ast.{CSeq, CSymmetry, CPar, CPrim, CSubConnector, CTrace, CId, CoreConnector, CoreInterface}


class Mcrl2Model(act: Set[Action], proc: List[Mcrl2Def], init: Mcrl2Process) {
  override def toString: String = {
    val acts = Mcrl2Def.toString(act.toList)
    var procs = ""
    for(p <- proc) procs += s"${p.toString};\n"
    val initProc = init.toString
    s"""
       |act
       |  $acts;
       |proc
       |  $procs
       |init
       |  $initProc;
      """.stripMargin
  }

  def webString: String = {
    val acts = Mcrl2Def.toString(act.toList)
    var procs = ""
    for(p <- proc) procs += s"${p.toString};<br>\n"
    val initProc = init.toString
    s"""
       |act <br>
       |  $acts;<br>
       |  <br>
       |proc<br>
       |<br>
       |  $procs<br>
       |init<br>
       |<br>
       |  $initProc;<br>
      """.stripMargin
  }

  def getStarterNodes: List[Mcrl2Node] = proc.filter(p => p.isInstanceOf[Mcrl2Node] && p.asInstanceOf[Mcrl2Node].getBefore.equals(Action.nullAction)).asInstanceOf[List[Mcrl2Node]]

  def getActions: Set[Action] = act

  def getProc: List[Mcrl2Def] = proc

  def getInit: Mcrl2Process = init

  //testing usefull stuff
  def getNodes: List[Mcrl2Node] = proc.filter(p => p.isInstanceOf[Mcrl2Node]).asInstanceOf[List[Mcrl2Node]]

  def getChannels: List[Mcrl2Channel] = proc.filter(p => p.isInstanceOf[Mcrl2Channel]).asInstanceOf[List[Mcrl2Channel]]

  def getInits: List[Mcrl2Init] = proc.filter(p => p.isInstanceOf[Mcrl2Init]).asInstanceOf[List[Mcrl2Init]]


}

object Mcrl2Model{

  var var_count = 1
  var channel_count = 1
  var nodes: List[Mcrl2Node] = List[Mcrl2Node]()
  var last_init: Mcrl2Process = null
  var starterNodes: List[Mcrl2Node] = List[Mcrl2Node]()
  var to_check: List[Mcrl2Def] = List[Mcrl2Def]()
  var missingVars: List[Action] = List[Action]()

  def apply(ccon: CoreConnector): Mcrl2Model = {
    val (_, channels,_) = conToChannels(ccon, Nil, Nil)
    to_check = channels ++ nodes
    missingVars = Util.getVars(channels++nodes).toList.filter{case a@Action(name, number, group, state) => !(group == NoLine && state == Nothing) && a != Action.nullAction}
    if(starterNodes.isEmpty) starterNodes = nodes.head :: starterNodes
    val inits = initsMaker
    if(last_init == null){
      last_init = nodes.head.getName
      for( node <- nodes.tail){
        last_init = Par(last_init, node.getName)
      }
    }
    val program = new Mcrl2Model(Util.getVars(channels++nodes++inits), channels ++ this.nodes ++ inits, last_init)

    var_count = 0
    channel_count = 0
    nodes= List[Mcrl2Node]()
    last_init = null
    starterNodes = List[Mcrl2Node]()
    missingVars = List[Action]()
    to_check = List[Mcrl2Def]()
    program
  }

  def conToChannels(ccon: CoreConnector, in_nodes: List[Mcrl2Node], out_nodes: List[Mcrl2Node]):
    (List[Mcrl2Node], List[Mcrl2Def], List[Mcrl2Node]) = ccon match{
    case CSeq(c1, c2) =>
      val (in1, channel1, out1) = conToChannels(c1, in_nodes, Nil)
      val (_, channel2, out2) = conToChannels(c2, out1, out_nodes)
      (in1, channel1 ++  channel2, out2)

    case CPar(c1, c2) =>
      val (in1, channel1, out1) = conToChannels(c1, in_nodes, out_nodes)
      val (in2, channel2, out2) = conToChannels(c2, in_nodes.drop(in1.length), out_nodes.drop(out1.length))
      (in1 ++ in2, channel1 ++ channel2, out1 ++ out2)

    case CSymmetry(CoreInterface(i), CoreInterface(j)) =>
      val ins = {
        val nodes = Util.makeNodes((channel_count until channel_count + i + j - in_nodes.length).toList)
        this.nodes ++= nodes
        starterNodes ++= nodes
        channel_count += Math.max(0, i + j - in_nodes.length)
        in_nodes.take(i + j) ++ nodes
      }
      val outs = {
        val nodes = Util.makeNodes((channel_count until channel_count+ i + j - out_nodes.length).toList)
        this.nodes ++= nodes
        channel_count += Math.max(0, i + j - out_nodes.length)
        out_nodes.take(i + j) ++ nodes
      }
      val outs2 = outs.drop(i) ++ outs.take(i)
      (ins, makeSyncs(ins, outs), outs2)

    case CTrace(CoreInterface(i), c) =>
      val (ins, channels, outs) = conToChannels(c, in_nodes, out_nodes)
      val sincs = makeSyncs(outs.takeRight(i), ins.takeRight(i) )
      (ins.dropRight(i), channels ++ sincs, outs.dropRight(i))

    case CId(CoreInterface(i)) =>
      val ins = {
        val nodes = Util.makeNodes((channel_count until channel_count + i - in_nodes.length).toList)
        this.nodes ++= nodes
        starterNodes ++= nodes
        channel_count += Math.max(0, i - in_nodes.length)
        in_nodes.take(i) ++ nodes
      }
      val outs = {
        val nodes = Util.makeNodes((channel_count until channel_count+ i - out_nodes.length).toList)
        this.nodes ++= nodes
        channel_count += Math.max(0, i - out_nodes.length)
        out_nodes.take(i) ++ nodes
      }
      (ins, makeSyncs(ins, outs), outs)

    case CSubConnector(_, c) => conToChannels(c, in_nodes, out_nodes)
    case x@CPrim(_, _, _, _) => primToChannel(x, in_nodes, out_nodes)
    case _ => (Nil, Nil, Nil)
  }


  def getChannelNodes(total: Int, existingNodes: List[Mcrl2Node], in_node: Boolean): List[Mcrl2Node] = {
    val nodes = existingNodes.take(total)
    val new_nodes = Util.makeNodes((channel_count until channel_count + total - nodes.length).toList)
    this.nodes ++= new_nodes
    channel_count += total - nodes.length
    if(in_node) starterNodes ++= new_nodes
    nodes ++ new_nodes
  }

  def primToChannel(prim: CPrim, ins: List[Mcrl2Node], outs: List[Mcrl2Node]):
    (List[Mcrl2Node], List[Mcrl2Def], List[Mcrl2Node]) = prim match{
    case CPrim("fifo", CoreInterface(i), CoreInterface(j), _) => {
      //nodes
      val in_node = getChannelNodes(i, ins, true).head
      val out_node = getChannelNodes(j, outs, false).head

      in_node.setRight("fifo", channel_count)
      out_node.setLeft("fifo", channel_count)

      //channel
      val firstAction = Action("fifo",channel_count, TwoLine, In1)
      val secondAction = Action("fifo",channel_count, TwoLine, Out1)

      val channel = Mcrl2Channel("Fifo", channel_count, List(firstAction), List(secondAction),
        Seq(firstAction, secondAction), List(in_node), List(out_node))
      //updating
      in_node.setNext(channel)
      out_node.setPrev(channel)
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
    case CPrim("fifofull", CoreInterface(i), CoreInterface(j), _) => {
      //nodes
      val in_node = getChannelNodes(i, ins, true).head
      val out_node = getChannelNodes(j, outs, false).head

      in_node.setRight("fifofull", channel_count)
      out_node.setLeft("fifofull", channel_count)

      //channel
      val firstAction = Action("fifofull", channel_count, TwoLine, In1)
      val secondAction = Action("fifofull",channel_count, TwoLine, Out1)
      val channel = Mcrl2Channel("FifoFull", channel_count, List(firstAction), List(secondAction),
        Seq(secondAction, firstAction), List(in_node), List(out_node))
      //updating
      in_node.setNext(channel)
      out_node.setPrev(channel)
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
    case CPrim("lossy", CoreInterface(i), CoreInterface(j), _ ) => {
      //nodes
      val in_node = getChannelNodes(i, ins, true).head
      val out_node = getChannelNodes(j, outs, false).head

      in_node.setRight("lossy", channel_count)
      out_node.setLeft("lossy", channel_count)

      //channel
      val firstAction = Action("lossy", channel_count, TwoLine, In1)
      val secondAction = Action("lossy", channel_count, TwoLine, Out1)
      val channel = Mcrl2Channel("Lossy", channel_count, List(firstAction), List(secondAction),
        Choice(firstAction, MultiAction(firstAction, secondAction)), List(in_node), List(out_node))
      //updating
      in_node.setNext(channel)
      out_node.setPrev(channel)
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
    case CPrim("merger", CoreInterface(i), CoreInterface(j),_) => {
      //nodes

      val in_nodes = getChannelNodes(i, ins, true)
      val in_node1 = in_nodes.head
      val in_node2 = in_nodes.last
      val out_node = getChannelNodes(j, outs, false).head


      in_node1.setRight("merger", channel_count)
      in_node2.setRight("merger", channel_count, In2)
      out_node.setLeft("merger", channel_count)

      //channel
      val firstAction = Action("merger", channel_count, TwoLine, In1)
      val secondAction = Action("merger", channel_count, TwoLine, In2)
      val thirdAction = Action("merger", channel_count, TwoLine, Out1)
      val channel = Mcrl2Channel("Merger", channel_count, List(firstAction, secondAction), List(thirdAction),
        Choice(MultiAction(List(firstAction, thirdAction)), MultiAction(secondAction, thirdAction)),
        List(in_node1, in_node2), List(out_node))
      //updating
      in_node1.setNext(channel)
      in_node2.setNext(channel)
      out_node.setPrev(channel)
      channel_count += 1
      (List(in_node1, in_node2), List(channel), List(out_node))
    }
    case CPrim("dupl", CoreInterface(i), CoreInterface(j), _) => {
      val in_node = getChannelNodes(i, ins, true).head
      val out_nodes = getChannelNodes(j, outs, false)
      val out_node1 = out_nodes.head
      val out_node2 = out_nodes.last

      in_node.setRight("dupl", channel_count)
      out_node1.setLeft("dupl", channel_count)
      out_node2.setLeft("dupl", channel_count, Out2)

      //channel
      val firstAction = Action("dupl", channel_count, TwoLine, In1)
      val secondAction = Action("dupl", channel_count, TwoLine, Out1)
      val thirdAction = Action("dupl", channel_count, TwoLine, Out2)

      val channel = Mcrl2Channel("Dupl", channel_count, List(firstAction), List(secondAction, thirdAction),
        MultiAction(List(firstAction, secondAction, thirdAction)), List(in_node), List(out_node1, out_node2))
      //updating
      in_node.setNext(channel)
      out_node1.setPrev(channel)
      out_node2.setPrev(channel)
      channel_count += 1
      (List(in_node), List(channel), List(out_node1, out_node2))
    }
    case CPrim("drain", CoreInterface(i), CoreInterface(j), _) => {
      //nodes
      val in_nodes = getChannelNodes(i, ins, true)
      val in_node1 = in_nodes.head
      val in_node2 = in_nodes.last

      in_node1.setRight("drain", channel_count)
      in_node2.setRight("drain", channel_count, In2)

      //channel
      val firstAction = Action("drain", channel_count, TwoLine, In1)
      val secondAction = Action("drain", channel_count, TwoLine, In2)
      val channel = Mcrl2Channel("Drain", channel_count, List(firstAction, secondAction),Nil,
        MultiAction(List(firstAction, secondAction)), List(in_node1, in_node2), Nil)
      //updating
      in_node1.setNext(channel)
      in_node2.setNext(channel)
      channel_count += 1
      (List(in_node1, in_node2), List(channel), Nil)
    }
    case CPrim("reader", CoreInterface(i), CoreInterface(j), _) => {
      val in_node = getChannelNodes(i, ins, false).head

      in_node.setRight(Action("reader", var_count, NoLine, Nothing))
      var_count +=1
      (List(in_node), Nil, Nil)
    }
    case CPrim("writer", CoreInterface(i), CoreInterface(j), _) => {
      val out_node = getChannelNodes(j, outs, true).head

      out_node.setLeft(Action("writer", var_count, NoLine, Nothing))
      var_count += 1
      (Nil, Nil, List(out_node))
    }
    case CPrim(name, CoreInterface(i), CoreInterface(j), _) => {
      //nodes
      val in_node = getChannelNodes(i, ins, true).head
      val out_node = getChannelNodes(j, outs, false).head

      in_node.setRight(name, channel_count)
      out_node.setLeft(name, channel_count)

      val firstAction = Action(name, channel_count, TwoLine, In1)
      val secondAction = Action(name, channel_count, TwoLine, Out1)
      val channel = Mcrl2Channel(number = channel_count, before = List(firstAction), after = List(secondAction),
        operator = MultiAction(firstAction, secondAction), prev = List(in_node), next = List(out_node))
      in_node.setNext(channel)
      out_node.setPrev(channel)
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
  }



  private def makeSyncs(ins: List[Mcrl2Node], outs: List[Mcrl2Node]): List[Mcrl2Def] = (ins, outs) match{
    case (i::r1, o::r2) =>{
      i.setRight("sync", channel_count)
      o.setLeft("sync", channel_count)

      val firstAction = Action("sync", channel_count, TwoLine, In1)
      val secondAction = Action("sync", channel_count, TwoLine, Out1)
      val channel = Mcrl2Channel(number = channel_count, before = List(firstAction), after = List(secondAction),
        operator = MultiAction(firstAction, secondAction), prev = List(i), next = List(o))
      i.setNext(channel)
      o.setPrev(channel)
      channel_count += 1
      channel :: makeSyncs(r1, r2)
    }
    case (Nil, Nil) => Nil
  }


  private def initsMaker: List[Mcrl2Init] =
    if(starterNodes.nonEmpty){
      val inits = makeInitsNode(starterNodes.head, null)
      starterNodes = starterNodes.tail
      if(inits.nonEmpty) {
        last_init = if (last_init == null) inits.last.getName else Par(last_init, inits.last.getName)
      }
      inits ++ initsMaker
    }
    else if(missingVars.nonEmpty) {
      val inits = makeblockers(missingVars, last_init)
      last_init = inits.last.getName
      inits
    }
    else{
      Nil
    }


  private def check(element: Mcrl2Def): Unit = to_check = to_check.filter(x => x != element)

  private def notMissing(action: Action): Unit = missingVars = missingVars.filter(x=> x.get_number !=  action.get_number || x.state != action.state)

  private def makeblockers(actions: List[Action], last: Mcrl2Process): List[Mcrl2Init] = actions match{
    case Action(name, number, group, state) :: rest => {
      val filtered_rest = rest.filter{case Action(_, n, g,s) => n != number || s != state}
      val m = Mcrl2Init(channel_count, name, number,state, last)
      channel_count += 1
      m :: makeblockers(filtered_rest, m.getName)
    }
    case Nil => Nil
  }

  //we have to prepare this to the spout in case we had it
  private def makeInitsNode(current: Mcrl2Node, last: Mcrl2Def, backwards: Boolean = false): List[Mcrl2Init] = {
    if(to_check.contains(current)) {
      //in this case we know the node doesn't have prev
      //we can also assure that in this case backwards = false
      check(current)
      if (last == null) {
        makeInitsChannel(current.getNext, current, current)
      }
      else {
        if (!backwards) {
          notMissing(current.before)
          val Action(name, number, group, state) = current.before
          val init = Mcrl2Init(channel_count, name, number, state, current.getName, last.getName)
          channel_count += 1
          val rest = makeInitsChannel(current.next, init, current)
          init :: rest
        }
        else{
          notMissing(current.getAfter)
          val Action(name, number, group, state) = current.after
          var init = Mcrl2Init(channel_count, name, number, state, current.getName, last.getName)
          channel_count +=1
          val rest = makeInitsChannel(current.prev, init, current, true)
          init::rest
        }
      }
    }
    else{
      Nil
    }
  }

  private def makeInitsChannel(current: Mcrl2Channel, last: Mcrl2Def, last_node: Mcrl2Node, backwards: Boolean = false): List[Mcrl2Init] = {
    if(to_check.contains(current)) {
      check(current)
      //this will never happen but oh well
      if (last == null) {
        makeInitsNode(current.getNext.head, current)
      }
      else {
        if (!backwards) {
          notMissing(last_node.getAfter)
          val Action(name, number, group, state) = last_node.getAfter
          var inits = List(Mcrl2Init(channel_count, name, number, state, current.getName, last.getName))
          channel_count += 1
          for(n <- current.getNext){
            val last = inits.last
            inits ++= makeInitsNode(n, last)
          }
          for(n <- current.getPrev){
            if(n != last_node) {
              val last = inits.last
              inits ++= makeInitsNode(n, last, true)
            }
          }
          inits
        }
        else{
          notMissing(last_node.getBefore)
          val Action(name, number, group, state) = last_node.getBefore
          var inits = List(Mcrl2Init(channel_count, name, number, state, current.getName, last.getName))
          channel_count += 1
          for(n <- current.getPrev){
            val last = inits.last
            inits ++= makeInitsNode(n, last, true)
          }
          for(n <- current.getNext){
            if(n != last_node) {
              val last = inits.last
              inits ++= makeInitsNode(n, last)
            }
          }
          inits
        }
      }
    }
    else{
      Nil
    }
  }



}

