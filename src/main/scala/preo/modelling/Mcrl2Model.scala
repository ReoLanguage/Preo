package preo.modelling

import preo.ast.{CSeq, CSymmetry, CPar, CPrim, CSubConnector, CTrace, CId, CoreConnector, CoreInterface}

//we need testing

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

  //testing usefull stuff
  def getNodes: List[Mcrl2Node] = proc.filter(p => p.isInstanceOf[Mcrl2Node]).asInstanceOf[List[Mcrl2Node]]

  def getChannels: List[Mcrl2Channel] = proc.filter(p => p.isInstanceOf[Mcrl2Channel]).asInstanceOf[List[Mcrl2Channel]]

  def getInits: List[Mcrl2Init] = proc.filter(p => p.isInstanceOf[Mcrl2Init]).asInstanceOf[List[Mcrl2Init]]

  def getActions: Set[Action] = act
}

object Mcrl2Model{

  var var_count = 0
  var channel_count = 0
  var nodes: List[Mcrl2Node] = List[Mcrl2Node]()
  var last_init: Mcrl2Process = null //maybe obsolete
  var starterNodes: List[Mcrl2Node] = List[Mcrl2Node]()
  var to_check: List[Mcrl2Def] = List[Mcrl2Def]()
  var missingVars: List[Action] = List[Action]()

  def apply(ccon: CoreConnector): Mcrl2Model = {
    val (_, channels,_) = conToChannels(ccon, Nil, Nil)
    to_check = channels ++ nodes
    missingVars = getVars(channels++nodes).toList.filter{case Action(number, group) => group < 3}
    if(starterNodes.isEmpty) starterNodes = nodes.head :: starterNodes
    val inits = initsMaker
    if(last_init == null){
      last_init = nodes.head.getName
      for( node <- nodes.tail){
        last_init = Par(last_init, node.getName)
      }
    }
    val program = new Mcrl2Model(getVars(channels++nodes++inits), channels ++ this.nodes ++ inits, last_init)
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
    case CSeq(c1, c2) => {
      val (in1, channel1, out1) = conToChannels(c1, in_nodes, Nil)
      val (in2, channel2, out2) = conToChannels(c2, out1, out_nodes)
      (in1, channel1 ++  channel2, out2)
    }
    case CPar(c1, c2) => {
      val (in1, channel1, out1) = conToChannels(c1, in_nodes, out_nodes)
      val (in2, channel2, out2) = conToChannels(c2, in_nodes.drop(in1.length), out_nodes.drop(out1.length))
      (in1 ++ in2, channel1 ++ channel2, out1 ++ out2)
    }
    case CSymmetry(CoreInterface(i), CoreInterface(j)) => {
      val ins = {
        val nodes = makeNodes((channel_count until channel_count + i + j - in_nodes.length).toList)
        starterNodes ++= nodes
        channel_count += Math.max(0, i + j - in_nodes.length)
        in_nodes.take(i + j) ++ nodes
      }
      val outs = {
        val nodes = makeNodes((channel_count until channel_count+ i + j - out_nodes.length).toList)
        channel_count += Math.max(0, i + j - out_nodes.length)
        out_nodes.take(i + j) ++ nodes
      }
      val outs2 = outs.drop(i) ++ outs.take(i)
      (ins, makeSyncs(ins, outs), outs2)
    }
    case CTrace(CoreInterface(i), c) => {
      val (ins, channels, outs) = conToChannels(c, in_nodes, out_nodes);
      val sincs = makeSyncs(outs.takeRight(i), ins.takeRight(i) )
      (ins.dropRight(i), channels ++ sincs, outs.dropRight(i))
    }
    case CId(CoreInterface(i)) => {
      val ins = {
        val nodes = makeNodes((channel_count until channel_count + i - in_nodes.length).toList)
        starterNodes ++= nodes
        channel_count += Math.max(0, i - in_nodes.length)
        in_nodes.take(i) ++ nodes
      }
      val outs = {
        val nodes = makeNodes((channel_count until channel_count+ i - out_nodes.length).toList)
        channel_count += Math.max(0, i - out_nodes.length)
        out_nodes.take(i) ++ nodes
      }
      (ins, makeSyncs(ins, outs), outs)
    }
    case CSubConnector(_, c) => conToChannels(c, in_nodes, out_nodes)
    case x@CPrim(_, _, _, _) => primToChannel(x, in_nodes, out_nodes)
    case _ => (Nil, Nil, Nil)
  }


  //todo: simplify this
  def primToChannel(prim: CPrim, ins: List[Mcrl2Node], outs: List[Mcrl2Node]):
    (List[Mcrl2Node], List[Mcrl2Def], List[Mcrl2Node]) = prim match{
    case CPrim("fifo", _, _, _) => {
      //nodes
      val in_node = if (ins.isEmpty){
        val node = makeNodes(List(channel_count)).head
        channel_count +=1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.head

      val out_node = if(outs.isEmpty){
        val node = makeNodes(List(channel_count)).head
        channel_count +=1
        node
      } else outs.head

      in_node.setRight(var_count)
      out_node.setLeft(var_count+1)

      //channel
      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count+1, 1)

      val channel = Mcrl2Channel("Fifo", channel_count, List(firstAction), List(secondAction),
        Seq(firstAction, secondAction), List(in_node), List(out_node))
      //updating
      in_node.setNext(channel)
      out_node.setPrev(channel)
      var_count +=2
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
    case CPrim("fifofull", _, _, _) => {
      //nodes
      val in_node = if (ins.isEmpty){
        val node = makeNodes(List(channel_count)).head
        channel_count +=1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.head

      val out_node = if(outs.isEmpty){
        val node = makeNodes(List(channel_count)).head
        channel_count +=1
        node
      } else outs.head

      in_node.setRight(var_count)
      out_node.setLeft(var_count+1)

      //channel
      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count+1, 1)
      val channel = Mcrl2Channel("FifoFull", channel_count, List(firstAction), List(secondAction),
        Seq(secondAction, firstAction), List(in_node), List(out_node))
      //updating
      in_node.setNext(channel)
      out_node.setPrev(channel)
      var_count +=2
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
    case CPrim("lossy",_,_, _ ) => {
      //nodes
      val in_node = if (ins.isEmpty){
        val node = makeNodes(List(channel_count)).head
        channel_count +=1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.head

      val out_node = if(outs.isEmpty){
        val node = makeNodes(List(channel_count)).head
        channel_count +=1
        node
      } else outs.head

      in_node.setRight(var_count)
      out_node.setLeft(var_count+1)

      //channel
      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count+1, 1)
      val channel = Mcrl2Channel("Lossy", channel_count, List(firstAction), List(secondAction),
        Choice(firstAction, MultiAction(firstAction, secondAction)), List(in_node), List(out_node))
      //updating
      in_node.setNext(channel)
      out_node.setPrev(channel)
      var_count +=2
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
    case CPrim("merger",_,_,_) => {
      //nodes
      val in_node1 = if (ins.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.head

      val in_node2 = if (ins.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.tail.head

      val out_node = if (outs.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        node
      } else outs.head

      in_node1.setRight(var_count)
      in_node2.setRight(var_count + 1)
      out_node.setLeft(var_count + 2)

      //channel
      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count + 1, 1)
      val thirdAction = Action(var_count + 2, 1)
      val channel = Mcrl2Channel("Merger", channel_count, List(firstAction, secondAction), List(thirdAction),
        Choice(MultiAction(List(firstAction, thirdAction)), MultiAction(secondAction, thirdAction)),
        List(in_node1, in_node2), List(out_node))
      //updating
      in_node1.setNext(channel)
      in_node2.setNext(channel)
      out_node.setPrev(channel)
      var_count += 3
      channel_count += 1
      (List(in_node1, in_node2), List(channel), List(out_node))
    }
    case CPrim("dupl",_,_, _) => {
      val in_node = if (ins.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.head

      val out_node1 = if (outs.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else outs.head

      val out_node2 = if (outs.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        node
      } else outs.tail.head

      in_node.setRight(var_count)
      out_node1.setLeft(var_count + 1)
      out_node2.setLeft(var_count + 2)

      //channel
      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count+1, 1)
      val thirdAction = Action(var_count+2, 1)

      val channel = Mcrl2Channel("Dupl", channel_count, List(firstAction), List(secondAction, thirdAction),
        MultiAction(List(firstAction, secondAction, thirdAction)), List(in_node), List(out_node1, out_node2))
      //updating
      in_node.setNext(channel)
      out_node1.setPrev(channel)
      out_node2.setPrev(channel)
      var_count +=3
      channel_count += 1
      (List(in_node), List(channel), List(out_node1, out_node2))
    }
    case CPrim("drain",_,_, _) => {
      //nodes
      val in_node1 = if (ins.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.head

      val in_node2 = if (ins.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.tail.head
      in_node1.setRight(var_count)
      in_node2.setRight(var_count + 1)

      //channel
      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count+1, 1)
      val channel = Mcrl2Channel("Drain", channel_count, List(firstAction, secondAction),Nil,
        MultiAction(List(firstAction, secondAction)), List(in_node1, in_node2), Nil)
      //updating
      in_node1.setNext(channel)
      in_node2.setNext(channel)
      var_count += 2
      channel_count += 1
      (List(in_node1, in_node2), List(channel), Nil)
    }
    case CPrim("reader", _, _, _) => {
      val in_node = if (ins.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        node
      } else ins.head

      in_node.setRight(Action(var_count, 4))
      var_count +=1
      (List(in_node), Nil, Nil)
    }
    case CPrim("writer", _, _, _) => {
      val out_node = if (outs.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else outs.head
      out_node.setLeft(Action(var_count, 5))
      var_count += 1
      (Nil, Nil, List(out_node))
    }
    case CPrim(_, _, _, _) => {
      //nodes
      val in_node = if (ins.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        starterNodes = starterNodes ++ List(node)
        node
      } else ins.head

      val out_node = if (outs.isEmpty) {
        val node = makeNodes(List(channel_count)).head
        channel_count += 1
        node
      } else outs.head

      in_node.setRight(var_count)
      out_node.setLeft(var_count + 1)


      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count + 1, 1)
      val channel = Mcrl2Channel(number = channel_count, before = List(firstAction), after = List(secondAction),
        operator = MultiAction(firstAction, secondAction), prev = List(in_node), next = List(out_node))
      in_node.setNext(channel)
      out_node.setPrev(channel)
      var_count += 2
      channel_count += 1
      (List(in_node), List(channel), List(out_node))
    }
  }

  private def makeNodes(numbers: List[Int]): List[Mcrl2Node] = numbers match{
    case n :: rest => val node =  Mcrl2Node(n, Action(0, 6), Action(0, 6)); this.nodes ++= List(node) ;node :: makeNodes(rest)
    case Nil => Nil
  }

  private def makeSyncs(ins: List[Mcrl2Node], outs: List[Mcrl2Node]): List[Mcrl2Def] = (ins, outs) match{
    case (i::r1, o::r2) =>{
      i.setRight(var_count)
      o.setLeft(var_count+1)

      val firstAction = Action(var_count, 1)
      val secondAction = Action(var_count + 1, 1)
      val channel = Mcrl2Channel(number = channel_count, before = List(firstAction), after = List(secondAction),
        operator = MultiAction(firstAction, secondAction), prev = List(i), next = List(o))
      i.setNext(channel)
      o.setPrev(channel)
      var_count += 2
      channel_count += 1
      channel :: makeSyncs(r1, r2)
    }
    case (Nil, Nil) => Nil
  }

  private def getVars(defs: List[Mcrl2Def]): Set[Action] = defs match{
    case head :: tail => head.getVars.toSet ++ getVars(tail)
    case Nil => Set()
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

  private def notMissing(action: Action): Unit = missingVars = missingVars.filter(x=> x.get_number !=  action.get_number)

  private def makeblockers(actions: List[Action], last: Mcrl2Process): List[Mcrl2Init] = actions match{
    case Action(number, group) :: rest => {
      val filtered_rest = rest.filter{case Action(n, g) => n != number}
      val m = Mcrl2Init(channel_count, number, last)
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
          val init = Mcrl2Init(channel_count, current.before.get_number, current.getName, last.getName)
          channel_count += 1
          val rest = makeInitsChannel(current.next, init, current)
          init :: rest
        }
        else{
          notMissing(current.getAfter)
          var init = Mcrl2Init(channel_count, current.after.get_number, current.getName, last.getName)
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
          var inits = List(Mcrl2Init(channel_count, last_node.getAfter.get_number, current.getName, last.getName))
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
          var inits = List(Mcrl2Init(channel_count, last_node.getBefore.get_number, current.getName, last.getName))
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

