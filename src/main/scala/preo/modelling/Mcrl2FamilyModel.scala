package preo.modelling

import preo.ast.{CPar, CSeq, Connector, CoreConnector, CSymmetry, CTrace, CId, CoreInterface, CSubConnector, CPrim}
import preo.common.TypeCheckException
import preo.frontend.Eval.conn2CoreConn
import preo.frontend.Solver.varIntIntervals
import preo.frontend._

import util.Random.{nextBoolean, nextInt}


class Mcrl2FamilyModel(act: Set[Action], proc: List[Mcrl2Def], init: Mcrl2Process) {
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

object Mcrl2FamilyModel{


  var starter_count = 0
  var var_count = 0
  var channel_count = 0
  var last_inits: List[(Int, ProcessName)] = Nil
  var to_check: List[Mcrl2Def] = List[Mcrl2Def]()
  var missingVars: Map[Int, List[Action]] = Map()

  var nodes: List[Mcrl2Node] = List[Mcrl2Node]()
  var starterNodes: List[Mcrl2StarterNode] = List[Mcrl2StarterNode]()
  var starters: List[Mcrl2Starter] = List()
  var channels: Map[String, ChannelSubs] = Map()
  var manager: Mcrl2Manager = Mcrl2Manager(Nil)



  //todo: exrouter não funciona
  def apply(con: Connector): Mcrl2FamilyModel ={
    // get applied connectors
    val instances = Eval.getInstances(con)
    //for every value we create an app with the vars in the instances and turn it into a core connector
    val connectors = instances.map(x => Eval.simpleReduce(x))
    //if there is only one possible connector then we simply use the mcrl2model of the connector
    if (connectors.length == 1){
      val m = Mcrl2Model(connectors.head)
      return new Mcrl2FamilyModel(m.getActions, m.getProc, m.getInit)
    }
    var inits: List[Mcrl2Def] = Nil
    var our_starters: List[Mcrl2StarterNode] = List()
    var our_nodes: List[Mcrl2Node] = List()
    for(ccon <- connectors){
      inits = inits ++ convertCcon(ccon)
      val actions: List[Action] = last_inits.map((n: (Int, ProcessName)) => new Action("a", n._1, TwoLine, Nothing))
      manager = manager.addMultiAction(new MultiAction(actions))
      for((n, p) <- last_inits){
        starters = starters ++ List(Mcrl2Starter(n, if(starters.isEmpty) manager.getName else starters.last.getName, p))
      }
      for((name, sub) <- this.channels){
        sub.reset()
      }

      our_nodes = our_nodes ++ nodes
      our_starters = our_starters ++ starterNodes
      last_inits= Nil
      to_check= List[Mcrl2Def]()
      missingVars= Map()


      nodes= List[Mcrl2Node]()
      starterNodes= List[Mcrl2StarterNode]()

    }
    val list_of_channels = channels.values.map(f => f.items).foldRight(Nil: List[Mcrl2Channel])((a, b) => a ++ b)
    val family = new Mcrl2FamilyModel(Util.getVars(list_of_channels ++ our_nodes ++ inits ++ starters ++ our_starters ++ List(manager) ), list_of_channels ++ our_nodes ++ our_starters ++ inits ++ starters ++ List(manager) , starters.last.getName)

    starters= List()
    channel_count = 0
    starter_count = 0
    var_count = 0
    channels= Map()
    manager= Mcrl2Manager(Nil)



    family
  }


  private def convertCcon(ccon: CoreConnector): List[Mcrl2Def] = {
    val (a, channels, _) = conToChannels(ccon, Nil, Nil)
    //missingVars = Util.getVars(channels++nodes).toList.filter{case a@Action(name, number, group, state) => !(group == NoLine && state == Nothing) && a != Action.nullAction}
    starterNodes = a.map(node => makeStarterNode(node))
    if(starterNodes.isEmpty) starterNodes = makeStarterNode(nodes.head) :: starterNodes
    to_check = channels ++ nodes ++ starterNodes
    val inits = initsMaker(starterNodes)
    if(last_inits == Nil){
      for( node <- nodes){
        val s = makeStarterNode(node)
        starterNodes = starterNodes ++ List(s)
        last_inits = last_inits ++ List((s.number, s.getName))
      }
    }
    inits
//
//    var_count = 0
//    channel_count = 0
//    nodes= List[Mcrl2Node]()
//    last_init = null
//    starterNodes = List[Mcrl2Node]()
//    missingVars = List[Action]()
//    to_check = List[Mcrl2Def]()
//    program
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
//        starterNodes = starterNodes ++ nodes.map(node => makeStarterNode(node))
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
//        starterNodes = starterNodes ++ nodes.map(node => makeStarterNode(node))
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

  private def makeSyncs(ins: List[Mcrl2Node], outs: List[Mcrl2Node]): List[Mcrl2Def] = (ins, outs) match {
    case (i :: r1, o :: r2) => {
      val reusable_channels = this.channels.get("Channel")
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()) {
        val channel = reusable_channels.get.getNext()

        val Action(_, bn, _, _) = channel.getBefore.head
        val Action(_, an, _, _) = channel.getAfter.head

        i.setRight("sync", bn)
        o.setLeft("sync", an)
        channel.prev = List(i)
        channel.next = List(o)


        i.setNext(channel)
        o.setPrev(channel)

        channel :: makeSyncs(r1, r2)
      }
      else {
        i.setRight("sync", channel_count)
        o.setLeft("sync", channel_count)

        val firstAction = Action("sync", channel_count, TwoLine, In1)
        val secondAction = Action("sync", channel_count, TwoLine, Out1)
        val channel = Mcrl2Channel(number = channel_count, before = List(firstAction), after = List(secondAction),
          operator = MultiAction(firstAction, secondAction), prev = List(i), next = List(o))

        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("Channel" -> new ChannelSubs("Channel", List(channel)))
        }

        i.setNext(channel)
        o.setPrev(channel)
        channel_count += 1
        channel :: makeSyncs(r1, r2)
      }
    }
    case (Nil, Nil) => Nil
  }


  def getChannelNodes(total: Int, existingNodes: List[Mcrl2Node], in_node: Boolean): List[Mcrl2Node] = {
    val nodes = existingNodes.take(total)
    val new_nodes = Util.makeNodes((channel_count until channel_count + total - nodes.length).toList)
    this.nodes ++= new_nodes
    channel_count += total - nodes.length
//    if(in_node) this.starterNodes = this.starterNodes ++ new_nodes.map(node => makeStarterNode(node))
    nodes ++ new_nodes
  }

  def primToChannelAux(prim: CPrim, ins: List[Mcrl2Node], outs:List[Mcrl2Node], number: Int):
    (List[Mcrl2Node],  List[Mcrl2Node], List[Action], List[Action]) = prim match{
    case CPrim(name, CoreInterface(i), CoreInterface(j), _) => {
      val in_nodes = getChannelNodes(i, ins, true)
      val out_nodes = getChannelNodes(j, outs, false)

      val action_number = if (number == -1) channel_count else number

      var k = false
      val before_actions = in_nodes.map(in_node => {
        in_node.setRight(name, action_number, if(k) In2 else In1)
        val a = Action(name,action_number, TwoLine,if(k) In2 else In1)
        k = true
        a
      })
      k = false
      val after_actions = out_nodes.map(out_node => {
        out_node.setLeft(name, action_number,if(k) Out2 else Out1);
        val a = Action(name,action_number, TwoLine, if(k) Out2 else Out1)
        k = true
        a
      })
      //channel

      (in_nodes, out_nodes, before_actions, after_actions)
    }
  }


  def primToChannel(prim: CPrim, ins: List[Mcrl2Node], outs: List[Mcrl2Node]):
  (List[Mcrl2Node], List[Mcrl2Def], List[Mcrl2Node]) = prim match{
    case CPrim("fifo", CoreInterface(i), CoreInterface(j), _) => {
      //nodes
      val reusable_channels = channels.get("Fifo")
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()){
        val channel = reusable_channels.get.getNext()

        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, channel.number)
        channel.prev = in_nodes
        channel.next = out_nodes
        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})
        (in_nodes, List(channel), out_nodes)
      }
      else{
        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, -1)

        val channel = Mcrl2Channel("Fifo", channel_count, before_actions, after_actions,
          Seq(before_actions.head, after_actions.head), in_nodes, out_nodes)


        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        channel_count += 1
        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("Fifo" -> new ChannelSubs("Fifo", List(channel)))
        }
        (in_nodes, List(channel), out_nodes)
      }
    }
    case CPrim("fifofull", CoreInterface(i), CoreInterface(j), _) => {
      val reusable_channels = channels.get("FifoFull")
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()){
        val channel = reusable_channels.get.getNext()

        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, channel.number)
        channel.prev = in_nodes
        channel.next = out_nodes

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})
        (in_nodes, List(channel), out_nodes)
      }
      else{
        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, -1)

        val channel = Mcrl2Channel("FifoFull", channel_count, before_actions, after_actions,
          Seq(after_actions.head, before_actions.head), in_nodes, out_nodes)

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        channel_count += 1
        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("FifoFull" -> new ChannelSubs("FifoFull", List(channel)))
        }
        (in_nodes, List(channel), out_nodes)
      }
    }
    case CPrim("lossy", CoreInterface(i), CoreInterface(j), _ ) => {
      //nodes

      val reusable_channels = channels.get("Lossy")
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()){
        val channel = reusable_channels.get.getNext()

        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, channel.number)
        channel.prev = in_nodes
        channel.next = out_nodes

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        (in_nodes, List(channel), out_nodes)
      }
      else{
        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, -1)

        val channel = Mcrl2Channel("Lossy", channel_count, before_actions, after_actions,
          Choice(before_actions.head, MultiAction(before_actions.head, after_actions.head)), in_nodes, out_nodes)

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        channel_count += 1
        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("Lossy" -> new ChannelSubs("Lossy", List(channel)))
        }
        (in_nodes, List(channel), out_nodes)
      }
    }
    case CPrim("merger", CoreInterface(i), CoreInterface(j),_) => {
      //nodes

      val reusable_channels = channels.get("Merger")
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()){
        val channel = reusable_channels.get.getNext()

        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, channel.number)
        channel.prev = in_nodes
        channel.next = out_nodes

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})
        (in_nodes, List(channel), out_nodes)
      }
      else{
        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, -1)

        val channel = Mcrl2Channel("Merger", channel_count, before_actions, after_actions,
          Choice(MultiAction(List(before_actions.head, after_actions.head)),
            MultiAction(before_actions.last, after_actions.head)), in_nodes, out_nodes)

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        channel_count += 1
        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("Merger" -> new ChannelSubs("Merger", List(channel)))
        }
        (in_nodes, List(channel), out_nodes)
      }
    }
    case CPrim("dupl", CoreInterface(i), CoreInterface(j), _) => {
      val reusable_channels = channels.get("Dupl")
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()){
        val channel = reusable_channels.get.getNext()

        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, channel.number)
        channel.prev = in_nodes
        channel.next = out_nodes

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})
        (in_nodes, List(channel), out_nodes)
      }
      else{
        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, -1)

        val channel = Mcrl2Channel("Dupl", channel_count, before_actions, after_actions,
          MultiAction(List(before_actions.head, after_actions.head, after_actions.last)), in_nodes, out_nodes)


        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        channel_count += 1
        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("Dupl" -> new ChannelSubs("Dupl", List(channel)))
        }
        (in_nodes, List(channel), out_nodes)
      }

    }
    case CPrim("drain", CoreInterface(i), CoreInterface(j), _) => {
      //nodes
      val reusable_channels = channels.get("Drain")
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()){
        val channel = reusable_channels.get.getNext()

        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, channel.number)
        channel.prev = in_nodes
        channel.next = out_nodes

        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})
        (in_nodes, List(channel), out_nodes)
      }
      else{
        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, -1)

        val channel = Mcrl2Channel("Drain", channel_count, before_actions, after_actions,
          MultiAction(List(before_actions.head, after_actions.head)), in_nodes, out_nodes)


        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        channel_count += 1
        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("Dupl" -> new ChannelSubs("Dupl", List(channel)))
        }
        (in_nodes, List(channel), out_nodes)
      }
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
      val reusable_channels = channels.get(name.head.toUpper.toString ++ name.tail)
      if(reusable_channels.isDefined && reusable_channels.get.hasNext()){
        val channel = reusable_channels.get.getNext()

        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, channel.number)
        channel.prev = in_nodes
        channel.next = out_nodes
        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})
        (in_nodes, List(channel), out_nodes)
      }
      else{
        val (in_nodes, out_nodes, before_actions, after_actions) = primToChannelAux(prim, ins, outs, -1)

        val channel = Mcrl2Channel(name.head.toUpper.toString ++ name.tail, channel_count, before_actions, after_actions,
          MultiAction(before_actions.head, after_actions.head), in_nodes, out_nodes)


        in_nodes.map(node => {node.setNext(channel); null})
        out_nodes.map(node => {node.setPrev(channel); null})

        channel_count += 1
        if(reusable_channels.isDefined){
          reusable_channels.get.put(channel)
        }
        else{
          channels = channels + ("Fifo" -> new ChannelSubs("Fifo", List(channel)))
        }
        (in_nodes, List(channel), out_nodes)
      }
    }
  }

  private def makeStarterNode(node: Mcrl2Node): Mcrl2StarterNode = {
    val s = Mcrl2StarterNode(this.starter_count, node)
    this.starter_count += 1
    s
  }

  private def initsMaker(starters: List[Mcrl2StarterNode]): List[Mcrl2Init] =
    if(starters.nonEmpty){
      var inits = makeInitsStarterNode(starters.head)
      if(missingVars.get(starters.head.number).isDefined && missingVars.get(starters.head.number).get.nonEmpty){
        inits =  inits ++ makeblockers(missingVars(starters.head.number), inits.last.getName)
      }
      if(inits.nonEmpty) {
        last_inits = last_inits++ List((starters.head.number, inits.last.getName))
      }
      inits ++ initsMaker(starters.tail)
    }
    else{
      Nil
    }


  private def check(element: Mcrl2Def): Unit = to_check = to_check.filter(x => x != element)

  private def notMissing(number: Int, action: Action): Unit ={
    val value = missingVars.get(number)
    if(value.isDefined){
      missingVars = missingVars updated (number, value.get.filter(x=> x.get_number !=  action.get_number || x.state != action.state))
    }
  }

  private def isMissing(starter: Int, action: Action): Unit = {
    val value = missingVars.get(starter)
    if(value.isDefined){
      missingVars = missingVars updated (starter, action :: value.get)
    }
    else{
      missingVars = missingVars updated (starter, List(action))
    }
  }

  private def makeblockers(actions: List[Action], last: Mcrl2Process): List[Mcrl2Init] = actions match{
    case Action(name, number, group, state) :: rest => {
      val filtered_rest = rest.filter{case Action(_, n, g,s) => n != number || s != state}
      val m = Mcrl2Init(channel_count, name, number,state, last)
      channel_count += 1
      m :: makeblockers(filtered_rest, m.getName)
    }
    case Nil => Nil
  }

  private def makeInitsStarterNode(current: Mcrl2StarterNode): List[Mcrl2Init] = {
    if(to_check.contains(current)) {
      check(current)
      val inits = makeInitsChannel(current.getNext, current, current.node, current.number)
      inits
    }
    else{
      Nil
    }
  }

  private def makeInitsNode(current: Mcrl2Node, last: Mcrl2Def, starter: Int, backwards: Boolean = false): List[Mcrl2Init] = {
    if(to_check.contains(current)) {
      //in this case we know the node doesn't have prev
      //we can also assure that in this case backwards = false

      check(current)
      if (last == null) {
        if(current.getBefore != Action.nullAction) isMissing(starter, current.getBefore)
        makeInitsChannel(current.getNext, current, current, starter)
      }
      else {
        if (!backwards) {
          notMissing(starter, current.before)
          val Action(name, number, group, state) = current.before
          val init = Mcrl2Init(channel_count, name, number, state, current.getName, last.getName)
          channel_count += 1
          val rest = makeInitsChannel(current.next, init, current, starter)
          init :: rest
        }
        else{
          notMissing(starter, current.getAfter)
          val Action(name, number, group, state) = current.after
          var init = Mcrl2Init(channel_count, name, number, state, current.getName, last.getName)
          channel_count +=1
          val rest = makeInitsChannel(current.prev, init, current,starter,  true)
          init::rest
        }
      }
    }
    else{
      Nil
    }
  }

  private def makeInitsChannel(current: Mcrl2Channel, last: Mcrl2Def, last_node: Mcrl2Node, starter: Int, backwards: Boolean = false): List[Mcrl2Init] = {
    if(to_check.contains(current)) {
      check(current)
      //this will never happen but oh well
      if (last == null) {
        for(action <- current.getBefore)
          if(action != Action.nullAction) isMissing(starter, action)
        makeInitsNode(current.getNext.head, current, starter)
      }
      else {
        if (!backwards) {
          notMissing(starter, last_node.getAfter)
          val Action(name, number, group, state) = last_node.getAfter
          var inits = List(Mcrl2Init(channel_count, name, number, state, current.getName, last.getName))
          channel_count += 1
          for(n <- current.getNext){
            val last = inits.last
            inits ++= makeInitsNode(n, last, starter)
          }
          for(n <- current.getPrev){
            if(n != last_node) {
              val last = inits.last
              inits ++= makeInitsNode(n, last,starter,  true)
            }
          }
          inits
        }
        else{
          notMissing(starter, last_node.getBefore)
          val Action(name, number, group, state) = last_node.getBefore
          var inits = List(Mcrl2Init(channel_count, name, number, state, current.getName, last.getName))
          channel_count += 1
          for(n <- current.getPrev){
            val last = inits.last
            inits ++= makeInitsNode(n, last,starter, true)
          }
          for(n <- current.getNext){
            if(n != last_node) {
              val last = inits.last
              inits ++= makeInitsNode(n, last, starter)
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
