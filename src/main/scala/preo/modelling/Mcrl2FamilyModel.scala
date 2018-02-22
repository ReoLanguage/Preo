package preo.modelling

import preo.ast.{CPar, CSeq, Connector, CoreConnector, CSymmetry, CTrace, CId, CoreInterface, CSubConnector, CPrim}
import preo.frontend._

//todo: use Channel Subs to make better counting system
/**
  * The Mcrl2FamilyModel defines a printable model in mcrl2 for a Connector with several instances
  * @param act the actions of this Model
  * @param proc The processes of this model
  * @param init The init process
  */
class Mcrl2FamilyModel(act: Set[Action], proc: List[Mcrl2Def], init: Mcrl2Process) {
  override def toString: String = {
    val acts = Mcrl2Def.toString(act.toList)
    var procs = ""
    for(p <- proc) procs += s"${p.toString};\n"
    val initProc = Hide(List(Action.nullAction), init)
    s"""
       |act
       |  $acts;
       |proc
       |  $procs
       |init
       |  $initProc;
      """.stripMargin
  }

  /**
    * Creates a string that can be printed in the HTML
    * @return the string formated for HTML
    */
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
  /**
    * Filters the nodes from the processes
    */
  def getNodes: List[Mcrl2Node] = proc.filter(p => p.isInstanceOf[Mcrl2Node]).asInstanceOf[List[Mcrl2Node]]

  /**
    * Filters the Channels from the processes
    */
  def getChannels: List[Mcrl2Channel] = proc.filter(p => p.isInstanceOf[Mcrl2Channel]).asInstanceOf[List[Mcrl2Channel]]

  /**
    * Filters the Inits from the processes
    */
  def getInits: List[Mcrl2Init] = proc.filter(p => p.isInstanceOf[Mcrl2Init]).asInstanceOf[List[Mcrl2Init]]

  /**
    * returns the actions
    */
  def getActions: Set[Action] = act

  /**
    * Filters the Starters from the processes
    */
  def getStarters: List[Mcrl2Starter] = proc.filter(p => p.isInstanceOf[Mcrl2Starter]).asInstanceOf[List[Mcrl2Starter]]


  /**
    * Filters the StarterNodes from the processes
    */
  def getStarterNodes: List[Mcrl2StarterNode] = proc.filter(p => p.isInstanceOf[Mcrl2StarterNode]).asInstanceOf[List[Mcrl2StarterNode]]
}

object Mcrl2FamilyModel{


  var starter_count = 0
  var var_count = 0
  var channel_count = 0
  var last_inits: List[(Int, ProcessName)] = Nil
  var to_check: List[Mcrl2Def] = List[Mcrl2Def]()

  var missingVars: Map[Int, List[Action]] = Map()
  var checkedVars: List[Action] = List()

  var nodes: List[Mcrl2Node] = List[Mcrl2Node]()
  var starterNodes: List[Mcrl2StarterNode] = List[Mcrl2StarterNode]()
  var starters: List[Mcrl2Starter] = List()
  var channels: List[Mcrl2Channel] = List()
  var manager: Mcrl2Manager = Mcrl2Manager(Nil)


  //todo: define block notion
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

      our_nodes = our_nodes ++ nodes
      our_starters = our_starters ++ starterNodes
      last_inits= Nil
      to_check= List[Mcrl2Def]()
      missingVars= Map()
      checkedVars = List()

      nodes= List[Mcrl2Node]()
      starterNodes= List[Mcrl2StarterNode]()

    }
    val family = new Mcrl2FamilyModel(Util.getVars(channels ++ our_nodes ++ inits ++ starters ++ our_starters ++ List(manager) ), channels ++ our_nodes ++ our_starters ++ inits ++ starters ++ List(manager) , starters.last.getName)

    starters= List()
    channel_count = 0
    starter_count = 0
    var_count = 0
    manager= Mcrl2Manager(Nil)
    nodes = List[Mcrl2Node]()
    starterNodes = List[Mcrl2StarterNode]()
    starters = List()
    channels = List()


    family
  }

  /**
    * converts a core connector into a list of definitions
    * Almost like the normal definition in Mcrl2Model, but saves only the nodes, channels and inits
    * instead of creating a Model
    * @param ccon core connector to convert
    * @return the inits
    */
  private def convertCcon(ccon: CoreConnector): List[Mcrl2Def] = {
    val (in_nodes, channels, middle_nodes, out_nodes) = conToChannels(ccon)
//    missingVars = Util.getVars(channels++nodes).toList.filter{case a@Action(name, number, group, state) => !(group == NoLine && state == Nothing) && a != Action.nullAction}
    starterNodes = in_nodes.map(node => makeStarterNode(node))
    nodes = in_nodes ++ middle_nodes ++ out_nodes
    this.channels = this.channels ++ channels
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
  }


  /**
    * converts the core connector into (Input Nodes, Channels, Middle Nodes, Output Nodes)
    * @param ccon the core connector to convert
    * @return the tuple mentioned above
    */
  def conToChannels(ccon: CoreConnector):
  (List[Mcrl2Node], List[Mcrl2Channel], List[Mcrl2Node], List[Mcrl2Node]) = ccon match{
    case CSeq(c1, c2) =>
      val (in1, channel1, nodes1, out1) = conToChannels(c1)
      val (in2, channel2, nodes2, out2) = conToChannels(c2)
      var replacements: Map[String, Mcrl2Node] = Map()
      val nodes = out1.zip(in2).map((n: (Mcrl2Node, Mcrl2Node)) => {
        val result: Mcrl2Node = n._1 ++ n._2
        replacements = replacements + (n._1.getName.toString -> result)
        replacements = replacements + (n._2.getName.toString -> result)
        result
      })
      channel1.foreach(channel => channel.replace(replacements))
      channel2.foreach(channel => channel.replace(replacements))
      (in1,  channel1 ++  channel2, nodes++nodes1++nodes2, out2)

    case CPar(c1, c2) =>
      val (in1, channel1, nodes1, out1) = conToChannels(c1)
      val (in2, channel2, nodes2, out2) = conToChannels(c2)
      (in1 ++ in2, channel1 ++ channel2, nodes1 ++ nodes2, out1 ++ out2)

    case CSymmetry(CoreInterface(i), CoreInterface(j)) =>
      val channels = makeSyncs(i+j)
      var ins: List[Mcrl2Node] = List()
      var outs: List[Mcrl2Node] = List()
      for(channel <- channels){
        val Action(bname, bnumber, _, bstate) = channel.getBefore.head
        val Action(aname, anumber, _, astate) = channel.getAfter.head
        ins = ins ++ List(Mcrl2Node(channel_count, Action.nullAction, Action(bname, bnumber, OneLine, bstate), null, channel))
        outs = outs ++ List(Mcrl2Node(channel_count+1, Action(aname, anumber, OneLine, astate), Action.nullAction, channel , null))
        channel.prev = List(ins.last)
        channel.next = List(outs.last)
        channel_count += 2
      }
      (ins, channels, Nil, outs.drop(i) ++ outs.take(i))

    case CTrace(CoreInterface(i), c) =>
      val (ins, channels,nodes, outs) = conToChannels(c)
      var replacements: Map[String, Mcrl2Node] = Map()
      val new_nodes = outs.takeRight(i).zip(ins.takeRight(i)).map((n: (Mcrl2Node, Mcrl2Node)) => {
        val result: Mcrl2Node = n._1 ++ n._2
        replacements = replacements + (n._1.getName.toString -> result)
        replacements = replacements + (n._2.getName.toString -> result)
        result
      })
      channels.foreach(channel => channel.replace(replacements))
      (ins.dropRight(i), channels , nodes ++ new_nodes, outs.dropRight(i))

    case CId(CoreInterface(i)) =>
      val channels = makeSyncs(i)
      var ins: List[Mcrl2Node] = List()
      var outs: List[Mcrl2Node] = List()
      for(channel <- channels){
        val Action(bname, bnumber, _, bstate) = channel.getBefore.head
        val Action(aname, anumber, _, astate) = channel.getAfter.head
        ins = ins ++ List(Mcrl2Node(channel_count, Action.nullAction, Action(bname, bnumber, OneLine, bstate), null, channel))
        outs = outs ++ List(Mcrl2Node(channel_count+1, Action(aname, anumber, OneLine, astate), Action.nullAction, channel , null))
        channel.prev = List(ins.last)
        channel.next = List(outs.last)
        channel_count += 2
      }
      (ins, channels, Nil , outs)

    case CSubConnector(name, c) =>
      val (in_nodes, channels, middle_nodes, out_nodes) = conToChannels(c)
      var replacements: Map[String, (String, State)] = Map()
      var nodeReplacement: Map[String, Mcrl2Node] = Map()
      var channelReplacement: Map[String, Mcrl2Channel] = Map()
      var count = 0
      replacements = in_nodes.foldRight(replacements)((node, r) =>{count +=1; r + (node.getAfter.identification -> (name, In(count)))})
      count = 0
      replacements = middle_nodes.foldRight(replacements)((node, r) =>{count +=1; val k = r + (node.getBefore.identification -> (name, Middle(count))); count +=1; k + (node.getAfter.identification -> (name, Middle(count)))})
      count = 0
      replacements = out_nodes.foldRight(replacements)((node, r) =>{count +=1; r + (node.getBefore.identification -> (name, Out(count)))})

      val new_in_nodes =in_nodes.map(n => Replacements.replaceActions(n, replacements))
      nodeReplacement = nodeReplacement ++ in_nodes.map(a => a.getName.toString).zip(new_in_nodes)
      val new_channels = channels.map(c =>{channel_count += 1; Replacements.replaceActionsWithName(c, replacements, name, channel_count-1)})

      channelReplacement = channelReplacement ++ channels.map(c => c.getName.toString).zip(new_channels)
      val new_middle_nodes = middle_nodes.map(n => Replacements.replaceActions(n, replacements))
      nodeReplacement = nodeReplacement ++ middle_nodes.map(a => a.getName.toString).zip(new_middle_nodes)
      val new_out_nodes = out_nodes.map(n => Replacements.replaceActions(n, replacements))
      nodeReplacement = nodeReplacement ++ out_nodes.map(a => a.getName.toString).zip(new_out_nodes)

      (new_in_nodes ++ new_middle_nodes ++ new_out_nodes).foreach(n => n.replace(channelReplacement))
      new_channels.foreach(c => c.replace(nodeReplacement))


      (new_in_nodes, new_channels, new_middle_nodes, new_out_nodes)

    case x@CPrim(_, _, _, _) => primToChannel(x)
    case _ => (Nil, Nil, Nil, Nil)
  }

  /**
    * returns the number of syncs requested
    * gets as many as possible from reusable_channels, and then creates new ones
    * @param i number of syncs to return
    * @return the syncs necessary
    */
  private def makeSyncs(i: Int): List[Mcrl2Channel] =
    if (i == 0) {
      Nil
    }
    else {
      val firstAction = Action("sync", channel_count, TwoLine, In1)
      val secondAction = Action("sync", channel_count, TwoLine, Out1)
      val channel = Mcrl2Channel(number = channel_count, before = List(firstAction), after = List(secondAction),
        operator = MultiAction(firstAction, secondAction), prev = List(), next = List())

      channel_count += 1
      channel :: makeSyncs(i - 1)
    }


  /**
    * creates the input and output nodes necessary for a channel
    * @param prim the primitive with the information about the number of inputs and outputs
    * @param number the number identification for the action. if number == -1 then number = channel_count
    * @return (Input Nodes, Output Nodes)
    */
  def primToChannelAux(prim: CPrim, number: Int):
    (List[Mcrl2Node],  List[Mcrl2Node]) = prim match{
    case CPrim(name, CoreInterface(i), CoreInterface(j), _) =>
      val action_number = if (number == -1) channel_count else number

      val in_nodes = (0 until i).toList.map(n => Mcrl2Node(channel_count+n, Action.nullAction, Action(name, action_number, OneLine,if (n == 0) In1 else In2)))
      val out_nodes = (0 until j).toList.map(n => Mcrl2Node(channel_count+i+n, Action(name, action_number, OneLine,if (n == 0) Out1 else Out2) , Action.nullAction))
      channel_count += i + j
      (in_nodes, out_nodes)

    case _ => (Nil, Nil)
  }

  /**
    * Converts a primitive into a channel, using its name to identify the channel
    * @param prim the primitive to convert
    * @return (Input Nodes, Channel, Nil, Output Nodes)
    */
  def primToChannel(prim: CPrim):
  (List[Mcrl2Node], List[Mcrl2Channel], List[Mcrl2Node], List[Mcrl2Node]) = prim match{
    case CPrim("fifo", _ , _, _) =>

      //channel
      val firstAction = Action("fifo",channel_count, TwoLine, In1)
      val secondAction = Action("fifo",channel_count, TwoLine, Out1)

      val channel = Mcrl2Channel("Fifo", channel_count, List(firstAction), List(secondAction),
          Seq(firstAction, secondAction), Nil, Nil)

      channel_count += 1

      val (in_nodes, out_nodes) = primToChannelAux(prim, channel.number)
      channel.prev = in_nodes
      channel.next = out_nodes
      in_nodes.map(node => {node.setNext(channel); null})
      out_nodes.map(node => {node.setPrev(channel); null})
      (in_nodes, List(channel),Nil, out_nodes)

    case CPrim("fifofull", _, _, _) =>
      val firstAction = Action("fifofull",channel_count, TwoLine, In1)
      val secondAction = Action("fifofull",channel_count, TwoLine, Out1)

      val channel = Mcrl2Channel("FifoFull", channel_count, List(firstAction), List(secondAction),
        Seq(secondAction, firstAction), Nil, Nil)

      channel_count += 1

      val (in_nodes, out_nodes) = primToChannelAux(prim, channel.number)
      channel.prev = in_nodes
      channel.next = out_nodes

      in_nodes.map(node => {node.setNext(channel); null})
      out_nodes.map(node => {node.setPrev(channel); null})
      (in_nodes, List(channel),Nil, out_nodes)


    case CPrim("lossy", _, _, _ ) =>
      val firstAction = Action("lossy",channel_count, TwoLine, In1)
      val secondAction = Action("lossy",channel_count, TwoLine, Out1)

      val channel = Mcrl2Channel("Lossy", channel_count, List(firstAction), List(secondAction),
        Choice(firstAction, MultiAction(firstAction, secondAction)), Nil, Nil)

      channel_count += 1

      val (in_nodes, out_nodes) = primToChannelAux(prim, channel.number)
      channel.prev = in_nodes
      channel.next = out_nodes

      in_nodes.map(node => {node.setNext(channel); null})
      out_nodes.map(node => {node.setPrev(channel); null})

      (in_nodes, List(channel), Nil, out_nodes)

    case CPrim("merger", _, _,_) =>
      val firstAction1 = Action("merger",channel_count, TwoLine, In1)
      val firstAction2 = Action("merger",channel_count, TwoLine, In2)
      val secondAction = Action("merger",channel_count, TwoLine, Out1)

      val channel = Mcrl2Channel("Merger", channel_count, List(firstAction1, firstAction2), List(secondAction),
        Choice(MultiAction(List(firstAction1, secondAction)),
          MultiAction(firstAction2, secondAction)), Nil, Nil)

      channel_count += 1

      val (in_nodes, out_nodes) = primToChannelAux(prim, channel.number)
      channel.prev = in_nodes
      channel.next = out_nodes

      in_nodes.map(node => {node.setNext(channel); null})
      out_nodes.map(node => {node.setPrev(channel); null})
      (in_nodes, List(channel), Nil, out_nodes)

    case CPrim("dupl", _, _, _) =>
      val firstAction = Action("dupl",channel_count, TwoLine, In1)
      val secondAction1 = Action("dupl",channel_count, TwoLine, Out1)
      val secondAction2 = Action("dupl",channel_count, TwoLine, Out2)

      val channel = Mcrl2Channel("Dupl", channel_count, List(firstAction), List(secondAction1, secondAction2),
        MultiAction(List(firstAction, secondAction1, secondAction2)), Nil, Nil)

      channel_count += 1

      val (in_nodes, out_nodes) = primToChannelAux(prim, channel.number)
      channel.prev = in_nodes
      channel.next = out_nodes

      in_nodes.map(node => {node.setNext(channel); null})
      out_nodes.map(node => {node.setPrev(channel); null})
      (in_nodes, List(channel), Nil, out_nodes)

    case CPrim("drain", _, _, _) =>
      val firstAction1 = Action("drain",channel_count, TwoLine, In1)
      val firstAction2 = Action("drain",channel_count, TwoLine, In2)

      val channel = Mcrl2Channel("Drain", channel_count, List(firstAction1, firstAction2),Nil,
        MultiAction(List(firstAction1, firstAction2)), Nil, Nil)

      channel_count += 1

      val (in_nodes, out_nodes) = primToChannelAux(prim, channel.number)
      channel.prev = in_nodes
      channel.next = out_nodes

      in_nodes.map(node => {node.setNext(channel); null})
      out_nodes.map(node => {node.setPrev(channel); null})
      (in_nodes, List(channel), Nil, out_nodes)


    case CPrim("reader", _, _, _) =>
      val in_node = Mcrl2Node(channel_count, Action.nullAction, Action("reader", var_count,NoLine, Nothing))
      var_count +=1
      (List(in_node), Nil, Nil, Nil)


    case CPrim("writer", _, _, _) =>
      val out_node = Mcrl2Node(channel_count, Action("writer", channel_count, NoLine, Nothing), Action.nullAction)

      var_count += 1
      channel_count += 1
      (Nil, Nil, Nil, List(out_node))

    case CPrim(name, _, _, _) =>
      val firstAction = Action(name,channel_count, TwoLine, In1)
      val secondAction = Action(name,channel_count, TwoLine, Out1)

      val channel = Mcrl2Channel(name.head.toUpper.toString ++ name.tail, channel_count, List(firstAction), List(secondAction),
          MultiAction(firstAction, secondAction), Nil, Nil)

      channel_count += 1

      val (in_nodes, out_nodes) = primToChannelAux(prim, channel.number)
      channel.prev = in_nodes
      channel.next = out_nodes
      in_nodes.map(node => {node.setNext(channel); null})
      out_nodes.map(node => {node.setPrev(channel); null})
      (in_nodes, List(channel),Nil, out_nodes)
  }

  /**
    * Creates a StarterNode for the given node
    * @param node a node with before = Null
    * @return the starterNode
    */
  private def makeStarterNode(node: Mcrl2Node): Mcrl2StarterNode = {
    val s = Mcrl2StarterNode(this.starter_count, node)
    this.starter_count += 1
    s
  }

  /**
    * Creates the inits following the graph with the starterNodes
    * @param starters the starterNodes where we begin crossing the graph
    * @return the inits
    */
  private def initsMaker(starters: List[Mcrl2StarterNode]): List[Mcrl2Init] =
    if(starters.nonEmpty){
      var inits = makeInitsStarterNode(starters.head)
      if(missingVars.get(starters.head.number).isDefined && missingVars(starters.head.number).nonEmpty){
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

  /**
    * removes a channel or node from the to_check list
    * this list is usefull to know if we have travelled through all the nodes and channels
    * @param element the element to remove from the list
  */
  private def check(element: Mcrl2Def): Unit = to_check = to_check.filter(x => x != element)

  /**
    * removes an action from the missingVars list
    * this list is usefull to know if we have used all the vars in the init processes
    * @param action the action to remove
    */
  private def notMissing(number: Int, action: Action): Unit ={
    val value = missingVars.get(number)
    if(value.isDefined){
      missingVars = missingVars updated (number, value.get.filter(x=> ! x.sameType(action)))
    }
    checkedVars = checkedVars ++ List(action)
  }

  /**
    * Places an action in the missingVars of the starter number given
    * @param starter the starter node identification
    * @param action the action to place in the missingVars
    */
  private def isMissing(starter: Int, action: Action): Unit = {
    val value = missingVars.get(starter)
    if(value.isDefined){
      missingVars = missingVars updated (starter, action :: value.get)
    }
    else{
      missingVars = missingVars updated (starter, List(action))
    }
  }

  /**
    * The blockers are inits that we create when all the processes have been used but some vars are still
    * left out from the inits
    * @param actions the remaining actions
    * @param last the last init process we created
    * @return a list of inits
    */
  private def makeblockers(actions: List[Action], last: Mcrl2Process): List[Mcrl2Init] = actions match{
    case Action(name, number, _, state) :: rest =>
      val filtered_rest = rest.filter{case Action(_, n, g,s) => n != number || s != state}
      val m = Mcrl2Init(channel_count, name, number,state, last)
      channel_count += 1
      m :: makeblockers(filtered_rest, m.getName)

    case Nil => Nil
  }

  /**
    * Creates an init with the starterNode and instead of jumping to the node used in the starterNode it jumps to
    * the channel of the node
    * @param current current starter Node
    * @return a list of inits
    */
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

  /**
    * Creates an init process for the node received
    * @param current The current node we need to put in an init
    * @param last the last init created
    * @param starter the starterNode identification
    * @param backwards are we going backwards in the graph?
    * @return a list of inits
    */
  private def makeInitsNode(current: Mcrl2Node, last: Mcrl2Def, starter: Int, backwards: Boolean = false): List[Mcrl2Init] = {
    if(to_check.contains(current)) {
      //in this case we know the node doesn't have prev
      //we can also assure that in this case backwards = false

      check(current)
      if (last == null) {
        if(current.getBefore != Action.nullAction && current.getBefore.group != NoLine && current.getBefore.state != Nothing)
          isMissing(starter, current.getBefore)
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

  /**
    * Creates an init process for the channel received
    * @param current The current channel we need to put in an init
    * @param last the last init created
    * @param starter the starterNode identification
    * @param backwards are we going backwards in the graph?
    * @return a list of inits
    */
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
          (current.getBefore ++ current.getAfter).foreach{
            case a@Action(_, _, gr, st) =>
              if(!(checkedVars.exists(x => x.sameType(a)) || a.sameType(last_node.getAfter) || a == Action.nullAction || (gr == NoLine && st == Nothing) )){
                isMissing(starter, a)
              }
          }
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
          (current.getAfter ++ current.getBefore).foreach{
            case a@Action(_, _, _, _) =>
              if(! (checkedVars.exists(x => x.sameType(a)) || a.sameType(last_node.getBefore)|| a == Action.nullAction || (group == NoLine && state == Nothing) )){
                isMissing(starter, a)
              }
          }
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

  //for usage in testing

  def testApply(con: Connector): List[CoreConnector] = {
    // get applied connectors
    val instances = Eval.getInstances(con)
    //for every value we create an app with the vars in the instances and turn it into a core connector
    val connectors = instances.map(x => Eval.simpleReduce(x))
    connectors
  }

  def restApply(connectors: List[CoreConnector]): Mcrl2FamilyModel = {
    starters= List()
    channel_count = 0
    starter_count = 0
    var_count = 0
    channels= List()
    manager= Mcrl2Manager(Nil)

    last_inits = Nil
    to_check = List[Mcrl2Def]()
    missingVars = Map()
    checkedVars = List()

    nodes = List[Mcrl2Node]()
    starterNodes = List[Mcrl2StarterNode]()
    starters = List()


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

      our_nodes = our_nodes ++ nodes
      our_starters = our_starters ++ starterNodes
      last_inits= Nil
      to_check= List[Mcrl2Def]()
      missingVars= Map()
      checkedVars = List()

      nodes= List[Mcrl2Node]()
      starterNodes= List[Mcrl2StarterNode]()

    }
    val family = new Mcrl2FamilyModel(Util.getVars(channels ++ our_nodes ++ inits ++ starters ++ our_starters ++ List(manager) ), channels ++ our_nodes ++ our_starters ++ inits ++ starters ++ List(manager) , starters.last.getName)

    starters= List()
    channel_count = 0
    starter_count = 0
    var_count = 0
    channels= List()
    manager= Mcrl2Manager(Nil)

    last_inits = Nil
    to_check = List[Mcrl2Def]()
    missingVars = Map()

    nodes = List[Mcrl2Node]()
    starterNodes = List[Mcrl2StarterNode]()
    starters = List()


    family
  }

}
