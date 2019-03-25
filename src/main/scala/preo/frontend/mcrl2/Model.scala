package preo.frontend.mcrl2

import preo.ast._
import preo.common.GenerationException
import preo.frontend.mcrl2

import scala.collection.mutable



class Model(val procs: List[Process],val init: ProcessExpr) {
  override def toString: String = {
    val actions: String = toString(procs.flatMap(p => p.getActions))
    var processes: String = ""
    for(p <- procs) processes += s"${p.toString};\n"
    val initProc = init
    s"""
       |act
       |  $actions;
       |proc
       |  $processes
       |init
       |  $initProc;
      """.stripMargin
  }

  /**
    * Creates a string that can be printed in the HTML
    * @return the string formated for HTML
    */
  def webString: String = {
    val actions: String = toString(procs.flatMap(p => p.getActions))
    var processes = ""
    for(p <- procs) processes += s"${p.toString};<br>\n"
    val initProc = init
    s"""
       |act <br>
       |  $actions;<br>
       |  <br>
       |proc<br>
       |<br>
       |  $processes<br>
       |init<br>
       |<br>
       |  $initProc;<br>
      """.stripMargin
  }

  private def toString(act: List[Action]): String = 
    act.mkString(", ")

  def getChannels: List[Process] = procs.filter(p => p.isInstanceOf[Channel])

  def getInits: List[Process] = procs.filter(p => p.isInstanceOf[Init])

  def getActions: Set[Action] = procs.flatMap(p => p.getActions).toSet

  def getMultiActions: List[Set[Action]] = {
    val procs_map = procs.foldRight(Map(): Map[String, Process])((p, m) => m.updated(p.getName.toString, p))
    getMultiActions(init, procs_map).filter(s => s.nonEmpty)
  }

  private def getMultiActions(e: ProcessExpr, procs_map: Map[String, Process]): List[Set[Action]] = e match{
    case a@Action(_, _, _) => List(Set(a))
    case MultiAction(actions) => List(actions.toSet)
    case ProcessName(name) => getMultiActions(procs_map(name).getOperation, procs_map)
    case mcrl2.Seq(before, after) => getMultiActions(before, procs_map) ++ getMultiActions(after, procs_map)
    case mcrl2.Choice(left, right) => getMultiActions(left, procs_map) ++ getMultiActions(right, procs_map)
    case mcrl2.Par(left, right) => {
      val mleft = getMultiActions(left, procs_map)
      val mright = getMultiActions(right, procs_map)
      mleft ++ mright ++ mleft.flatMap(ml => mright.map(mr => ml ++ mr))
    }
    case Comm(syncActions, resultingAction, expr) => {
      val m = getMultiActions(expr, procs_map)
      val syncActionsSet = syncActions.toSet
      m.map(f =>
        if(syncActionsSet.subsetOf(f)) (f -- syncActionsSet) ++ Set(resultingAction)
        else f
      )
    }
    case Block(actions, expr) => getMultiActions(expr, procs_map).filter(f => f.intersect(actions.toSet).isEmpty)
    case Hide(actions, expr)  => getMultiActions(expr, procs_map).map(f => f -- actions.toSet)
  }

  /**
    * For each base name (e.g., "lossy"), return the set of all multiactions where it can occur
    * @return
    */
  def getMultiActionsMap: mutable.Map[String,Set[Set[Action]]] = {
    val res = mutable.Map[String,Set[Set[Action]]]()
    for (ma <- getMultiActions) {
      mergeMAs(res,getMultiActionsMap(ma))
    }
    res
  }

  /**
    * For a given multiaction, maps all base names to the given multiaction.
    * @param ma given multiaction (set of actions)
    * @return
    */
  private def getMultiActionsMap(ma: Set[Action]): Map[String,Set[Action]] = {
    var res = Map[String,Set[Action]]()
    for (a <- ma) {
      var prev = List[String]()
      for (subname <- a.name.split("_")) {
        if (subname.matches("[0-9]+[imo][0-9]+"))
          prev = List()
        else {
          prev ::= subname
          res += (prev.reverse.mkString("_") -> ma)
        }
      }
    }

    res
  }

  private def mergeMAs(m: mutable.Map[String, Set[Set[Action]]], newnames: Map[String, Set[Action]]): Unit = {
    for ((n,ma) <- newnames) {
      m(n) = m.getOrElse(n,Set()) + ma
    }
  }

}


object Model {

  var init_count = 1
  var channel_count = 1

  /**
    * Converts the CoreConnector into an instance of the Mcrl2Model
    *
    * @param realccon the coreConnector to convert
    * @return the converted Mcrl2Model
    */
  def apply(realccon: CoreConnector): Model = {
    //remove top subconnector if there is one
    val ccon = realccon match {
      case CSubConnector(name, c1, a) if name == "" => c1
      case c => c
    }
    val (ins, names_in, procs, names_out, outs) = conToChannels(ccon)

    val inits = (names_in ++ names_out).toSet
    val init: ProcessExpr =
      if(inits.isEmpty) ProcessName("delta")
      else
        inits.tail.foldRight(inits.head.asInstanceOf[ProcessExpr])((a, b) => preo.frontend.mcrl2.Par(a, b))

    init_count = 1
    channel_count = 1

    new Model(procs, init)
  }

  /**
    * Convertes a CoreConnector into (Input Nodes, Channels, Middle Nodes, Output Nodes)
    *
    * @param ccon The CoreConnector to Convert
    * @return the output mentioned above
    */
  def conToChannels(ccon: CoreConnector):
  (List[Action], List[ProcessName], List[Process], List[ProcessName], List[Action]) = ccon match {
    case CSeq(c1, c2) =>
      val (in1, namesIn1, procs1, namesOut1, out1) = conToChannels(c1)
      val (in2, namesIn2, procs2, namesOut2, out2) = conToChannels(c2)

      val inits: Map[ProcessName, Process] = makeInits(namesOut1, out1, namesIn2, in2)
      val replaced1 = namesIn1.map(name => {
        getRealName(inits, name)
      })
      val replaced2 = namesOut2.map(name => {
        getRealName(inits, name)
      })

      (in1, replaced1, procs1 ++ procs2 ++ inits.values.toSet.toList, replaced2, out2) //inits1.values == inits2.values

    case CPar(c1, c2) =>
      val (in1, namesIn1, procs1, namesOut1, out1) = conToChannels(c1)
      val (in2, namesIn2, procs2, namesOut2, out2) = conToChannels(c2)
      (in1 ++ in2, namesIn1 ++ namesIn2, procs1 ++ procs2, namesOut1 ++ namesOut2, out1 ++ out2)

    case CSymmetry(CoreInterface(i), CoreInterface(j)) =>
      val channels = makeSyncs(i + j)
      val ins: List[Action] = channels.flatMap(f => f.in)
      val outs: List[Action] = channels.flatMap(f => f.out)
      val namesIn: List[ProcessName] = channels.flatMap(f => f.in.map(_ => f.getName))
      val namesOut: List[ProcessName] = channels.flatMap(f => f.out.map(_ => f.getName))

      (ins, namesIn, channels, namesOut.drop(i) ++ namesOut.take(i), outs.drop(i) ++ outs.take(i))

    case CTrace(CoreInterface(i), c) =>
      val (in, namesIn, procs, namesOut, out) = conToChannels(c)

      val inits: Map[ProcessName, Process] = makeInits(namesOut.takeRight(i), out.takeRight(i), namesIn.takeRight(i), in.takeRight(i))
      val replaced1 = namesIn.dropRight(i).map(name => {
        getRealName(inits, name)
      })
      val replaced2 = namesOut.dropRight(i).map(name => {
        getRealName(inits, name)
      })

      (in.dropRight(i), replaced1, procs ++ inits.values.toSet.toList, replaced2, out.dropRight(i))

    case CId(CoreInterface(i)) =>
      val channels = makeSyncs(i)
      val ins: List[Action]           = channels.flatMap(f => f.in)
      val outs: List[Action]          = channels.flatMap(f => f.out)
      val namesIn: List[ProcessName]  = channels.flatMap(f => f.in.map(_ => f.getName))
      val namesOut: List[ProcessName] = channels.flatMap(f => f.out.map(_ => f.getName))

      (ins, namesIn, channels, namesOut, outs)

    case CSubConnector(name, c, anns) =>
      val toHide = Annotation.hidden(anns)
      val c2 = if (toHide)  hideAllSubConn(c) else c

      val (in, namesIn, procs, namesOut, out) = conToChannels(c2)

      var count = 1
      for (proc <- procs; a <- proc.getActions if a.state != Sync) {
        a.state = Middle(count)
        a.name  = if (toHide) name else name+"_"+a.name
        count  += 1
      }
      count = 1
      //no action of the form "p[0..9]+sync" with state Sync should have a name change
      for (a <- in if a.state != Sync) {
        a.state = In(count)
//        a.name  = name+"/"+a.name
        count  += 1
      }
      count = 1
      //no action of the form "p[0..9]+sync" with state Sync should have a name change
      for (a <- out if a.state != Sync) {
        a.state = Out(count)
//        a.name  = name+"/"+a.name
        count  += 1
      }
      if (toHide)
        procs.foreach {
          case i:Init =>
            i.toHide = true // TODO: CHECK if it makes sense (seems ok now)
          case x      =>
        }
      (in, namesIn, procs, namesOut, out)

    case x@CPrim(_, _, _, _) =>
      val channel = primToChannel(x)
      (channel.in, channel.in.map(_ => channel.getName), List(channel), channel.out.map(_ => channel.getName), channel.out)

    case _ => (Nil, Nil, Nil, Nil, Nil)
  }

  /**
    * Adds a "hide" annotation to all children (recursively)
    * @param connector
    * @return new connector
    */
  def hideAllSubConn(connector: CoreConnector) =
    CoreConnector.visit(connector,{
      case CSubConnector(name, c, anns) => CSubConnector(name,c,Annotation("hide",None)::anns)
    })

  /**
    * Hide all sub connectors not in the given list of prefixes, and unhides the remaining ones.
    * @param conn connector to be updated
    * @param conts prefixes that should be exposed
    * @return updated connector with different 'hide' annotations
    */
  def unhideUntilPrefix(conn: CoreConnector, conts: List[List[Container]]): CoreConnector = conn match {
    case CSeq(c1, c2) => CSeq(unhideUntilPrefix(c1,conts),unhideUntilPrefix(c2,conts))
    case CPar(c1, c2) => CPar(unhideUntilPrefix(c1,conts),unhideUntilPrefix(c2,conts))
    case CTrace(i,c) => CTrace(i,unhideUntilPrefix(c,conts))
    case CSubConnector(name, c, anns) =>
      if (conts.exists(_.headOption.contains(Container(name))))
        CSubConnector(name,unhideUntilPrefix(c,enterPrefix(name,conts)),rmHide(anns))
      else
        hideAllSubConn(conn)
    case _ => conn
  }

  private def rmHide(annotations: List[Annotation]): List[Annotation] =
    annotations.filter(_.name.toLowerCase != "hide")
  private def addHide(annotations: List[Annotation]): List[Annotation] =
    Annotation("hide",None) :: annotations
  private def enterPrefix(str: String, list: List[List[Container]]): List[List[Container]] =
    for (cts <- list if cts.headOption.contains(str)) yield cts.tail


  /**
    * Converts a primitive into (Input Nodes, Channel, Nil, Output Nodes) based on the name of the primitive
    *
    * @param prim The primitive to Convert
    * @return The output mentioned above
    */
  def primToChannel(prim: CPrim): Channel = prim match {
    case CPrim("fifo", _, _, _) =>

      //channel
      val inAction = Action("fifo", In(1), Some(channel_count))
      val outAction = Action("fifo", Out(1), Some(channel_count))

      val channel = Channel("Fifo", Some(channel_count), List(inAction), List(outAction),
        preo.frontend.mcrl2.Seq(inAction, outAction))
      channel_count += 1
      channel

    case CPrim("fifofull", _, _, _) =>
      //channel
      val inAction = Action("fifofull", In(1), Some(channel_count))
      val outAction = Action("fifofull", Out(1), Some(channel_count))

      val channel = Channel("FifoFull", Some(channel_count), List(inAction), List(outAction),
        preo.frontend.mcrl2.Seq(outAction, inAction))
      //updating
      channel_count += 1
      channel

    case CPrim("lossy", _, _, _) =>
      //channel
      val inAction = Action("lossy", In(1), Some(channel_count))
      val outAction = Action("lossy", Out(1), Some(channel_count))

      val channel = Channel("Lossy", Some(channel_count), List(inAction), List(outAction),
        preo.frontend.mcrl2.Choice(inAction, MultiAction(inAction, outAction)))
      //updating
      channel_count += 1
      channel

    case CPrim("merger", _, _, _) =>
      //channel
      val inAction1 = Action("merger", In(1), Some(channel_count))
      val inAction2 = Action("merger", In(2), Some(channel_count))
      val outAction = Action("merger", Out(1), Some(channel_count))

      val channel = Channel("Merger", Some(channel_count), List(inAction1, inAction2), List(outAction),
        preo.frontend.mcrl2.Choice(MultiAction(List(inAction1, outAction)), MultiAction(inAction2, outAction)))
      //updating
      channel_count += 1
      channel

    case CPrim("dupl", _, _, _) =>
      //channel
      val inAction = Action("dupl", In(1), Some(channel_count))
      val outAction1 = Action("dupl", Out(1), Some(channel_count))
      val outAction2 = Action("dupl", Out(2), Some(channel_count))

      val channel = Channel("Dupl", Some(channel_count), List(inAction), List(outAction1, outAction2),
        MultiAction(List(inAction, outAction1, outAction2)))
      //updating

      channel_count += 1
      channel

    case CPrim("drain", _, _, _) =>
      //channel
      val inAction1 = Action("drain", In(1), Some(channel_count))
      val inAction2 = Action("drain", In(2), Some(channel_count))
      val channel = Channel("Drain", Some(channel_count), List(inAction1, inAction2), Nil,
        MultiAction(List(inAction1, inAction2)))
      //updating

      channel_count += 1
      channel

    //todo: either remove the reader and writer making them not do nothing, or create a syncronizing entrance and exit Channel with them
    //    case CPrim("reader", _, _, _) =>
    //      val in_node = Mcrl2Node(channel_count, Action.nullAction, Action("reader", var_count,NoLine, Nothing))
    //      var_count +=1
    //      (List(in_node), Nil, Nil, Nil)
    //
    //    case CPrim("writer", _, _, _) =>
    //      val out_node = Mcrl2Node(channel_count, Action("writer", channel_count, NoLine, Nothing), Action.nullAction)
    //
    //      var_count += 1
    //      channel_count += 1
    //      (Nil, Nil, Nil, List(out_node))

    case CPrim(name, CoreInterface(1), CoreInterface(1), _) =>
      val inAction = Action(name, In(1), Some(channel_count))
      val outAction = Action(name, Out(1), Some(channel_count))
      val channel = Channel(number = Some(channel_count), in = List(inAction), out = List(outAction),
        expression = MultiAction(inAction, outAction))

      channel_count += 1
      channel

    case _ =>
      throw new GenerationException(s"Unknown connector ${prim.name}.")

  }


  /**
    * Makes some sync channels
    *
    * @param i the number of channels to make
    * @return the list of channels created
    */
  private def makeSyncs(i: Int): List[Channel] =
    if (i == 0) {
      Nil
    }
    else {
      val inAction = Action("sync", In(1), Some(channel_count))
      val outAction = Action("sync", Out(1), Some(channel_count))
      val channel = Channel(number = Some(channel_count), in = List(inAction), out = List(outAction),
        expression = MultiAction(inAction, outAction))
      channel_count += 1
      channel :: makeSyncs(i - 1)
    }


  private def makeInits(names1: List[ProcessName], actions1: List[Action], names2: List[ProcessName], actions2: List[Action])
  : Map[ProcessName, Process] = {
    var map: Map[ProcessName, Process] = Map()

    actions1.zip(actions2).zip(names1.zip(names2)).foreach { case ((a1, a2), (n1, n2)) =>
      val real_name1 = getRealName(map, n1)
      val real_name2 = getRealName(map, n2)
      val init = Init(Some(init_count), a1, a2, if(real_name1 != real_name2) List(real_name1, real_name2) else List(real_name1), false)

      map += (real_name1 -> init)
      map += (real_name2 -> init)

      init_count += 1
    }
    map
  }

  private def getRealName(map: Map[ProcessName, Process],name: ProcessName): ProcessName =
    if(map.contains(name)) getRealName(map, map(name).getName)
    else name

}