package preo.frontend.mcrl2

import preo.ast._

object Util {

  /**
    * Converts a primitive into (Input Nodes, Channel, Nil, Output Nodes) based on the name of the primitive
    *
    * @param prim The primitive to Convert
    * @return The output mentioned above
    */
  def primToChannel(prim: Prim, channel_count: Int): Channel = prim match {
    case Prim("fifo", _, _, _) =>

      //channel
      val inAction = Action("fifo", In(1), Some(channel_count))
      val outAction = Action("fifo", Out(1), Some(channel_count))

      val channel = Channel("Fifo", Some(channel_count), List(inAction), List(outAction),
        preo.frontend.mcrl2.Seq(inAction, outAction))
      channel

    case Prim("fifofull", _, _, _) =>
      //channel
      val inAction = Action("fifofull", In(1), Some(channel_count))
      val outAction = Action("fifofull", Out(1), Some(channel_count))

      val channel = Channel("FifoFull", Some(channel_count), List(inAction), List(outAction),
        preo.frontend.mcrl2.Seq(outAction, inAction))
      //updating
      channel

    case Prim("lossy", _, _, _) =>
      //channel
      val inAction = Action("lossy", In(1), Some(channel_count))
      val outAction = Action("lossy", Out(1), Some(channel_count))

      val channel = Channel("Lossy", Some(channel_count), List(inAction), List(outAction),
        preo.frontend.mcrl2.Choice(inAction, MultiAction(inAction, outAction)))
      //updating
      channel

    case Prim("merger", _, _, _) =>
      //channel
      val inAction1 = Action("merger", In(1), Some(channel_count))
      val inAction2 = Action("merger", In(2), Some(channel_count))
      val outAction = Action("merger", Out(1), Some(channel_count))

      val channel = Channel("Merger", Some(channel_count), List(inAction1, inAction2), List(outAction),
        preo.frontend.mcrl2.Choice(MultiAction(List(inAction1, outAction)), MultiAction(inAction2, outAction)))
      //updating
      channel

    case Prim("dupl", _, _, _) =>
      //channel
      val inAction = Action("dupl", In(1), Some(channel_count))
      val outAction1 = Action("dupl", Out(1), Some(channel_count))
      val outAction2 = Action("dupl", Out(2), Some(channel_count))

      val channel = Channel("Dupl", Some(channel_count), List(inAction), List(outAction1, outAction2),
        MultiAction(List(inAction, outAction1, outAction2)))
      //updating
      channel

    case Prim("drain", _, _, _) =>
      //channel
      val inAction1 = Action("drain", In(1), Some(channel_count))
      val inAction2 = Action("drain", In(2), Some(channel_count))
      val channel = Channel("Drain", Some(channel_count), List(inAction1, inAction2), Nil,
        MultiAction(List(inAction1, inAction2)))
      //updating
      channel

    //todo: either remove the reader and writer making them not do nothing, or create a syncronizing entrance and exit Channel with them
    //    case Prim("reader", _, _, _) =>
    //      val in_node = Mcrl2Node(channel_count, Action.nullAction, Action("reader", var_count,NoLine, Nothing))
    //      var_count +=1
    //      (List(in_node), Nil, Nil, Nil)
    //
    //    case Prim("writer", _, _, _) =>
    //      val out_node = Mcrl2Node(channel_count, Action("writer", channel_count, NoLine, Nothing), Action.nullAction)
    //
    //      var_count += 1
    //      channel_count += 1
    //      (Nil, Nil, Nil, List(out_node))

    case Prim(name, _, _, _) =>
      val inAction = Action(name, In(1), Some(channel_count))
      val outAction = Action(name, Out(1), Some(channel_count))
      val channel = Channel(number = Some(channel_count), before = List(inAction), after = List(outAction),
        operator = MultiAction(inAction, outAction))

      channel

  }

  def makeInits(names1: List[ProcessName], actions1: List[Action], names2: List[ProcessName], actions2: List[Action], counter: Counter)
  : Map[ProcessName, Process] = {
    var map: Map[ProcessName, Process] = Map()

    actions1.zip(actions2).zip(names1.zip(names2)).foreach { case ((a1, a2), (n1, n2)) =>
      val real_name1 = getRealName(map, n1)
      val real_name2 = getRealName(map, n2)
      val init = Init(Some(counter.getInitCount), a1, a2, if(real_name1 != real_name2) List(real_name1, real_name2) else List(real_name1), false)

      map += (real_name1 -> init)
      map += (real_name2 -> init)

      counter.incrementInitCount(1)
    }
    map
  }

  def getRealName(map: Map[ProcessName, Process],name: ProcessName): ProcessName =
    if(map.contains(name)) getRealName(map, map(name).getName)
    else name

  def maximumValue(vars: Set[Var], expr: IExpr, range: Map[Var, List[Int]]): Int = {
    if(vars.isEmpty){
      val res = preo.frontend.Eval(expr)
      res match{
        case IVal(n) => n
      }
    }
    else{
      val v = vars.head
      val rest = vars.tail
      if(range.get(v).isDefined)
        range(v).map(f => maximumValue(rest, preo.frontend.Substitution(v, IVal(f))(expr), range)).max
      else //this should not happen. It's unsafe
        (1 to 10).map(f => maximumValue(rest, preo.frontend.Substitution(v, IVal(f))(expr), range)).max
    }
  }

  def minimumValue(vars: Set[Var], expr: IExpr, range: Map[Var, List[Int]]): Int = {
    if(vars.isEmpty){
      val res = preo.frontend.Eval(expr)
      res match{
        case IVal(n) => n
      }
    }
    else{
      val v = vars.head
      val rest = vars.tail
      if(range.get(v).isDefined)
        range(v).map(f => minimumValue(rest, preo.frontend.Substitution(v, IVal(f))(expr), range)).min
      else //this should not happen. It's unsafe
        (1 to 10).map(f => minimumValue(rest, preo.frontend.Substitution(v, IVal(f))(expr), range)).min
    }
  }


  def makeSyncs(i: Int, counter:Counter): List[Channel] =
    if (i == 0) {
      Nil
    }
    else {
      val inAction = Action("sync", In(1), Some(counter.getChannelCount))
      val outAction = Action("sync", Out(1), Some(counter.getChannelCount))
      val channel = Channel(number = Some(counter.getChannelCount), before = List(inAction), after = List(outAction),
        operator = MultiAction(inAction, outAction))
      counter.incrementChannelCount(1)
      channel :: makeSyncs(i - 1, counter)
    }

  def makeStarterNode(counter:Counter, channel: ProcessName): EntryNode = {
    val action = Action.controllerAction(counter.getSyncCount1)
    val starterNode = EntryNode(counter.getChannelCount, action, channel)
    counter.incrementSyncCount2(1)
    counter.incrementChannelCount(1)
    starterNode
  }


}
