//package preo.modelling
//
//import preo.ast._
//
//
//object Manager {
//
//  var init_count = 1
//  var act_count = 1
//  var channel_count = 1
//
//  def generate_family(con: Connector, typ: Type): Model = ???
//
////  //todo: later
////  def generate_Model(ccon: CoreConnector): Model = ???
//// type is necessary so we know how to instanciate
//  def conToChannels(con: Connector, typ: Type, give_number: Boolean):
//    (List[Action], List[ProcessName], List[Process], List[ProcessName], List[Action]) = con match{
//    case Seq(c1, c2) => val (in1, namesIn1, procs1, namesOut1, out1) = conToChannels(c1, typ, give_number)
//      val (in2, namesIn2, procs2, namesOut2, out2) = conToChannels(c2, typ, give_number)
//
//      val inits: Map[ProcessName, Process] = makeInits(namesOut1, out1, namesIn2, in2, give_number)
//      val replaced1 = namesIn1.map(name => {
//        getRealName(inits, name)
//      })
//      val replaced2 = namesOut2.map(name => {
//        getRealName(inits, name)
//      })
//
//      (in1, replaced1, procs1 ++ procs2 ++ inits.values.toSet.toList, replaced2, out2) //inits1.values == inits2.values
//
//    case Par(c1, c2) => ???
//    case Id(i) => ???
//    case Symmetry(i, j) => ???
//    case Trace(i, c) => ???
//    case p@Prim(_, _, _, _) =>
//      val channel = primToChannel(p)
//      (channel.before, channel.before.map(_ => channel.getName), List(channel), channel.after.map(_ => channel.getName), channel.after)
//    case SubConnector(name, c, _) => ???
//    case Exp(a, c) =>
//
//
//    case ExpX(x, a, c) => ???
//    case Choice(b, c1, c2) => ???
//    case Abs(x, et, c)=> ???
//    case App(c, a) => ???
//  }
//
//  /**
//    * Converts a primitive into (Input Nodes, Channel, Nil, Output Nodes) based on the name of the primitive
//    *
//    * @param prim The primitive to Convert
//    * @return The output mentioned above
//    */
//  def primToChannel(prim: Prim, give_number: Boolean = true): Channel = prim match {
//    case Prim("fifo", _, _, _) =>
//
//      //channel
//      val inAction = Action("fifo", In(1), if(give_number) Some(channel_count) else None)
//      val outAction = Action("fifo", Out(1), if(give_number) Some(channel_count) else None)
//
//      val channel = Channel("Fifo", if(give_number) Some(channel_count) else None, List(inAction), List(outAction),
//        preo.modelling.Seq(inAction, outAction))
//      if(give_number) channel_count += 1
//      channel
//
//    case Prim("fifofull", _, _, _) =>
//      //channel
//      val inAction = Action("fifofull", In(1), if(give_number) Some(channel_count) else None)
//      val outAction = Action("fifofull", Out(1), if(give_number) Some(channel_count) else None)
//
//      val channel = Channel("FifoFull", if(give_number) Some(channel_count) else None, List(inAction), List(outAction),
//        preo.modelling.Seq(outAction, inAction))
//      //updating
//      if(give_number) channel_count += 1
//      channel
//
//    case Prim("lossy", _, _, _) =>
//      //channel
//      val inAction = Action("lossy", In(1), if(give_number) Some(channel_count) else None)
//      val outAction = Action("lossy", Out(1), if(give_number) Some(channel_count) else None)
//
//      val channel = Channel("Lossy", if(give_number) Some(channel_count) else None, List(inAction), List(outAction),
//        preo.modelling.Choice(inAction, MultiAction(inAction, outAction)))
//      //updating
//      if(give_number) channel_count += 1
//      channel
//
//    case Prim("merger", _, _, _) =>
//      //channel
//      val inAction1 = Action("merger", In(1), if(give_number) Some(channel_count) else None)
//      val inAction2 = Action("merger", In(2), if(give_number) Some(channel_count) else None)
//      val outAction = Action("merger", Out(1), if(give_number) Some(channel_count) else None)
//
//      val channel = Channel("Merger", Some(channel_count), List(inAction1, inAction2), List(outAction),
//        preo.modelling.Choice(MultiAction(List(inAction1, outAction)), MultiAction(inAction2, outAction)))
//      //updating
//      if(give_number) channel_count += 1
//      channel
//
//    case Prim("dupl", _, _, _) =>
//      //channel
//      val inAction = Action("dupl", In(1), if(give_number) Some(channel_count) else None)
//      val outAction1 = Action("dupl", Out(1), if(give_number) Some(channel_count) else None)
//      val outAction2 = Action("dupl", Out(2), if(give_number) Some(channel_count) else None)
//
//      val channel = Channel("Dupl", if(give_number) Some(channel_count) else None, List(inAction), List(outAction1, outAction2),
//        MultiAction(List(inAction, outAction1, outAction2)))
//      //updating
//
//      if(give_number) channel_count += 1
//      channel
//
//    case Prim("drain", _, _, _) =>
//      //channel
//      val inAction1 = Action("drain", In(1), if(give_number) Some(channel_count) else None)
//      val inAction2 = Action("drain", In(2), if(give_number) Some(channel_count) else None)
//      val channel = Channel("Drain", if(give_number) Some(channel_count) else None, List(inAction1, inAction2), Nil,
//        MultiAction(List(inAction1, inAction2)))
//      //updating
//
//      if(give_number) channel_count += 1
//      channel
//
//    //todo: either remove the reader and writer making them not do nothing, or create a syncronizing entrance and exit Channel with them
//    //    case CPrim("reader", _, _, _) =>
//    //      val in_node = Mcrl2Node(channel_count, Action.nullAction, Action("reader", var_count,NoLine, Nothing))
//    //      var_count +=1
//    //      (List(in_node), Nil, Nil, Nil)
//    //
//    //    case CPrim("writer", _, _, _) =>
//    //      val out_node = Mcrl2Node(channel_count, Action("writer", channel_count, NoLine, Nothing), Action.nullAction)
//    //
//    //      var_count += 1
//    //      channel_count += 1
//    //      (Nil, Nil, Nil, List(out_node))
//
//    case Prim(name, _, _, _) =>
//      val inAction = Action(name, In(1), if(give_number) Some(channel_count) else None)
//      val outAction = Action(name, Out(1), if(give_number) Some(channel_count) else None)
//      val channel = Channel(number = if(give_number) Some(channel_count) else None, before = List(inAction), after = List(outAction),
//        operator = MultiAction(inAction, outAction))
//
//      if(give_number) channel_count += 1
//      channel
//
//  }
//
//
//
//
//
//}
