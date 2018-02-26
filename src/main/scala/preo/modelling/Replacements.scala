package preo.modelling

/**
  * Object contains the necessary functions when you want to replace actions and nodes in channels and processes
  */
object Replacements {

  /**
    * Replaces the given action with a new action if the name and number match
    * @param a action to replace
    * @param replacements the actions we need replaced
    * @return the new action if the name was in the map, or the same action, otherwise
    */
  def replaceAct(a: Action, replacements: Map[String, (String, State)]): Action = a match{
    case Action(na, nu, group, state) =>
      if (replacements.get(a.identification).isDefined) Action(replacements(a.identification)._1, nu, group, replacements(a.identification)._2)
      else a
  }


  /**
    * Replaces the actions in the given process with the respective replacements
    * @param process the process whose actions need to be replaced
    * @param replacements the mapping of the current actions and respective replacements
    * @return a new process with actions changed
    */
  def replaceActions(process: Mcrl2Process, replacements: Map[String, (String, State)]): Mcrl2Process = process match {
    case a@Action(name, number, group, state) => replaceAct(a, replacements)
    case p@ProcessName(name) => p
    case MultiAction(actions) => MultiAction(actions.map(a => replaceAct(a, replacements)))
    case Seq(before, after) => Seq(replaceActions(before, replacements), replaceActions(after, replacements))
    case Choice(left, right) => Choice(replaceActions(left, replacements), replaceActions(right, replacements))
    case Par(left, right) => Par(replaceActions(left, replacements), replaceActions(right, replacements))
    case Comm(actions, in) => Comm((replaceAct(actions._1, replacements), replaceAct(actions._2, replacements), replaceAct(actions._3, replacements)), replaceActions(in, replacements))
    case Allow(actions, in) => Allow(actions.map(a => replaceAct(a, replacements)), replaceActions(in, replacements))
    case Block(actions, in) => Block(actions.map(a => replaceAct(a, replacements)), replaceActions(in, replacements))
    case Hide(actions, in) => Hide(actions.map(a => replaceAct(a, replacements)), replaceActions(in, replacements))
    case _ => null
  }

  /**
    * Replaces the actions in the channel
    * @param channel the channels whose actions we need to replace
    * @param replacements the mapping of the current actions and their replacements
    * @return a new channel with the actions replaced
    */
  def replaceActions(channel: Mcrl2Channel, replacements: Map[String, (String, State)]) : Mcrl2Channel = {
    val new_befores = channel.before.map(a => replaceAct(a, replacements))
    val new_afters = channel.after.map(a => replaceAct(a, replacements))
    Mcrl2Channel(channel.name, channel.number, new_befores, new_afters, replaceActions(channel.operator, replacements), channel.prev, channel.next)
  }


  /**
    * Replaces the actions in the node
    * @param node the channels whose actions we need to replace
    * @param replacements the mapping of the current actions and their replacements
    * @return a new node with the actions replaced
    */
  def replaceActions(node: Mcrl2Node, replacements: Map[String, (String, State)]) : Mcrl2Node = node match {
    case Mcrl2Node(number, before, after, prev, next) => Mcrl2Node(number, replaceAct(before, replacements), replaceAct(after, replacements), prev, next)
  }

  /**
    * Replaces the actions in the channel and changes its name and number
    * @param channel the channels whose actions we need to replace
    * @param replacements the mapping of the current actions and their replacements
    * @param new_name the new name of the channel
    * @param new_number the new number of the channel
    * @return a new channel with the actions and names replaced
    */
  def replaceActionsWithName(channel: Mcrl2Channel, replacements: Map[String, (String, State)], new_name:String, new_number: Int) : Mcrl2Channel = {
    val new_befores = channel.before.map(a => replaceAct(a, replacements))
    val new_afters = channel.after.map(a => replaceAct(a, replacements))
    Mcrl2Channel(new_name, new_number, new_befores, new_afters, replaceActions(channel.operator, replacements), channel.prev, channel.next)
  }

}
