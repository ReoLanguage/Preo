package preo.modelling

abstract class Mcrl2Process{
  def vars: List[Action]
}

class Group

case object NoLine extends Group
case object OneLine extends Group
case object TwoLine extends Group


class State

case object In1 extends State
case object In2 extends State
case object Out1 extends State
case object Out2 extends State
case object Nothing extends State


/**
  * An action is an atomic element of mcrl2
  * @param name name of the action
  * @param number identification number
  * @param group the group defines how many lines the action has when it is converted to a string
  * @param state the state defines if it is an input or output action, or neither
  */
case class Action(name: String, number: Int, group: Group, state: State) extends Mcrl2Process{
  //group 1 is for actions of the channels
  //group 2 is for actions of nodes
  //group 3 is for actions of comunication
  override def toString: String = {
    val group_name = group match {
      case TwoLine => "''"
      case OneLine => "'"
      case NoLine => ""
    }
    val state_name = state match{
      case In1 => "in1"
      case In2 => "in2"
      case Out1 => "out1"
      case Out2 => "out2"
      case Nothing => ""
    }
    s"$name${if (number!= -1) number else ""}$state_name$group_name"
  }

  override def equals(o: scala.Any): Boolean =
    if (o == null)
      false
    else if(o.getClass != this.getClass)
      false
    else
      this.number == o.asInstanceOf[Action].get_number && this.group == o.asInstanceOf[Action].group && this.name ==o.asInstanceOf[Action].name && this.state == o.asInstanceOf[Action].state

  def get_number: Int = number

  override def vars: List[Action] = List(this)

}

object Action{
  def apply(number: Int, group: Group): Action = new Action("X", number, group, Nothing)

  /**
    * An action that will print Null
    */
  def nullAction: Action = new Action("Null", -1, NoLine, Nothing)
}

/**
  * A process name is just a name of a process (usefull when we have a Mcrl2Process aglomeration)
  * @param name the name of the process this represents
  */
case class ProcessName(name: String) extends Mcrl2Process{
  override def vars: List[Action] = Nil

  override def toString: String = name
}

/**
  * A multiaction defines the operator for multiactions
  * @param actions the actions that will be used in the multiaction
  */
case class MultiAction(actions: List[Action]) extends Mcrl2Process{
  override def toString: String = toString(actions)

  private def toString(actions: List[Action]): String = actions match {
    case x :: y :: rest => x.toString + " | " + toString(y :: rest)
    case x :: Nil => x.toString
    case Nil => "Null"
  }

  def getHead: Action = if(actions.nonEmpty) actions.head else null

  def getLast: Action = if(actions.nonEmpty) actions.last else null

  override def vars: List[Action] = if(actions.isEmpty) List(Action.nullAction) else actions

}

object MultiAction{
  def apply(actions: Action*) = new MultiAction(actions.toList)
}

/**
  * the sequence operator in mcrl2 (before . after) when printed
  * @param before before process
  * @param after after process
  */
case class Seq(before: Mcrl2Process, after: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"(${before.toString}) . (${after.toString})"

  override def vars: List[Action] = before.vars ++ after.vars
}


/**
  * the choice operator in mcrl2 (left + right) when printed
  * @param left left process
  * @param right right process
  */
case class Choice(left: Mcrl2Process, right: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"(${left.toString}) + (${right.toString})"

  override def vars: List[Action] = left.vars ++ right.vars
}


/**
  * the paralel operator in mcrl2 (left || right) when printed
  * @param left left process
  * @param right right process
  */
case class Par(left: Mcrl2Process, right:Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"(${left.toString}) || (${right.toString})"

  override def vars: List[Action] = left.vars ++ right.vars
}

/**
  * creates a communication operator with the 3 actions in the tuple in the process
  * @param actions the actions to communicate
  * @param in the process where the communication will happen
  */
case class Comm(actions: (Action, Action, Action), in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""comm({${toString(actions)}}, ${in.toString})"""

  def toString(vars: (Action, Action, Action)): String = s"""${vars._1.toString}|${vars._2.toString} -> ${vars._3.toString}"""

  override def vars: List[Action] = actions._1 :: in.vars
}

/**
  * The allow operator in mcrl2
  * @param actions actions allowed
  * @param in process where the actions are allowed
  */
case class Allow(actions: List[Action], in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""allow({${Mcrl2Def.toString(actions)}}, ${in.toString})"""

  override def vars: List[Action] = in.vars
}

/**
  * the block operator in mcrl2
  * @param actions the blocked actions
  * @param in the process where the actions are blocked
  */
case class Block(actions: List[Action], in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""block({${Mcrl2Def.toString(actions)}}, ${in.toString})"""

  override def vars: List[Action] = in.vars
}

/**
  * the hide operator in mcrl2
  * @param actions the actions to be hidden
  * @param in the process where the actions are hidden
  */
case class Hide(actions: List[Action], in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""hide({${Mcrl2Def.toString(actions)}}, ${in.toString})"""

  override def vars: List[Action] = in.vars
}