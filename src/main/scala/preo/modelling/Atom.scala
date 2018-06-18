package preo.modelling

abstract class Atom extends Operation//{
//  override def toString: String
//
//  def getActions: Set[Action]
//}

abstract class Group{
  def coGroup: Group
}

case object NoLine extends Group{
  def coGroup: Group = NoLine
}
case object OneLine extends Group{
  def coGroup: Group = TwoLine
}
case object TwoLine extends Group {
  def coGroup: Group = OneLine
}

class State

case class In(n: Int) extends State
case class Middle(n: Int) extends State
case class Out(n: Int) extends State
case object Nothing extends State


/**
  * An action is an atomic element of mcrl2
  * @param name name of the action
  * @param group the group defines how many lines the action has when it is converted to a string
  * @param state the state defines if it is an input or output action, or neither
  * @param number identification number, or None in case of families
  */
case class Action(name: String, group: Group, state: State, number: Option[Int] = None) extends Atom{

  def getActions: Set[Action] = Set(this)

  override def toString: String = {
    val group_name = group match {
      case TwoLine => "''"
      case OneLine => "'"
      case NoLine => ""
    }
    val state_name = state match{
      case Nothing => ""
      case Middle(n) => "mid" + n.toString
      case In(n) => "in" + n.toString
      case Out(n) => "out" + n.toString
    }
    s"$name${if(number.isDefined) number else ""}$state_name$group_name"
  }

  //todo: usage of this. Can it be deleted?
//  def identification: String = {
//    val state_name = state match {
//      case Nothing => ""
//      case Middle(n) => "mid" + n.toString
//      case In(n) => "in" + n.toString
//      case Out(n) => "out" + n.toString
//    }
//    s"$name${if (number != -1) number else ""}$state_name"
//  }

  def coAction: Action = Action(name, group.coGroup, state, number)

  override def equals(o: scala.Any): Boolean =
    if (o == null)
      false
    else if(o.getClass != this.getClass)
      false
    else
      this.number == o.asInstanceOf[Action].number && this.group == o.asInstanceOf[Action].group &&
        this.name ==o.asInstanceOf[Action].name && this.state == o.asInstanceOf[Action].state

  def toNumberedAction(n: Int): Action = Action(name, group, state, Some(n))

  def sameType(a: Action): Boolean =
    this.number == a.number && this.name == a.name && this.state == a.state

}

object Action{
  //  def apply(number: Int, group: Group): Action = new Action("X", number, group, Nothing)

  /**
    * An action that will print Null
    */
  def nullAction: Action = Action("Null", NoLine, Nothing)

  def nodeAction(name: String, number: Int, state:State): Action = new Action(name, OneLine, state, Some(number))

  def nodeAction(name:String, state:State): Action = new Action(name, OneLine, state)

  def channelAction(name: String, number: Int, state:State): Action = new Action(name, TwoLine, state, Some(number))

  def channelAction(name: String, state:State): Action = new Action(name, TwoLine, state)

}

/**
  * A process name is just a name of a process (useful when we have a Mcrl2Process agglomeration)
  * @param name the name of the process this represents
  */
case class ProcessName(name: String) extends Atom{
  override def getActions: Set[Action] = Set()

  override def toString: String = name
}

/**
  * A multiaction defines the operator for multiactions
  * @param actions the actions that will be used in the multiaction
  */
//todo: check if we can distinguish between NAction and UAction without explicitly saying so
case class MultiAction(actions: List[Action]) extends Atom{
  override def toString: String = toString(actions)

  private def toString(actions: List[Action]): String = actions match {
    case x :: y :: rest => x.toString + " | " + toString(y :: rest)
    case x :: Nil => x.toString
    case Nil => "Null"
  }

  def getHead: Action = if(actions.nonEmpty) actions.head else null

  def getLast: Action = if(actions.nonEmpty) actions.last else null

  override def getActions: Set[Action] = actions.toSet

}

object MultiAction{
  def apply(actions: Action*) = new MultiAction(actions.toList)
}
