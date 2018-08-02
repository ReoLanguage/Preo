package preo.frontend.mcrl2

abstract class Atom extends Operation



class State

case class In(n: Int) extends State
case class Middle(n: Int) extends State
case class Out(n: Int) extends State
case object Sync extends State
case object Nothing extends State


/**
  * An action is an atomic element of mcrl2
  * @param name name of the action
  * @param state the state defines if it is an input or output action, or neither
  * @param number identification number, or None in case of families
  */
case class Action(var name: String,var state: State, number: Option[Int] = None) extends Atom{

  def getActions: Set[Action] = Set(this)

  override def toString: String = {
    val state_name = state match{
      case Nothing => ""
      case Middle(n) => "mid" + n.toString
      case In(n) => "in" + n.toString
      case Out(n) => "out" + n.toString
      case Sync => "sync"
    }
    s"$name${if(number.isDefined) number.get else ""}$state_name"
  }


  override def equals(o: scala.Any): Boolean =
    if (o == null)
      false
    else if(o.getClass != this.getClass)
      false
    else
      this.number == o.asInstanceOf[Action].number &&
        this.name ==o.asInstanceOf[Action].name && this.state == o.asInstanceOf[Action].state

  //joins actions in our new nodeless model
  def join(a: Action): Action = Action(this.toString + a.toString, Nothing, None)

}

object Action{
  /**
    * An action that will print Null
    */
  def nullAction: Action = Action("Null", Nothing)

  def controllerAction(number: Int): Action = Action("X", Sync, Some(number))

  def syncAction(number: Int): Action = Action("X", Nothing, Some(number))

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
