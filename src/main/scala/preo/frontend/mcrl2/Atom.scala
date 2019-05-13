package preo.frontend.mcrl2


/**
  * Atoms can be single actions, multi-actions, or process names
  */
sealed abstract class Atom extends ProcessExpr



sealed abstract class State

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
  * @param params list of parameters (var,type) (for now, until we have proper Data Types)
  */
case class Action(var name: String,var state: State, number: Option[Int] = None,params:List[(String,String)]=List()) extends Atom{

  def getActions: Set[Action] = Set(this)

  override def toString: String = {
    val state_name = state match{
      case Nothing => ""
      case Middle(n) => "m" + n.toString
      case In(n) => "i" + n.toString
      case Out(n) => "o" + n.toString
      case Sync => "sync"
    }
    s"$name${if(number.isDefined) "_"+number.get else ""}$state_name${if (params.nonEmpty) params.map(_._1).mkString("(",",",")") else ""}"
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
  def join(a: Action): Action = Action(this.toString +"_"+ a.toString, Nothing, None)

  def |(other:Action):MultiAction = MultiAction(List(this,other))

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
case class ProcessName(name: String,actualParam:List[String]=List()) extends Atom{
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

  def |(a:Action): MultiAction = MultiAction(actions++List(a))
  def |(ma:MultiAction):MultiAction = MultiAction(actions++ma.actions)

  override def getActions: Set[Action] = actions.toSet

}

object MultiAction{
  def apply(actions: Action*) = new MultiAction(actions.toList)
}
