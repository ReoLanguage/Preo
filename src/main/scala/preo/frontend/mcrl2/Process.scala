package preo.frontend.mcrl2

abstract class Process {
  def getName: ProcessName
  def getActions: Set[Action]
  override def toString: String
}



object Process{
  def toString(act: List[Action]): String = act match{
    case x :: y:: rest => x.toString + ", " + toString(y :: rest)
    case x:: Nil => x.toString
    case Nil => ""
  }
}


/**
  * Defines a Channel (ex: fifo) in Mcrl2
  * Defines a graph with the node class (see above)
  * @param name name of the channel (ex: Fifo)
  * @param number the identification number
  * @param before the actions on the left side (should be 1 or 2)
  * @param after the actions on the right side (should be 1 or 2)
  * @param operator the operator that defines the channel. This should include the before and after actions
  */
//todo: do we need prev and next?
case class Channel(name:String = "Channel", number: Option[Int],var before: List[Action],var after: List[Action],
                        operator: Operation)
  extends Process{

  if(operator == null){
    throw new NullPointerException("null Operator Invalid")
  }

  //should not be used when it has no number
  override def toString: String = s"$name${if(number.isDefined) number.get else ""} = (${operator.toString}) . $name${if(number.isDefined) number.get else ""}"

  def getName:ProcessName = ProcessName(s"$name${if(number.isDefined) number.get else ""}")

  def getActions: Set[Action] = before.toSet ++ after.toSet


  override def equals(o: scala.Any): Boolean = {
    if(o == null || o.getClass != this.getClass) false
    else {
      val c = o.asInstanceOf[Channel]
      this.name == c.name && this.number == c.number
    }
  }

  override def hashCode(): Int = (name, number).hashCode()

  def toNumberedChannel(n: Int): Channel = Channel(name, Some(n), before, after, operator)
}


/**
  * The init processes (for making communication between nodes and channels in mcrl2)
  * @param number the identification number of the init
  * @param procs the processes that will be integrated in the init (should be 1 or 2)
  */
case class Init(number: Option[Int], action1 :Action, action2: Action, procs: List[ProcessName],var toHide: Boolean) extends Process{
  def operator: Operation = {
    val sync_action = action1 join action2
    val basicProc = procs.tail.foldRight(procs.head)((base, p) => ProcessName(Par(base, p).toString))
    val operator =  Block(List(action1, action2), Comm(List(action1, action2), sync_action, basicProc))

    if(toHide) Hide(List(sync_action), operator)
    else operator
  }

  override def toString: String = s"Init${if(number.isDefined) number.get else ""} = ${operator.toString}"

  def getActions: Set[Action] = Set(action1 join action2)

  def getName: ProcessName= ProcessName(s"Init${if(number.isDefined) number.get else ""}")

  def toNumberedInit(n: Int): Init = Init(Some(n), action1, action2, procs, toHide)
}

case class EntryNode(number:Int, action: Action, proc: ProcessName) extends Process{
  private val operator: Operation = Seq(action, proc)

  override def toString: String = s"EntryNode$number = ${operator.toString}"

  def getActions: Set[Action] = Set(action)

  def getName: ProcessName= ProcessName(s"EntryNode$number")

}


case class Starter(number: Int, syncActions: List[Action], resultingAction: Action, procs: List[ProcessName]) extends Process{
  def operator: Operation = {
    val basicProc = procs.tail.foldRight(procs.head)((base, p) => ProcessName(Par(base, p).toString))
    val operator =  Hide(List(resultingAction), Block(syncActions, Comm(syncActions, resultingAction, basicProc)))
    operator
  }

  override def toString: String = s"Starter$number = ${operator.toString}"

  def getActions: Set[Action] = Set(resultingAction)

  def getName: ProcessName= ProcessName(s"Starter$number")
}


case class Manager(actions: List[MultiAction]) extends Process{
  override def getName: ProcessName = ProcessName("Manager")

  override def getActions: Set[Action] = actions.flatMap(m => m.getActions).toSet

  override def toString: String =
    if(actions.isEmpty) "Manager = delta"
    else{
      val operation = actions.tail.foldRight(actions.head: Operation)((m, res) => Par(res, m)).toString
      s"Manager = $operation"
    }
}
