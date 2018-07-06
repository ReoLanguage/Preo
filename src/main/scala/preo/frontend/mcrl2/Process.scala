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
  * @param prev the previous nodes (as many as the length of before)
  * @param next the next nodes (as many as the length of after)
  */
//todo: do we need prev and next?
case class Channel(name:String = "Channel", number: Option[Int],var before: List[Action],var after: List[Action],
                        operator: Operation, var prev: List[Channel] = Nil,var next: List[Channel]= Nil)
  extends Process{

  if(operator == null){
    throw new NullPointerException("null Operator Invalid")
  }

  //should not be used when it has no number
  override def toString: String = s"$name${if(number.isDefined) number.get else ""} = (${operator.toString}) . $name${if(number.isDefined) number.get else ""}"

  def getName:ProcessName = ProcessName(s"$name${if(number.isDefined) number.get else ""}")

  def addPrev(new_prev: Channel): Unit = this.prev ++= List(new_prev)

  def addNext(new_next: Channel): Unit = this.next ++= List(new_next)

  def getActions: Set[Action] = before.toSet ++ after.toSet


  override def equals(o: scala.Any): Boolean = {
    if(o == null || o.getClass != this.getClass) false
    else {
      val c = o.asInstanceOf[Channel]
      this.name == c.name && this.number == c.number
    }
  }

  override def hashCode(): Int = (name, number).hashCode()

  def toNumberedChannel(n: Int): Channel = Channel(name, Some(n), before, after, operator, prev, next)
}


/**
  * The init processes (for making communication between nodes and channels in mcrl2)
  * @param number the identification number of the init
  * @param var_name the name of the actions to communicate
  * @param var_number the number identification of the actions
  * @param var_state the state of the actions (with in1, in2, out1, out2 or nothing)
  * @param procs the processes that will be integrated in the init (should be 1 or 2)
  */
case class Init(number: Option[Int], action1 :Action, action2: Action, procs: List[ProcessName],var toHide: Boolean) extends Process{
  def operator: Operation = {
    val sync_action = action1 join action2
    val basicProc = procs.tail.foldRight(procs.head)((base, p) => ProcessName(Par(base, p).toString))
    val operator =  Block(List(action1, action2), Comm((action1, action2, sync_action), basicProc))

    if(toHide) Hide(List(sync_action), operator)
    else operator
  }

  override def toString: String = s"Init${if(number.isDefined) number.get else ""} = ${operator.toString}"

  def getActions: Set[Action] = Set(action1 join action2)

  def getName: ProcessName= ProcessName(s"Init${if(number.isDefined) number.get else ""}")

  def toNumberedInit(n: Int): Init = Init(Some(n), action1, action2, procs, toHide)
}


