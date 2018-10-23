package preo.frontend.mcrl2

abstract class Process {
  def getName: ProcessName
  def getActions: Set[Action]
  override def toString: String
  def getOperation: ProcessExpr
}



object Process{
  def toString(act: List[Action]): String = act.mkString(", ")
//    act match{
//    case x :: y:: rest => x.toString + ", " + toString(y :: rest)
//    case x:: Nil => x.toString
//    case Nil => ""
//  }
}


/**
  * Defines a Channel (ex: fifo) in Mcrl2
  * Defines a graph with the node class (see above)
  * @param name name of the channel (ex: Fifo)
  * @param number the identification number
  * @param in the actions on the left side (should be 1 or 2)
  * @param out the actions on the right side (should be 1 or 2)
  * @param expression the operator that defines the channel. This should include the before and after actions
  */
//todo: do we need prev and next?
case class Channel(name:String = "Channel", number: Option[Int], var in: List[Action], var out: List[Action],
                   expression: ProcessExpr)
  extends Process{

  if(expression == null){
    throw new NullPointerException("null Operator Invalid")
  }

  //should not be used when it has no number
  override def toString: String = s"$name${if(number.isDefined) number.get else ""} = (${expression.toString}) . $name${if(number.isDefined) number.get else ""}"

  def getName:ProcessName = ProcessName(s"$name${if(number.isDefined) number.get else ""}")

  def getActions: Set[Action] = in.toSet ++ out.toSet


  override def equals(o: scala.Any): Boolean = {
    if(o == null || o.getClass != this.getClass) false
    else {
      val c = o.asInstanceOf[Channel]
      this.name == c.name && this.number == c.number
    }
  }

  override def hashCode(): Int = (name, number).hashCode()

  override def getOperation: ProcessExpr = expression
}


/**
  * The init processes (for making communication between nodes and channels in mcrl2)
  * @param number the identification number of the init
  * @param procs the processes that will be integrated in the init (should be 1 or 2)
  */
case class Init(number: Option[Int], action1 :Action, action2: Action, procs: List[ProcessName],var toHide: Boolean) extends Process{
  def getOperation: ProcessExpr = {
    val sync_action = action1 join action2
    val basicProc = procs.tail.foldRight(procs.head : ProcessExpr)((base, p) => Par(base, p))
    val operator =  Block(List(action1, action2), Comm(List(action1, action2), sync_action, basicProc))

    if(toHide) Hide(List(sync_action), operator)
    else operator
  }

  override def toString: String = s"Init${if(number.isDefined) number.get else ""} = ${getOperation.toString}"

  def getActions: Set[Action] = Set(action1 join action2)

  def getName: ProcessName= ProcessName(s"Init${if(number.isDefined) number.get else ""}")

  def toNumberedInit(n: Int): Init = Init(Some(n), action1, action2, procs, toHide)

  override def equals(o: scala.Any): Boolean = {
    if(o == null || o.getClass != this.getClass) false
    else {
      val c = o.asInstanceOf[Init]
      this.number == c.number && action1 == c.action1 && action2 == c.action2 && procs == c.procs
    }
  }
}

case class EntryNode(number:Int, action: Action, proc: ProcessName) extends Process{
  def getOperation: ProcessExpr = Seq(action, proc)

  override def toString: String = s"EntryNode$number = ${getOperation.toString}"

  def getActions: Set[Action] = Set(action)

  def getName: ProcessName= ProcessName(s"EntryNode$number")
}
