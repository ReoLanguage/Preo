package preo.modelling


/**
  * So far every mcrl2process puts the name at the end of the connector (ex: Fifo1 = (a.b).Fifo1)
  */
abstract class Mcrl2Def{
  def getVars: List[Action]
  def getName: ProcessName
}

object Mcrl2Def{
  def toString(act: List[Action]): String = act match{
    case x :: y:: rest => x.toString + ", " + toString(y :: rest)
    case x:: Nil => x.toString
    case Nil => ""
  }
}

case class Mcrl2Node(number: Int, var before: Action, var after: Action,var prev: Mcrl2Channel = null,var next: Mcrl2Channel=null)
  extends Mcrl2Def{

  if(before == null){
      before = Action(0, 4)
  }
  if(after == null){
      after = Action(0, 4)
  }

  override def toString: String = s"Node$number = (${before.toString} | ${after.toString}) . Node$number"

  def getName: ProcessName = ProcessName(s"Node$number")

  def getPrev: Mcrl2Channel = prev

  def getNext: Mcrl2Channel = next

  def setPrev(new_prev: Mcrl2Channel): Unit = this.prev = new_prev

  def setNext(new_next: Mcrl2Channel): Unit = this.next = new_next

  def getBefore: Action = before

  def getAfter: Action = after

  def getVars: List[Action] = Set(before, after).toList

  def setRight(name: String, number: Int, state: Int = 1): Unit = this.setRight(Action(name, number, 2, state))

  def setRight(action: Action): Unit = this.after = action

  def setLeft(name: String, number: Int, state: Int = 3): Unit = this.setLeft(Action(name, number, 2, state))

  def setLeft(action: Action): Unit = this.before = action
}

case class Mcrl2Channel(name:String = "Channel", number: Int,var before: List[Action],var after: List[Action],
                        operator: Mcrl2Process,var prev: List[Mcrl2Node] = Nil,var next: List[Mcrl2Node]= Nil)
  extends Mcrl2Def{

  if(operator == null){
    throw new NullPointerException("null Operator Invalid")
  }


  override def toString: String = s"$name$number = (${operator.toString}) . $name$number"

  def getName:ProcessName = ProcessName(s"$name$number")

  def getPrev: List[Mcrl2Node] = prev

  def getNext: List[Mcrl2Node] = next

  def addPrev(new_prev: Mcrl2Node): Unit = this.prev ++= List(new_prev)

  def addNext(new_next: Mcrl2Node): Unit = this.next ++= List(new_next)

  def getBefore: List[Action] = before

  def getAfter: List[Action] = after

  def getVars: List[Action] = before ++ after

  def addRight(action: Int): Unit = this.addRight(Action(action, 2))

  def addRight(action: Action): Unit = this.after ++= List(action)

  def addLeft(action: Int): Unit = this.addLeft(Action(action, 2))

  def addLeft(action: Action): Unit = this.before ++= List(action)
}



case class Mcrl2Init(number: Int, action: Action, operator: Mcrl2Process) extends Mcrl2Def{
  override def toString: String = s"Init$number = ${operator.toString}"

  def getVars = List(action)

  def getName: ProcessName= ProcessName(s"Init$number")
}


object Mcrl2Init{
  def apply(number: Int, var_name:String, var_number: Int,var_state:Int, proc: Mcrl2Process): Mcrl2Init = {
    val action1 = Action(var_name, var_number, 3, var_state)
    val action2 = Action(var_name, var_number, 2, var_state)
    val action3 = Action(var_name, var_number, 1, var_state)
    val operator =  Block(List(action2, action3), Comm((action2, action3, action1), ProcessName(proc.toString)))
    Mcrl2Init(number,action1, operator)
  }

  def apply(number: Int, var_name: String, var_number: Int,var_state:Int, proc1: Mcrl2Process, proc2: Mcrl2Process): Mcrl2Init = {
    val action1 = Action(var_name, var_number, 3, var_state)
    val action2 = Action(var_name, var_number, 2, var_state)
    val action3 = Action(var_name, var_number, 1, var_state)
    val operator1 = Par(ProcessName(proc1.toString), ProcessName(proc2.toString))
    val operator2 = Comm((action2, action3, action1), operator1)
    val operator = Block(List(action2, action3), operator2)
    Mcrl2Init(number,action1, operator)
  }
}
