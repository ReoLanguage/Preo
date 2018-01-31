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

case class Mcrl2Node(number: Int, before: Action, after: Action, var prev: Mcrl2Channel = null,var next: Mcrl2Channel=null)
  extends Mcrl2Def{


  override def toString: String = s"Node$number = (${before.toString} | ${after.toString}) . Node$number"

  def getName: ProcessName = ProcessName(s"Node$number")

  def getPrev: Mcrl2Channel = prev

  def getNext: Mcrl2Channel = next

  def setPrev(new_prev: Mcrl2Channel): Unit = this.prev = new_prev

  def setNext(new_next: Mcrl2Channel): Unit = this.next = new_next

  def getBefore: Action = before

  def getAfter: Action = after

  def getVars: List[Action] = Set(before, after).toList

  def ++(other: Mcrl2Node): Mcrl2Node = Mcrl2Node(number, before, other.after, prev, other.next)
//  def setRight(name: String, number: Int, state: State = In1): Unit = this.setRight(Action(name, number, OneLine, state))
//
//  def setRight(action: Action): Unit = this.after = action
//
//  def setLeft(name: String, number: Int, state: State = Out1): Unit = this.setLeft(Action(name, number, OneLine, state))
//
//  def setLeft(action: Action): Unit = this.before = action
}

case class Mcrl2Channel(name:String = "Channel", number: Int, before: List[Action], after: List[Action],
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

  def replace(replacements: Map[String, Mcrl2Node]): Unit = {
    prev = prev.map(node =>if (replacements.get(node.getName.toString).isDefined) replacements(node.getName.toString) else node)
    next = next.map(node =>if (replacements.get(node.getName.toString).isDefined) replacements(node.getName.toString) else node)
  }

//  def addRight(action: Int): Unit = this.addRight(Action(action, TwoLine))
//
//  def addRight(action: Action): Unit = this.after ++= List(action)
//
//  def addLeft(action: Int): Unit = this.addLeft(Action(action, TwoLine))
//
//  def addLeft(action: Action): Unit = this.before ++= List(action)
}



case class Mcrl2Init(number: Int, var_name:String, var_number: Int,var_state:State, procs: List[ProcessName]) extends Mcrl2Def{
  def operator: Mcrl2Process = {
    val action1 = Action(var_name, var_number, NoLine, var_state)
    val action2 = Action(var_name, var_number, OneLine, var_state)
    val action3 = Action(var_name, var_number, TwoLine, var_state)
    val basicProc = procs.tail.foldRight(procs.head)((base, p) => ProcessName(Par(base, p).toString))
    val operator =  Block(List(action2, action3), Comm((action2, action3, action1), basicProc))
    operator
  }

  override def toString: String = s"Init$number = ${operator.toString}"

  def getVars = List(Action(var_name, var_number, NoLine, var_state))

  def getName: ProcessName= ProcessName(s"Init$number")

  def getProcs: List[ProcessName] = procs

  def replaceProc(old_proc: ProcessName, new_proc: ProcessName) = Mcrl2Init(number,var_name, var_number, var_state, procs.map(f =>if (f==old_proc) new_proc else f))
}


object Mcrl2Init{
  def apply(number: Int, var_name:String, var_number: Int,var_state:State, proc: Mcrl2Process): Mcrl2Init = {
    val name = ProcessName(proc.toString)
    Mcrl2Init(number,var_name, var_number, var_state, List(name))
  }

  def apply(number: Int, var_name: String, var_number: Int,var_state:State, proc1: Mcrl2Process, proc2: Mcrl2Process): Mcrl2Init = {
    val name1 = ProcessName(proc1.toString)
    val name2 = ProcessName(proc2.toString)
    Mcrl2Init(number,var_name, var_number, var_state, List(name1, name2))
  }
}

case class Mcrl2StarterNode(number: Int, node: Mcrl2Node) extends Mcrl2Def{
  override def toString: String = s"StarterNode$number = ${getVars.head.toString} . ${node.getName}"

  def getVars = List(Action("a", number, OneLine, Nothing))

  def getName = ProcessName(s"StarterNode$number")

  def getNext = node.getNext
}

//receives initial number of nodes to the ending number
case class Mcrl2Manager(actions : List[MultiAction]) extends Mcrl2Def{
  override def toString: String = s"Manager = ${getProcess.toString}"

  private def getProcess: Mcrl2Process = {
    actions.tail.foldRight(actions.head: Mcrl2Process)((a, b) => Choice(a, b))
  }

  def getVars: List[Action] = {
    actions.map(m => m.actions).foldRight(Nil: List[Action])((a, b) => a++b)
  }

  def getName = ProcessName("Manager")

  def addMultiAction(ma: MultiAction): Mcrl2Manager = Mcrl2Manager(actions ++ List(ma))
}

case class Mcrl2Starter(number: Int, proc1: ProcessName, proc2: ProcessName) extends Mcrl2Def{
  private def getProcess = {
    val a1 = Action("a", number, TwoLine, Nothing)
    val a2 = Action("a", number, OneLine, Nothing)
    val a3 = Action("a", number, NoLine, Nothing)
    val operator1 = Par(proc1, proc2)
    val operator2 = Comm((a1, a2, a3), operator1)
    val operator3 = Block(List(a2, a1), operator2)
    Hide(List(a3), operator3)
  }

  override def toString: String = s"Starter$number = ${getProcess.toString}"

  def getVars = List(Action("a", number, NoLine, Nothing))

  def getName = ProcessName(s"Starter$number")
}
