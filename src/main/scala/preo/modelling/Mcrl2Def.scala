package preo.modelling


/**
  * Defines the Procs in Mcrl2.
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

/**
  * Defines a Node in Mcrl2.
  * The nodes and channels define a graph.
  * @param number Identification number of the Node
  * @param before Before action
  * @param after After action
  * @param prev Previous channel (connected by the before action)
  * @param next Next channel (connected by the after action)
  */

case class Mcrl2Node(number: Int, before: Action, after: Action, var prev: Mcrl2Channel = null,var next: Mcrl2Channel=null)
  extends Mcrl2Def{


  override def toString: String = s"Node$number = (${before.toString} | ${after.toString}) . Node$number"


  /**
    * Returns a Process Name with the proc name of the Node
    */
  def getName: ProcessName = ProcessName(s"Node$number")

  def getPrev: Mcrl2Channel = prev

  def getNext: Mcrl2Channel = next

  def setPrev(new_prev: Mcrl2Channel): Unit = this.prev = new_prev

  def setNext(new_next: Mcrl2Channel): Unit = this.next = new_next

  def getBefore: Action = before

  def getAfter: Action = after

  def getVars: List[Action] = Set(before, after).toList

  /**
    * Joins this node with another node in the following manner:
    * This(number1, before1, after1, prev1, next1) ++ Other(number2, before2, after2, prev2, next2) = Node(number1, before1, after2, prev1, next2)
    * @param other the node to join on this one
    * @return the new node.
    */
  def ++(other: Mcrl2Node): Mcrl2Node = Mcrl2Node(number, before, other.after, prev, other.next)

  def replace(replacements: Map[String, Mcrl2Channel]): Unit = {
    prev = if (prev != null && replacements.get(prev.getName.toString).isDefined) replacements(prev.getName.toString) else prev
    next = if (next != null && replacements.get(next.getName.toString).isDefined) replacements(next.getName.toString) else next
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

  /**
    * Replaces the nodes whose name is a key to the received map with the values associated.
    * Usefull to create a new graph in the familyModel
    * @param replacements The nodes to replace and respective replacements
    */
  def replace(replacements: Map[String, Mcrl2Node]): Unit = {
    prev = prev.map(node =>if (replacements.get(node.getName.toString).isDefined) replacements(node.getName.toString) else node)
    next = next.map(node =>if (replacements.get(node.getName.toString).isDefined) replacements(node.getName.toString) else node)
  }

  override def equals(o: scala.Any): Boolean = {
    if(o == null || o.getClass != this.getClass) false
    else {
      val c = o.asInstanceOf[Mcrl2Channel]
      this.name == c.name && this.number == c.number
    }
  }

  override def hashCode(): Int = (name, number).hashCode()
}


/**
  * The init processes (for making communication between nodes and channels in mcrl2)
  * @param number the identification number of the init
  * @param var_name the name of the actions to communicate
  * @param var_number the number identification of the actions
  * @param var_state the state of the actions (with in1, in2, out1, out2 or nothing)
  * @param procs the processes that will be integrated in the init (should be 1 or 2)
  */
case class Mcrl2Init(number: Int, var_name:String, var_number: Int,var_state:State, procs: List[ProcessName]) extends Mcrl2Def{
  def operator: Mcrl2Process = {
    val action1 = Action(var_name, var_number, NoLine, var_state)
    val action2 = Action(var_name, var_number, OneLine, var_state)
    val action3 = Action(var_name, var_number, TwoLine, var_state)
    val basicProc = procs.tail.foldRight(procs.head)((base, p) => ProcessName(Par(base, p).toString))
    val operator =  Block(List(action2, action3), Comm((action2, action3, action1), basicProc))
    var_state match{
      case Middle(_) => Hide(List(action1), operator)
      case _ => operator
    }
  }

  override def toString: String = s"Init$number = ${operator.toString}"

  def getVars = List(Action(var_name, var_number, NoLine, var_state))

  def getName: ProcessName= ProcessName(s"Init$number")

  def getProcs: List[ProcessName] = procs

//  def replaceProc(old_proc: ProcessName, new_proc: ProcessName) = Mcrl2Init(number,var_name, var_number, var_state, procs.map(f =>if (f==old_proc) new_proc else f))
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


/**
  * The starter node defines a process that starts a node after an action is performed
  * @param number the identification number of the StarterNode and its action
  * @param node The node which will be started after the action
  */
case class Mcrl2StarterNode(number: Int, node: Mcrl2Node) extends Mcrl2Def{
  override def toString: String = s"StarterNode$number = ${getVars.head.toString} . ${node.getName}"

  def getVars = List(Action("a", number, OneLine, Nothing))

  def getName = ProcessName(s"StarterNode$number")

  def getNext: Mcrl2Channel = node.getNext
}

/**
  * The Manager defines groups of actions that will be performed
  * In other words, if a manager has 5 multiactions, a choice operator will be inserted between them so that only
  * one multiaction is performed at a time
  * @param actions the list of multiactions
  */
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

/**
  * The starter will create a comunication between the inits and the manager, in the same way that inits make comunication
  * between the channels and the nodes
  * @param number The identification number of the starter
  * @param proc1 the first process to make comunication
  * @param proc2 the second process to make comunication
  */
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
