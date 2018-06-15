package preo.modelling

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
  * Defines a Node in Mcrl2.
  * The nodes and channels define a graph.
  * @param number Identification number of the Node
  * @param before Before action
  * @param after After action
  * @param prev Previous channel (connected by the before action)
  * @param next Next channel (connected by the after action)
  */
//todo: do the channels need to be vars?
case class Node(number: Option[Int], before: Action, after: Action, var prev: Channel = null,var next: Channel=null)
  extends Process{

  override def toString: String = s"Node${if(number.isDefined) number else ""} = (${before.toString} | ${after.toString}) . Node${if(number.isDefined) number else ""}"

  /**
    * Returns a Process Name with the proc name of the Node
    */
  def getName: ProcessName = ProcessName(s"Node$number")

  def getPrev: Channel = prev

  def getNext: Channel = next

  def setPrev(new_prev: Channel): Unit = this.prev = new_prev

  def setNext(new_next: Channel): Unit = this.next = new_next

  def getActions: Set[Action] = Set(before, after)

  /**
    * Joins this node with another node in the following manner:
    * This(number1, before1, after1, prev1, next1) ++ Other(number2, before2, after2, prev2, next2) = Node(number1, before1, after2, prev1, next2)
    * @param other the node to join on this one
    * @return the new node.
    */
  def ++(other: Node): Node = Node(number, before, other.after, prev, other.next)

  //todo: probably obsolete
//  def replace(replacements: Map[String, NChannel]): Unit = {
//    prev = if (prev != null && replacements.get(prev.getName.toString).isDefined) replacements(prev.getName.toString) else prev
//    next = if (next != null && replacements.get(next.getName.toString).isDefined) replacements(next.getName.toString) else next
//  }
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
case class Channel(name:String = "Channel", number: Option[Int], before: List[Action], after: List[Action],
                        operator: Operation, var prev: List[Node] = Nil,var next: List[Node]= Nil)
  extends Process{

  if(operator == null){
    throw new NullPointerException("null Operator Invalid")
  }

  //should not be used whem it has no number
  override def toString: String = s"$name${if(number.isDefined) number else ""} = (${operator.toString}) . $name${if(number.isDefined) number else ""}"

  def getName:ProcessName = ProcessName(s"$name$number")

  def getPrev: List[Node] = prev

  def getNext: List[Node] = next

  def addPrev(new_prev: Node): Unit = this.prev ++= List(new_prev)

  def addNext(new_next: Node): Unit = this.next ++= List(new_next)

  def getBefore: List[Action] = before

  def getAfter: List[Action] = after

  def getActions: Set[Action] = before.toSet ++ after.toSet

  /**
    * Replaces the nodes whose name is a key to the received map with the values associated.
    * Usefull to create a new graph in the familyModel
    * @param replacements The nodes to replace and respective replacements
    */
  def replace(replacements: Map[String, Node]): Unit = {
    prev = prev.map(node =>if (replacements.get(node.getName.toString).isDefined) replacements(node.getName.toString) else node)
    next = next.map(node =>if (replacements.get(node.getName.toString).isDefined) replacements(node.getName.toString) else node)
  }

  override def equals(o: scala.Any): Boolean = {
    if(o == null || o.getClass != this.getClass) false
    else {
      val c = o.asInstanceOf[Channel]
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
case class Init(number: Option[Int], var_name:String, var_number: Int,var_state:State, procs: List[ProcessName]) extends Process{
  def operator: Mcrl2Process = {
    val action1 = Action(var_name, NoLine, var_state, Some(var_number))
    val action2 = Action(var_name, OneLine, var_state, Some(var_number))
    val action3 = Action(var_name, TwoLine, var_state, Some(var_number))
    val basicProc = procs.tail.foldRight(procs.head)((base, p) => ProcessName(Par(base, p).toString))
    val operator =  Block(List(action2, action3), Comm((action2, action3, action1), basicProc))
    var_state match{
      case Middle(_) => Hide(List(action1), operator)
      case _ => operator
    }
  }

  override def toString: String = s"Init$number = ${operator.toString}"

  def getActions: Set[Action] = Set(Action(var_name, NoLine, var_state, Some(var_number)))

  def getName: ProcessName= ProcessName(s"Init$number")

  def getProcs: List[ProcessName] = procs

  //  def replaceProc(old_proc: ProcessName, new_proc: ProcessName) = Mcrl2Init(number,var_name, var_number, var_state, procs.map(f =>if (f==old_proc) new_proc else f))
}


object Init{
  def apply(number: Int, var_name:String, var_number: Int,var_state:State, proc: Process): Init = {
    val name = ProcessName(proc.toString)
    Init(Some(number),var_name, var_number, var_state, List(name))
  }

  def apply(var_name:String, var_number: Int,var_state:State, proc: Process): Init = {
    val name = ProcessName(proc.toString)
    Init(None,var_name, var_number, var_state, List(name))
  }

  def apply(number: Int, var_name: String, var_number: Int,var_state:State, proc1: Process, proc2: Process): Init = {
    val name1 = ProcessName(proc1.toString)
    val name2 = ProcessName(proc2.toString)
    Init(Some(number),var_name, var_number, var_state, List(name1, name2))
  }

  def apply(var_name: String, var_number: Int,var_state:State, proc1: Process, proc2: Process): Init = {
    val name1 = ProcessName(proc1.toString)
    val name2 = ProcessName(proc2.toString)
    Init(None,var_name, var_number, var_state, List(name1, name2))
  }
}






/**
  * The starter node defines a process that starts a node after an action is performed
  * @param number the identification number of the StarterNode and its action
  * @param node The node which will be started after the action
  */
case class StarterNode(number: Option[Int], node: Node) extends Process{
  override def toString: String = s"StarterNode$number = ${getActions.head.toString} . ${node.getName}"

  def getActions: Set[Action] = Set(Action("a", OneLine, Nothing, number))

  def getName = ProcessName(s"StarterNode${if(number.isDefined) number else ""}")

  def getNext: Channel = node.getNext
}

/**
  * The Manager defines groups of actions that will be performed
  * In other words, if a manager has 5 multiactions, a choice operator will be inserted between them so that only
  * one multiaction is performed at a time
  * @param actions the list of multiactions
  */
case class Manager(actions : List[MultiAction]) extends Process{
  override def toString: String = s"Manager = ${getProcess.toString}"

  private def getProcess: Process = {
    actions.tail.foldRight(actions.head: Operation)((a, b) => Choice(a, b))
  }

  def getVars: List[Action] = {
    actions.map(m => m.actions).foldRight(Nil: List[Action])((a, b) => a++b)
  }

  def getName = ProcessName("Manager")

  def addMultiAction(ma: MultiAction): Manager = Manager(actions ++ List(ma))
}

/**
  * The starter will create a comunication between the inits and the manager, in the same way that inits make comunication
  * between the channels and the nodes
  * @param number The identification number of the starter
  * @param proc1 the first process to make comunication
  * @param proc2 the second process to make comunication
  */
case class Starter(number: Option[Int], proc1: ProcessName, proc2: ProcessName) extends Process{
  private def getProcess = {
    val a1 = Action("a", TwoLine, Nothing, number)
    val a2 = Action("a", OneLine, Nothing, number)
    val a3 = Action("a", NoLine, Nothing, number)
    val operator1 = Par(proc1, proc2)
    val operator2 = Comm((a1, a2, a3), operator1)
    val operator3 = Block(List(a2, a1), operator2)
    Hide(List(a3), operator3)
  }

  override def toString: String = s"Starter$number = ${getProcess.toString}"

  def getVars = List(Action("a", NoLine, Nothing, number))

  def getName = ProcessName(s"Starter$number")
}
