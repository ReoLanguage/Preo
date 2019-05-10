package preo.frontend.mcrl2

/**
  * A process expression can be a sequence, a choice, a parallem, a communication, or an atomic action (Atom)
  */
abstract class ProcessExpr {
  override def toString: String

  def getActions: Set[Action]

  def ||(other:ProcessExpr) = Par(this,other)
  def &(other:ProcessExpr) = Seq(this,other)
  def +(other:ProcessExpr) = Choice(this,other)

}

/**
  * the sequence operator in mcrl2 (before . after) when printed
  * @param before before process
  * @param after after process
  */
case class Seq(before: ProcessExpr, after: ProcessExpr) extends ProcessExpr{
  override def toString: String = s"(${before.toString}) . (${after.toString})"

  override def getActions: Set[Action] = before.getActions ++ after.getActions
}


/**
  * the choice operator in mcrl2 (left + right) when printed
  * @param left left process
  * @param right right process
  */
case class Choice(left: ProcessExpr, right: ProcessExpr) extends ProcessExpr{
  override def toString: String = s"(${left.toString}) + (${right.toString})"

  override def getActions: Set[Action] = left.getActions ++ right.getActions
}


/**
  * the paralel operator in mcrl2 (left || right) when printed
  * @param left left process
  * @param right right process
  */
case class Par(left: ProcessExpr, right:ProcessExpr) extends ProcessExpr{
  override def toString: String = s"(${left.toString}) || (${right.toString})"

  override def getActions: Set[Action] = left.getActions ++ right.getActions
}


case class Sum(vars:Map[String,String],procExpr:ProcessExpr) extends ProcessExpr {
  override def toString: String = s"(sum ${vars.map(v => v._1 +":"+ v._2).mkString(",")} . (${procExpr}))"
  override def getActions: Set[Action] = procExpr.getActions
}

case class ITE(cond:String, thenProc:ProcessExpr, elseProc:Option[ProcessExpr]=None) extends ProcessExpr {
  override def toString: String = s"(${cond}) -> ((${thenProc}) ${if (elseProc.isDefined) s"<> ${elseProc.get}" else "" })"
  override def getActions: Set[Action] =
    thenProc.getActions ++ (if (elseProc.isDefined) elseProc.get.getActions else Set())
}

/**
  * creates a communication operator with the 3 actions in the tuple in the process
  * @param syncActions the actions to communicate
  * @param in the process where the communication will happen
  */
case class Comm(syncActions: List[Action], resultingAction: Action, in: ProcessExpr) extends ProcessExpr{
  override def toString: String = s"""comm({${multiAction} -> $resultingAction}, ${in.toString})"""

  def multiAction: MultiAction = MultiAction(syncActions)

  override def getActions: Set[Action] =  in.getActions ++ syncActions.toSet ++ Set(resultingAction)
}

///**
//  * The allow operator in mcrl2
//  * @param actions actions allowed
//  * @param in process where the actions are allowed
//  */
//case class Allow(actions: List[Action], in: Operation) extends Operation{
//  override def toString: String = s"""allow({${Process.toString(actions)}}, ${in.toString})"""
//
//  override def getActions: Set[Action] = in.getActions ++ actions.toSet
//}

/**
  * the block operator in mcrl2
  * @param actions the blocked actions
  * @param in the process where the actions are blocked
  */
case class Block(actions: List[Action], in: ProcessExpr) extends ProcessExpr{
  override def toString: String = s"""block({${Process.toString(actions)}}, ${in.toString})"""

  override def getActions: Set[Action] = in.getActions ++ actions.toSet
}

/**
  * the hide operator in mcrl2
  * @param actions the actions to be hidden
  * @param in the process where the actions are hidden
  */
case class Hide(actions: List[Action], in: ProcessExpr) extends ProcessExpr{
  override def toString: String = s"""hide({${Process.toString(actions)}}, ${in.toString})"""

  override def getActions: Set[Action] = in.getActions ++ actions.toSet
}


