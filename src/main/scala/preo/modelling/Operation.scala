package preo.modelling

abstract class Operation {
  override def toString: String

  def getActions: Set[Action]
}

/**
  * the sequence operator in mcrl2 (before . after) when printed
  * @param before before process
  * @param after after process
  */
case class Seq(before: Operation, after: Operation) extends Operation{
  override def toString: String = s"(${before.toString}) . (${after.toString})"

  override def getActions: Set[Action] = before.getActions ++ after.getActions
}


/**
  * the choice operator in mcrl2 (left + right) when printed
  * @param left left process
  * @param right right process
  */
case class Choice(left: Operation, right: Operation) extends Operation{
  override def toString: String = s"(${left.toString}) + (${right.toString})"

  override def getActions: Set[Action] = left.getActions ++ right.getActions
}


/**
  * the paralel operator in mcrl2 (left || right) when printed
  * @param left left process
  * @param right right process
  */
case class Par(left: Operation, right:Operation) extends Operation{
  override def toString: String = s"(${left.toString}) || (${right.toString})"

  override def getActions: Set[Action] = left.getActions ++ right.getActions
}

/**
  * creates a communication operator with the 3 actions in the tuple in the process
  * @param actions the actions to communicate
  * @param in the process where the communication will happen
  */
case class Comm(actions: (Action, Action, Action), in: Operation) extends Operation{
  override def toString: String = s"""comm({${toString(actions)}}, ${in.toString})"""

  def toString(vars: (Action, Action, Action)): String = s"""${vars._1.toString}|${vars._2.toString} -> ${vars._3.toString}"""

  override def getActions: Set[Action] =  in.getActions ++ Set(actions._1, actions._2, actions._3)
}

/**
  * The allow operator in mcrl2
  * @param actions actions allowed
  * @param in process where the actions are allowed
  */
case class Allow(actions: List[Action], in: Operation) extends Operation{
  override def toString: String = s"""allow({${Process.toString(actions)}}, ${in.toString})"""

  override def getActions: Set[Action] = in.getActions ++ actions.toSet
}

/**
  * the block operator in mcrl2
  * @param actions the blocked actions
  * @param in the process where the actions are blocked
  */
case class Block(actions: List[Action], in: Operation) extends Operation{
  override def toString: String = s"""block({${Process.toString(actions)}}, ${in.toString})"""

  override def getActions: Set[Action] = in.getActions ++ actions.toSet
}

/**
  * the hide operator in mcrl2
  * @param actions the actions to be hidden
  * @param in the process where the actions are hidden
  */
case class Hide(actions: List[Action], in: Operation) extends Operation{
  override def toString: String = s"""hide({${Process.toString(actions)}}, ${in.toString})"""

  override def getActions: Set[Action] = in.getActions ++ actions.toSet
}


