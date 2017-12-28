package preo.modelling

abstract class Mcrl2Process{
  def vars: List[Action];
}

//atomic
//todo: turn group and state into objects
case class Action(name: String, number: Int, group: Int, state: Int) extends Mcrl2Process{
  //group 1 is for actions of the channels
  //group 2 is for actions of nodes
  //group 3 is for actions of comunication
  override def toString: String = {
    if(group > 3) "Null"
    else{
      val group_name = group match {
        case 1 => "''"
        case 2 => "'"
        case 3 => ""
      }
      val state_name = state match{
        case 1 => "in1"
        case 2 => "in2"
        case 3 => "out1"
        case 4 => "out2"
        case 5 => ""
      }
      s"$name$number$state_name$group_name"
    }
  }

  override def equals(o: scala.Any): Boolean =
    if (o == null)
      false
    else if(o.getClass != this.getClass)
      false
    else
      this.number == o.asInstanceOf[Action].get_number && this.group == o.asInstanceOf[Action].group && this.name ==o.asInstanceOf[Action].name && this.state == o.asInstanceOf[Action].state

  def get_number: Int = number

  override def vars: List[Action] = List(this)
}

object Action{
  def apply(number: Int, group: Int): Action = new Action("X", number, group, 5)
}

case class ProcessName(name: String) extends Mcrl2Process{
  override def vars: List[Action] = Nil

  override def toString: String = name
}

//operations
case class MultiAction(actions: List[Action]) extends Mcrl2Process{
  override def toString: String = toString(actions)

  private def toString(actions: List[Action]): String = actions match {
    case x :: y :: rest => x.toString + " | " + toString(y :: rest)
    case x :: Nil => x.toString
    case Nil => "Null"
  }

  def getHead: Action = if(actions.nonEmpty) actions.head else null

  def getLast: Action = if(actions.nonEmpty) actions.last else null

  override def vars: List[Action] = if(actions.isEmpty) List(Action(0, 4)) else actions

}

object MultiAction{
  def apply(actions: Action*) = new MultiAction(actions.toList)
}

case class Seq(before: Mcrl2Process, after: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"(${before.toString}) . (${after.toString})"

  override def vars: List[Action] = before.vars ++ after.vars
}

case class Choice(left: Mcrl2Process, right: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"(${left.toString}) + (${right.toString})"

  override def vars: List[Action] = left.vars ++ right.vars
}

case class Par(left: Mcrl2Process, right:Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"(${left.toString}) || (${right.toString})"

  override def vars: List[Action] = left.vars ++ right.vars
}

//maybe we can simplify this with just the number and it does the rest
case class Comm(actions: (Action, Action, Action), in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""comm({${toString(actions)}}, ${in.toString})"""

  def toString(vars: (Action, Action, Action)): String = s"""${vars._1.toString}|${vars._2.toString} -> ${vars._3.toString}"""

  override def vars: List[Action] = actions._1 :: in.vars
}

case class Allow(actions: List[Action], in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""allow({${Mcrl2Def.toString(actions)}}, ${in.toString})"""

  override def vars: List[Action] = in.vars
}

case class Block(actions: List[Action], in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""block({${Mcrl2Def.toString(actions)}}, ${in.toString})"""

  override def vars: List[Action] = in.vars
}

case class Hide(actions: List[Action], in: Mcrl2Process) extends Mcrl2Process{
  override def toString: String = s"""hide({${Mcrl2Def.toString(actions)}}, ${in.toString})"""

  override def vars: List[Action] = in.vars
}