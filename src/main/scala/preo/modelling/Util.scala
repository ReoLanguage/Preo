package preo.modelling

object Util {
  def makeNodes(numbers: List[Int]): List[Mcrl2Node] = numbers match{
    case n :: rest => Mcrl2Node(n, Action.nullAction, Action.nullAction) :: makeNodes(rest)
    case Nil => Nil
  }

  def getVars(defs: List[Mcrl2Def]): Set[Action] = defs match{
    case head :: tail => head.getVars.toSet ++ getVars(tail)
    case Nil => Set()
  }
}
