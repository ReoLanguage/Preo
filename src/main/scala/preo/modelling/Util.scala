package preo.modelling

object Util {
  /**
    * gets the actions from a list of processes
    * @param defs the processes
    * @return the list of actions
    */
  def getVars(defs: List[Mcrl2Def]): Set[Action] = defs match{
    case head :: tail => head.getVars.toSet ++ getVars(tail)
    case Nil => Set()
  }

}
