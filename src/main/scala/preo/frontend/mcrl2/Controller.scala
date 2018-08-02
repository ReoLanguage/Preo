package preo.frontend.mcrl2

import preo.ast._
import preo.common.Utils

object Controller {

  def apply(con: Connector): Model = {
    val fam = conToFamily(con)
    val counter: Counter = new Counter
    val processes = fam.processes(counter, Map(), false)
    val families = fam.families

    if(fam.size == 1) {
      val (_, names_in, procs, names_out, _) = processes.head
      val inits = (names_in ++ names_out).toSet
      val init: Operation =
        if (inits.isEmpty) ProcessName("delta")
        else
          inits.tail.foldRight(inits.head.asInstanceOf[Operation])((a, b) => preo.frontend.mcrl2.Par(a, b))

      new Model(procs, init)
    }
    else{
      val variables = Utils.declaredVars(con)
      val family_values = variables.foldRight(Map():Map[Var, List[Int]])((v, m) => m.updated(v, (1 to 5).toList))

      val manager_vars: Map[Int, Action] = families.map(f => {
        val a = Action.controllerAction(counter.getSyncCount1)
        counter.incrementSyncCount1(1)
        f._1 -> a
      })

      val sync_vars: Map[Int, Action] = families.map(f => {
        val a = Action.syncAction(counter.getSyncCount2)
        counter.incrementSyncCount2(1)
        f._1 -> a
      })


      val manager = Manager(makeManagerActions(fam, family_values, manager_vars, Map()))

      
    }
  }

//  //todo: later
//  def generate_Model(ccon: CoreConnector): Model = ???
// type is necessary so we know how to instanciate
  def conToFamily(con: Connector): Family = con match{
    case preo.ast.Seq(c1, c2) => SeqFamily(conToFamily(c1), conToFamily(c2))

    case preo.ast.Par(c1, c2) => ParFamily(conToFamily(c1), conToFamily(c2))

//      todo: test after app
//    case Id(Port(i)) => IdFamily(i)

//    case Symmetry(pi, pj) => ???

    //    case Trace(i, c) => ???
    case p@Prim(_, _, _, _) => PrimFamily(p)
//    case SubConnector(name, c, _) => ???

    case Exp(a, c) => ExpFamily(a, conToFamily(c))
//
//
//    case ExpX(x, a, c) => ???
//    case Choice(b, c1, c2) =>
    case Abs(x, et, c)=> AbsFamily(x, et, conToFamily(c))
//    case App(c, a) => conToChannels(c, typ, family)
//
//    case Restr(c, phi) => conToChannels(c, typ, family)
  }


  private def makeManagerActions(family: Family, ranges: Map[Var, List[Int]], managerActions: Map[Int, Action], varToInt: Map[Var, Int]): List[MultiAction] = {
    if(ranges.isEmpty) MultiAction(family.choices(varToInt).map(n => managerActions(n)).toList) :: Nil
    else ranges.head._2.flatMap(i => makeManagerActions(family, ranges.tail, managerActions, varToInt.updated(ranges.head._1, i)))
  }

}
