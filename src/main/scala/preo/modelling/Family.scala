package preo.modelling

import preo.ast.{Connector, Expr, IExpr, Var}

abstract class Family{
  def instanciate(n: Expr, processCount: Int): (List[Action], List[ProcessName], List[Process], List[ProcessName], List[Action])
}

////what does a family receive
////what are the common methods
////what does it store
////how do we instanciate
//
//class IFamily(v: Var, c: Connector) extends Family
//
//class BFamily(v: Var, c: Connector) extends Family[Boolean]
//
//class ExpFamily(exp: Expr, con: Connector) extends Family[Int]{
//
//}
//
////Like this?
//class OptFamily[A] extends Family[A]
//
//class TraceFamily extends Family[Int]
//
//class SymFamily extends Family[Int]