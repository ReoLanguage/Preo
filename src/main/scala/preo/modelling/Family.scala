//package preo.modelling
//
//import preo.ast.{Connector, Expr, Var}
//
//abstract class Family[A]{
//  def instanciate(n: A): (Set[Action], Set[Mcrl2Def], Set[ProcessName])
//}
//
////what does a family receive
////what are the common methods
////what does it store
////how do we instanciate
//
//class IFamily(v: Var, c: Connector) extends Family[Int]
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