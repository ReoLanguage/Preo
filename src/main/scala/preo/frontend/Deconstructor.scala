package preo.frontend

import preo.ast._
import preo.common.TypeCheckException
//import preo.common.Utils.usedVars

object Deconstructor {
//  case class Context(vars:List[Var])

//  def subConnectors(c:Connector): List[]

  def check(con:Connector,checker:Connector=>Option[Type]): Type = checker(con) match {
    case Some(t) => t
    case None =>
      check(Set(),List(),con,checker)
      Type(Arguments(),Port(IVal(0)),Port(IVal(0)),BVal(true))
  }

  private def closeVars(ctx:Set[Var],c:Connector): Connector = {
    def getType(value: Var) = value match {
      case _:BExpr =>  BoolType
      case _       => IntType
    }
//    val vars = usedVars(c)
    ctx.foldRight(c)((v,c2)=>Abs(v,getType(v),c2))
  }

  private def fullCheck(ctx:Set[Var],trace:List[Connector],c:Connector,cs:List[Connector],checker:Connector=>Option[Type]): Unit = {
    // println(s"checking type of ${ctx.map(Show(_)).mkString("[", ",", "] ")}${Show(c)}")
    if (checker(closeVars(ctx, c)).isEmpty)
      throw new TypeCheckException(s"Failed to type check:\n - KO: ${ctx.map(Show(_)).mkString("[", ",", "] ")}${Show.short(c)}" +
        cs.map(x => checker(closeVars(ctx, x)) match {
          case Some(t) => s"\n - OK: ${ctx.map(Show(_)).mkString("[", ",", "] ")}${Show.short(x)} : ${Show(t)}"
          case None => s"\n - KO: ${Show.short(x)} -- ${Show(closeVars(ctx, x))}"
        }).mkString("") +
        trace.map(x => s"\n - in: ${Show(x)}").mkString(""))
  }

  private def check(ctx:Set[Var],trace: List[Connector], con:Connector, checker:Connector=>Option[Type]): Unit = con match {
    case Seq(c1,c2) =>
      check(ctx,con::trace, c1,checker)
      check(ctx,con::trace,c2,checker)
      fullCheck(ctx,trace,con,List(c1,c2),checker)
    case Par(c1,c2) =>
      check(ctx,con::trace, c1,checker)
      check(ctx,con::trace,c2,checker)
      fullCheck(ctx,trace,con,List(c1,c2),checker)
    case Trace(_,c) =>
      check(ctx,con::trace,c,checker)
      fullCheck(ctx,trace,con,List(c),checker)
    case Exp(_,c) =>
      check(ctx,con::trace,c,checker)
      fullCheck(ctx,trace,con,List(c),checker)
    case ExpX(x,_,c1) =>
      check(ctx+x,con::trace,c1,checker)
      fullCheck(ctx+x,trace,con,List(c1),checker)
    case SubConnector(_, c) =>
      check(ctx,con::trace,c,checker)
    case Choice(_,c1,c2) =>
      check(ctx,con::trace,c1,checker)
      check(ctx,con::trace,c2,checker)
      fullCheck(ctx,trace,con,List(c1,c2),checker)
    case Abs(x,_,c1) =>
      check(ctx+x,con::trace,c1,checker)
      fullCheck(ctx+x,trace,con,List(c1),checker)
    case App(c1,_) =>
      check(ctx,con::trace,c1,checker)
      fullCheck(ctx,trace,con,List(c1),checker)
    case Restr(c1,_) =>
      check(ctx,con::trace,c1,checker)
      fullCheck(ctx,trace,con,List(c1),checker)
    case _ => {}
  }


}
