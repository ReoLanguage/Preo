package preo.frontend

import preo.ast._
import preo.common.{TypeCheckException, Utils}
//import preo.common.Utils.usedVars

object Deconstructor {

  private type Ctx = Set[(Var,ExprType)]
  private def showEt(et:(Var,ExprType)):String = Show(et._1)+":"+Show(et._2)


  /**
    * Apply a type-checker to every subconnector, to pinpoint more precisely the source of a type error.
    * @param con connector to be typechecked
    * @param checker type-checking function
    * @return type of the connector, if no error is found - othewise throw a descriptive type error.
    */
  def check(con:Connector,checker:Connector=>Option[Type]): Type = checker(con) match {
    case Some(t) => t
    case None =>
      check(Set(),List(),con,checker) // should throw an error
      Type(Arguments(),Port(IVal(0)),Port(IVal(0)),BVal(true))
  }

  private def checkExpr(ctx:Ctx,trace:List[Connector],
                        expr: Expr,checker: Connector => Option[Type]): ExprType = {
    val dummy = Id(Port(IVal(1)))
    def getDummy(con:Connector) = con match {
      case Exp(a,Id(_)) => a
      case _ => throw new TypeCheckException(s"Expected pattern id^exp in ${Show(con)}")
    }
    expr match {
      case v:Var =>
        val mbType = ctx.find(_._1 == v).map(_._2)
        if (mbType.isDefined) mbType.get
        else throw new TypeCheckException(
          s"\n - KO: variable ${v.x} not found in ${ctx.map(showEt).mkString("[",",","]")}")

      case ie: IExpr =>
        val con = Exp(ie, dummy)
        if (checker(closeVars(ctx, con)).isDefined) {
          IntType
        }
        else {
          throw new TypeCheckException(
            s"\n - KO: ${ctx.map(showEt).mkString("[", ",", "] ")}${Show(getDummy(con))} : I" +
              trace.map(x => s"\n - in: ${Show(x)}").mkString("")
          )
        }
      case be: BExpr =>
        val con = Choice(expr.asInstanceOf[BExpr],dummy,dummy)
        if (checker(closeVars(ctx, con)).isDefined) {
          BoolType
        }
        else {
          throw new TypeCheckException(
            s"\n - KO: ${ctx.map(showEt).mkString("[", ",", "] ")}${Show(getDummy(con))} : B" +
              trace.map(x => s"\n - in: ${Show(x)}").mkString("")
          )
        }
    }
//
//    try{
//
//      println(s"checking ${Show(con)} -- ${Show(closeVars(ctx,con))}")
//      if (checker(closeVars(ctx,con)).isDefined)
//        IntType
//      else
//        throw
//    }
//    catch {
//      case _:ClassCastException => {
//        val con = Choice(expr.asInstanceOf[BExpr],dummy,dummy)
//        checker(closeVars(ctx,con)) // may throw an error
//        BoolType
//      }
//      case e:Throwable => throw e
//    }
  }

  private def mbCheckExpr(ctx:Ctx, trace:List[Connector],e: Expr, checker:Connector=>Option[Type]): Option[ExprType] = {
    try { Some(checkExpr(ctx,trace,e,checker)) }
    catch {
      case _:TypeCheckException => None
      case e:Throwable => throw e
    }
  }

  /** Transforms a connector with a typed context into a lambda abstraction (to be type-checked). */
  private def closeVars(ctx:Ctx,c:Connector): Connector = {
//    val vars = usedVars(c)
    ctx.foldRight(c)((v,c2)=>Abs(v._1,v._2,c2))
  }

  private def fullCheck(ctx:Ctx,trace:List[Connector],c:Connector,
                        cs:List[Connector],es:List[Expr],checker:Connector=>Option[Type]): Unit = {
    // println(s"checking type of ${ctx.map(Show(_)).mkString("[", ",", "] ")}${Show(c)}")
    if (checker(closeVars(ctx, c)).isEmpty)
      throw new TypeCheckException(s"Failed to type check:\n - KO: ${ctx.map(showEt).mkString("[", ",", "] ")}${Show.short(c)}" +
        cs.map(x => checker(closeVars(ctx, x)) match {
          case Some(t) => s"\n - OK: ${ctx.map(showEt).mkString("[", ",", "] ")}${Show.short(x)} : ${Show(t)}"
          case None => s"\n - KO: ${Show.short(x)} -- ${Show(closeVars(ctx, x))}"
        }).mkString("") +
        es.map(e => mbCheckExpr(ctx,trace,e,checker) match {
          case Some(t) => s"\n - OK: ${ctx.map(showEt).mkString("[", ",", "] ")}${Show(e)} : ${Show(t)}"
          case None => s"\n - KO: ${Show(e)}"
        }).mkString("") +
        trace.map(x => s"\n - in: ${Show(x)}").mkString(""))
  }


  private def check(ctx:Ctx,trace: List[Connector], con:Connector, checker:Connector=>Option[Type]): Unit = con match {
    case Seq(c1,c2) =>
      check(ctx,con::trace,c1,checker)
      check(ctx,con::trace,c2,checker)
      fullCheck(ctx,trace,con,List(c1,c2),List(),checker)
    case Par(c1,c2) =>
      check(ctx,con::trace,c1,checker)
      check(ctx,con::trace,c2,checker)
      fullCheck(ctx,trace,con,List(c1,c2),List(),checker)
    case Trace(i,c) =>
      val e = Utils.interfaceSem(i)
      check(ctx,con::trace,c,checker)
      checkExpr(ctx,trace,e,checker)
      fullCheck(ctx,trace,con,List(c),List(e),checker)
    case Exp(e,c) =>
      check(ctx,con::trace,c,checker)
      checkExpr(ctx,trace,e,checker)
      fullCheck(ctx,trace,con,List(c),List(e),checker)
    case ExpX(x,e,c1) =>
      check(ctx+(x->IntType),con::trace,c1,checker)
      checkExpr(ctx,trace,e,checker)
      fullCheck(ctx+(x->IntType),trace,con,List(c1),List(e),checker)
    case SubConnector(_, c, _) =>
      check(ctx,con::trace,c,checker)
    case Choice(e,c1,c2) =>
      check(ctx,con::trace,c1,checker)
      check(ctx,con::trace,c2,checker)
      checkExpr(ctx,trace,e,checker)
      fullCheck(ctx,trace,con,List(c1,c2),List(e),checker)
    case Abs(x,t,c1) =>
      check(ctx+(x->t),con::trace,c1,checker)
      fullCheck(ctx+(x->t),trace,con,List(c1),List(),checker)
    case App(c1,e) =>
      check(ctx,con::trace,c1,checker)
      checkExpr(ctx,trace,e,checker)
      fullCheck(ctx,trace,con,List(c1),List(e),checker)
    case Restr(c1,e) =>
      check(ctx,con::trace,c1,checker)
      fullCheck(ctx,trace,con,List(c1),List(e),checker)
    case _ => {}
  }
}
