package preo.ast

import preo.DSL
import preo.common.TypeCheckException
import preo.frontend.Show

sealed abstract class Connector {
  // helpers for the DSL
  def &(that:Connector) = Seq(this,that)
  def *(that:Connector) = Par(this,that)
  def ^(that:IExpr) = Exp(that,this)
  def ^(x:DSL.TypedVar, that:IExpr) = ExpX(Var(x.getName),that,this)
  def ^(ew:DSL.ExpWrap) = ExpX(Var(ew.x.getName),ew.to,this)
  def :^(that:IExpr) = Exp(that,this)           // experimenting with precedence
  def :^(x:Var,that:IExpr) = ExpX(x,that,this)  // experimenting with precedence
  def :^(ew:DSL.ExpWrap) = ExpX(Var(ew.x.getName),ew.to,this)    // experimenting with precedence
  def apply(that:Expr): Connector = App(this,that)
  def |(phi:BExpr): Connector = Restr(this,phi)
  def |+|(that:Connector) = Abs(Var("$"),BoolType,Choice(Var("$"),this,that))

  // hides the details to the developer/user
  override def toString = try {
    Show(this) + "\n   : "+Show(DSL.unsafeTypeOf(this)._1) // note: using lightweight version in toString
  } catch {
    case e: TypeCheckException => Show(this)+ "\n   ! Type error: "+e.getMessage
  }
}

case class Seq(c1:Connector, c2:Connector) extends Connector
case class Par(c1:Connector, c2:Connector) extends Connector
case class Id(i:Interface) extends Connector
case class Symmetry(i:Interface,j:Interface) extends Connector
case class Trace(i:Interface,c:Connector) extends Connector
case class Prim(name:String,i:Interface,j:Interface,extra:Set[Any]=Set()) extends Connector

case class SubConnector(name:String, c1:Connector, annotations: List[Annotation]) extends Connector


case class Exp(a:IExpr, c:Connector) extends Connector
case class ExpX(x:Var, a:IExpr, c:Connector) extends Connector
case class Choice(b:BExpr, c1:Connector, c2:Connector) extends Connector
case class Abs(x:Var,et:ExprType, c:Connector) extends Connector
case class App(c:Connector, a:Expr) extends Connector

case class Restr(c:Connector,phi:BExpr) extends Connector