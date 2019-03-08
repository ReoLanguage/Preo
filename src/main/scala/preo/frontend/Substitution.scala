package preo.frontend

import preo.ast.{_}
import preo.common.Utils

import scala.collection.immutable.Nil

/**
  * Created by jose on 25/05/15.
 */

/**
  * Stores a single replacement: a variable by an expression
  * @param v variable
  * @param e expression
  */
private case class Item(v:Var,e:Expr) {
  override def toString = s"${v.x} -> ${Show.apply(e)}"
}

/**
 * List of pairs (variable -> expression) that can be applied in succession.
  * Invariant: preserves expression type
  *
  * @param items pairs of (variable -> expression) to be replaced
 */
class Substitution(private val items:List[Item], private val isGeneral:Boolean = true) {

  def mkConcrete: Substitution = new Substitution(items,false)

  def +(x:Var,e:Expr): Substitution =
    new Substitution(Item(x,e)::items,isGeneral)
  def ++(that:Substitution): Substitution = {
    new Substitution(items ::: that.items,isGeneral && that.isGeneral)
  }
  def pop(x:Var): (Option[Expr],Substitution) = items match { // booleans: is an integer?
    case Nil => (None,this)
    case Item(`x`,e)::tl => (Some(e),new Substitution(tl))
    case hd::tl =>
      val (e,sub) = new Substitution(tl).pop(x)
      (e,new Substitution(hd::sub.items))
  }

  /** update all variables "x" inside the expressions by "e" */
  def update(x:Var,e:Expr): Substitution = items match {
    case Nil => this
    case Item(x2,e2)::tl => new Substitution(tl).update(x,e) + (x2,Substitution(x,e)(e2))
  }

  def compose(substitution: Substitution): Substitution =
    new Substitution((substitution.addTo(this).items ++ substitution.items).map((x:Item) => Item(x.v,Simplify(x.e))))
  def addTo(target: Substitution): Substitution = items match {
    case Nil => target
    case Item(x2,e2)::tl => new Substitution(tl).addTo(target.update(x2,e2))
  }

  def apply(exp:Expr): Expr = exp match {
    case e: IExpr => apply(e)
    case e: BExpr => apply(e)
  }
  def apply(exp:IExpr): IExpr =
    items.foldLeft(exp)((e:IExpr,i:Item) => substI(i,e))
  def apply(exp:BExpr): BExpr =
    items.foldLeft(exp)((e:BExpr,i:Item) => substB(i,e))

  def apply(itf:Interface): Interface = {
    var prev = itf
    for (i <- items)
      prev = subst(i,prev)
    prev
  }
  def apply(con:Connector): Connector = {
    var prev = con
    for (i <- items)
      prev = subst(i,prev)
    prev
  }
  def apply(typ:Type): Type = {
    var Type(args,i,j,const,genType) = typ
    for (it <- items) {
//      val vars = args.vars
      args = if (isGeneral) args else Arguments()
      // BEFORE: subst(it, args, vars) // either "ID" (if general) or "constant args" (if concrete)
      i = subst(it, i)
      j = subst(it, j)
      const = substB(it, const)
    }
    Type(args,i,j,const,isGeneral = isGeneral && genType)
  }

  /** updates a given type applying 'this' substitution to arguments, interfaces, and constraints.
    * Used only by alpha equivalence in [[TypeCheck]].  */
  def alphaEquiv(t:Type): Type = {
    var Type(args,i,j,const,genType) = t
    var vars = args.vars
    for (it <- items) {
      it match {
        case Item(v, x:Var) => vars = vars.map{case (`v`,typ) => (x,typ) case y => y}
        case _ =>
      }
      i = subst(it, i)
      j = subst(it, j)
      const = substB(it, const)
    }
    Type(Arguments(vars),i,j,const,isGeneral = isGeneral && genType)
  }

  /** substitution in expressions */
  private def subst(i:Item,exp:Expr): Expr = exp match {
      case e: IExpr => substI(i,e)
      case e: BExpr => substB(i,e)
//    case SomeVar(x) => XXX
  }

  /** substitution in boolean expressions */
  private def substB(i:Item,exp:BExpr): BExpr = exp match {
    case x:Var => i match {
      case Item(`x`, e:BExpr) => e
      case _                  => x
    }
    case BVal(_)     => exp
    case EQ(e1, e2)  => EQ(substI(i,e1),substI(i,e2))
    case GT(e1, e2)  => GT(substI(i,e1),substI(i,e2))
    case LT(e1, e2)  => LT(substI(i,e1),substI(i,e2))
    case GE(e1, e2)  => GE(substI(i,e1),substI(i,e2))
    case LE(e1, e2)  => LE(substI(i,e1),substI(i,e2))
    case And(es)     => And(es.map(substB(i,_)))
    case Or(e1, e2)  => Or(substB(i,e1),substB(i,e2))
    case Not(e1)     => Not(substB(i,e1))
    case AndN(x,f,t,e) => i match {
      case Item(`x`, e2) => exp // skip bound variable
      case _ => AndN(x,substI(i,f),substI(i,t),substB(i,e))
    }
  }
  /** substitution in int expressions */
  private def substI(i:Item,exp:IExpr): IExpr = exp match {
    case x:Var => i match {
      case Item(`x`, e:IExpr) => e
      case _                  => x
    }
    case IVal(n) => exp
    case Add(e1, e2) => Add(substI(i,e1),substI(i,e2))
    case Sub(e1, e2) => Sub(substI(i,e1),substI(i,e2))
    case Mul(e1, e2) => Mul(substI(i,e1),substI(i,e2))
    case Div(e1, e2) => Div(substI(i,e1),substI(i,e2))
    case Sum(x, from, to, e) => i match {
      case Item(`x`, e2) => exp // skip bound variable
      case _ => Sum(x, substI(i, from), substI(i, to), substI(i, e))
    }
    case ITE(b,ifTrue,ifFalse) => ITE(substB(i,b),substI(i,ifTrue),substI(i,ifFalse))
  }

  // substitution in interfaces
  private def subst(it:Item,itf:Interface): Interface = itf match {
    case Tensor(i, j) => Tensor(subst(it,i),subst(it,j))
    case Port(a) => Port(substI(it,a))
    case Repl(i, a) => Repl(subst(it,i), substI(it,a))
    case Cond(b, i1, i2) => Cond(substB(it,b),subst(it,i1),subst(it,i2))
  }

  // substitution in connectors (of free vars)
  private def subst(it:Item,con:Connector): Connector = con match {
    case Seq(c1, c2) => Seq(subst(it,c1),subst(it,c2))
    case Par(c1, c2) => Par(subst(it,c1),subst(it,c2))
    case Id(i) => Id(subst(it,i))
    case Symmetry(i, j) => Symmetry(subst(it,i),subst(it,j))
    case Trace(i, c) => Trace(subst(it,i),subst(it,c))
    case Prim(name, i, j,e) => Prim(name,subst(it,i),subst(it,j),e)
    case Exp(a, c) =>  Exp(substI(it,a),subst(it,c))
    case SubConnector(name, sub, a) => SubConnector(name, subst(it, sub), a)
    case ExpX(x, a, c) => it match {
      case Item(`x`, e) => ExpX(x,substI(it,a),c)
      case _            => ExpX(x,substI(it,a),subst(it,c))
    }
    case Choice(b, c1, c2) => Choice(substB(it,b),subst(it,c1),subst(it,c2))
    case Abs(x,et,c) => it match {
      case Item(`x`, e) => con
      case _            => Abs(x,et,subst(it,c))
    }
    case App(c, a)     => App(subst(it,c),subst(it,a))
    case Restr(c, phi) => Restr(subst(it,c),substB(it,phi))
  }

  /**
   * get extra constraints for the type after unification, from the substitution, if applicable
    *
    * @param typ type after unification
   * @return extra constraints
   */
  def getConstBoundedVars(typ:Type): BExpr = {
    var newvars = typ.args.vars.map(_._1).toSet
    var history = Set[Var]()
    var round = Set[Var]()

    var newrest:Set[BExpr] = Set()
    while (newvars.nonEmpty) {
      history ++= newvars
      round = newvars
      newvars = Set[Var]()
      for (it <- items) it match {
        case Item(v, e) =>
//          println(s"### checking if ${Show(v)} == ${Show(e)} has vars in $round.")
          if (round contains v) {
            e match {
              case ie:IExpr => newrest += (v === ie)
              case be:BExpr => newrest += (v === be)
            }
            newvars ++= (Utils.freeVars(e) -- history)
            //          println("##### yes!")
          }
      }
    }
    newrest.foldLeft[BExpr](BVal(true))(_ & _)
  }

  override def toString: String =
    (if (isGeneral) "" else "© ") +
      items.mkString("[",", ","]")

}

object Substitution {
  def apply() = new Substitution(List())
  def apply(x:Var,e:Expr) =
    new Substitution(List(Item(Var(x.x),e)))

  def replacePrim(s:String,con:Connector,by:Connector): Connector = con match {
    case Prim(name,_,_,_) if name == s => by
//    case Prim(n,i,j,Some(t:TreoLiteAST)) => Prim(n,i,j,Some(replacePrim(s,t,by)))
    case Prim(n,i,j,extra) => Prim(n,i,j,extra.map(tryToReplace(s,_,by)))
    case Seq(c1, c2)   => Seq(replacePrim(s,c1,by),replacePrim(s,c2,by))
    case Par(c1, c2)   => Par(replacePrim(s,c1,by),replacePrim(s,c2,by))
    case Trace(i, c)   => Trace(i,replacePrim(s,c,by))
    case Exp(a, c)     => Exp(a,replacePrim(s,c,by))
    case ExpX(x, a, c) => ExpX(x,a,replacePrim(s,c,by))
    case Abs(x,et,c)   => Abs(x,et,replacePrim(s,c,by))
    case App(c, a)    => App(replacePrim(s,c,by),a)
    case Restr(c, phi) => Restr(replacePrim(s,c,by),phi)
    case Choice(b, c1, c2) => Choice(b,replacePrim(s,c1,by),replacePrim(s,c2,by))
    case SubConnector(n, c, a) => SubConnector(n, replacePrim(s, c, by), a)
    case _ => con
  }

  def tryToReplace(s:String,elem:Any,by:Connector): Any = elem match {
    case t: TreoLiteAST => TreoLiteAST(t.args,t.conns.map(replacePrim(s,_,by)))
    case _ => elem
  }
//    TreoLiteAST(t.args,t.conns.map(replacePrim(s,_,by)))
  def replacePrim(s: String, t: TConnAST, by: Connector): TConnAST = t.name match {
    case Left(s2) if s2==s => TConnAST(Right(by),t.args)
    case Right(c2) => TConnAST(Right(replacePrim(s,c2,by)),t.args)
    case _ => t
  }
}


