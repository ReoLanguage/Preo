package preo.ast

import preo.frontend.Show

/**
  * Created by jose on 26/05/15.
 */


/**
 * Type of a parameterised connector
 *
 * @param args universally quantified variables with types used by i and j
 * @param i input interface
 * @param j output interface
 * @param const constraints that must hold to be a well-typed connector
 */
case class Type(args:Arguments, i:Interface, j:Interface, const:BExpr, isGeneral:Boolean = true) {
  // hides the details to the developer/user
  override def toString = Show(this)
}

// Sometimes order is important (arguments of applications)
/**
 * Sequence of pairs "variable name" -> "Type name".
 * Similar to a context, except order matters (and it is more general with the supported types).
 *
 * @param vars pars of variables (name,type).
 */
case class Arguments(vars:List[(Var,ExprType)]) {
  def ++(that:Arguments): Arguments = Arguments(vars ::: that.vars)
  def +(that:Var,exprType: ExprType) = Arguments((that,exprType) :: vars)
  def disjoint(that:Arguments): Boolean = {
    for (v <- vars)
      if (that.vars.contains(v)) return false
    true
  }

  override def toString: String = vars.map(x=>x._1.x+":"+x._2).mkString(",")
}

// empty constructure for arguments
object Arguments{
  def apply():Arguments = Arguments(List())
}


sealed abstract class ExprType
case object IntType  extends ExprType { override def toString = "I"}
case object BoolType extends ExprType { override def toString = "B"}


