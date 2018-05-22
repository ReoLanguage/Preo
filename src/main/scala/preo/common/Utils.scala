package preo.common

import preo.ast._

/**
 * Created by jose on 07/07/15.
 */
object Utils {

  /**
    * Checks if a variable occurs freely in an expression.
    * @param x variable
    * @param e expression
    * @return True if x occurs freely in e
    */
  def isFree(x:Var,e:Expr): Boolean = e match {
    case `x` => false
    case Var(_) => true

    case IVal(_) => true
    case Add(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case Sub(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case Mul(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case Div(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case Sum(`x`, from, to, _) => isFree(x,from) && isFree(x,to)
    case Sum(_, from, to, e2) => isFree(x,from) && isFree(x,to) && isFree(x,e2)
    case ITE(b, ifTrue, ifFalse) => isFree(x,b) && isFree(x,ifTrue) && isFree(x,ifFalse)

    case BVal(_) => true
    case EQ(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case GT(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case LT(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case GE(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case LE(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case And(Nil) => true
    case And(e2::es) => isFree(x,e2) && isFree(x,And(es))
    case Or(e1, e2) => isFree(x,e1) && isFree(x,e2)
    case Not(e1) => isFree(x,e1)
    case AndN(`x`, from, to, _) => isFree(x,from) && isFree(x,to)
    case AndN(_, from, to, e2) => isFree(x,from) && isFree(x,to) && isFree(x,e2)
  }

  /**
    * Collects all free variables in an expression
    * @param e Expression
    * @return the collected set of variables
    */
  def freeVars(e:Expr):Set[Var] = e match {
    case x:Var => Set(x)

    case IVal(n) => Set()
    case Add(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case Sub(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case Mul(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case Div(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case Sum(x, from, to, e1) => (freeVars(e1)-x) ++ freeVars(from) ++ freeVars(to)
    case ITE(b, ifTrue, ifFalse) => freeVars(b) ++ freeVars(ifTrue) ++ freeVars(ifFalse)

    case BVal(b) => Set()
    case EQ(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case GT(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case LT(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case GE(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case LE(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case And(Nil) => Set()
    case And(e1::es) => freeVars(e1) ++ freeVars(And(es))
    case Or(e1, e2) => freeVars(e1) ++ freeVars(e2)
    case Not(e1) => freeVars(e1)
    case AndN(x,from,to,e1) => (freeVars(e1)-x) ++ freeVars(from) ++ freeVars(to)
  }

  /**
    * Collects all free variables in an interface
    * @param i Interface
    * @return the collected free variables
    */
  def freeVars(i:Interface): Set[Var] = freeVars(interfaceSem(i))

  /**
    * Variables declared in a connector
    * @param c connector
    * @return set of variables declared in lambdas and quantified exponentials
    */
  def declaredVars(c:Connector): Set[Var] = c match {
    case Abs(x, _, c1)  => declaredVars(c1) + x
    case ExpX(x, _, c1) => declaredVars(c1) + x
    case Seq(c1, c2)    => declaredVars(c1) ++ declaredVars(c2)
    case Par(c1, c2)    => declaredVars(c1) ++ declaredVars(c2)
    case Trace(i, c1)   => declaredVars(c1)
    case SubConnector(_, c1, _) => declaredVars(c1)
    case Exp(_, c1)     => declaredVars(c1)
    case Choice(_, c1, c2) => declaredVars(c1) ++ declaredVars(c2)
    case App(c1, _)     => declaredVars(c1)
    case Restr(c1, _) => declaredVars(c1)
    case _ => Set()
  }

  def usedVars(c:Connector): Set[Var] = c match {
    case Abs(_, _, c1)  => usedVars(c1)
    case ExpX(_, a, c1) => freeVars(a) ++ usedVars(c1)
    case Seq(c1, c2)    => usedVars(c1) ++ usedVars(c2)
    case Par(c1, c2)    => usedVars(c1) ++ usedVars(c2)
    case Trace(i, c1)   => freeVars(i) ++ usedVars(c1)
    case SubConnector(_, c1, _) => usedVars(c1)
    case Exp(a, c1)     => freeVars(a) ++ usedVars(c1)
    case Choice(b, c1, c2) => freeVars(b) ++ usedVars(c1) ++ usedVars(c2)
    case App(c1, e)     => freeVars(e) ++ usedVars(c1)
    case Restr(c1, phi) => freeVars(phi) ++ usedVars(c1)
    case Prim(_,i,j,_) => freeVars(i) ++ freeVars(j)
    case Symmetry(i,j) => freeVars(i) ++ freeVars(j)
    case Id(i) => freeVars(i)
  }

  /**
   * Interprets an interface as an integer expression
   * @param itf the interface to be interpreted
   * @return
   */
  def interfaceSem(itf:Interface): IExpr = itf match {
    case Tensor(i, j) => interfaceSem(i) + interfaceSem(j)
    case Port(a) => a
    case Repl(i, a) => interfaceSem(i) * a
    case Cond(b, i1, i2) => ITE(b,interfaceSem(i1),interfaceSem(i2))
  }

  /**
   * checks if a string matches the pattern of a generated variable
   */
  def isGenVar(x:String) =
    x.matches("x[0-9]+")
  /**
   * checks if a string matches the pattern of a variable renamed for alpha equivalence
   */
  def isAlphaEquivVar(x:String) =
    x.matches(".*![0-9]+$")

}
