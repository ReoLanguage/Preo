package preo.frontend

import preo.ast._
import preo.common.TypeCheckException
import preo.common.Utils._

/**
 * Created by jose on 17/05/15.
 */
object TypeCheck {

  // set of variables with their types
  private class Context {
    protected val ints: Set[String]  = Set()
    protected val bools: Set[String] = Set()
    protected val conns: Map[String,(IExpr,IExpr)] = Map()
    private def build(i:Set[String],b:Set[String],c:Map[String,(IExpr,IExpr)]) = new Context {
      override val ints = i
      override val bools = b
      override val conns = c
    }

    /** checks if a variable is in the context. */
    def contains(variable:String): Boolean =
      (ints contains variable) || (bools contains variable) || (conns contains variable)
    /** checks if a variable is in the context. */
    def apply(v:Var): Boolean = (ints contains v.x) || (bools contains v.x)
    /** Check if 2 contexts are disjoint */
    def disjoint(other:Context): Boolean =
      (ints  & other.ints)  == Set() &
      (bools & other.bools) == Set() &
      (conns.keySet & other.conns.keySet) == Set()
//    def ++(other:Context): Context =
//      if (disjoint(other)) build(ints++other.ints, bools++other.bools)
//        else throw new TypeCheckException(s"Non-disjoint contexts:\n - $this\n and\n - $other")
    def addInt(v:String): Context = {
      assert(!ints(v), s"Context already contains int variable $v (vars: $ints)")
      build(ints + v, bools, conns)
    }
    def addBool(v:String): Context = {
      assert(!bools(v), s"Context already contains bool variable $v (vars: $bools)")
      build(ints, bools + v, conns)
    }

    def addVar(v:Var,et: ExprType): Context = et match {
      case IntType  => addInt(v.x)
      case BoolType => addBool(v.x)
    }

    /** Number of variables. */
    def size: Int = ints.size + bools.size + conns.size

    override def toString: String =
      "["+bools.map(_+":Bool").mkString(",") +
        (if (bools.nonEmpty) ",") +
         ints.map(_+":Int").mkString(",") +
        (if (conns.nonEmpty) ",") +
        conns.map(_+":Conn").mkString(",") +
    "]"
  }


  private var seed = 0
  private def fresh() = {seed += 1; "x"+seed}
  private def fresh(base:Var) = {seed += 1; base.x+"!"+seed}

  /**
   * Finds a type (and constraints) by building a type derivation tree.
    *
    * @param con connector to be type checked
   */
  def check(con:Connector): Type = {
    seed = 0
    check(new Context,con)
  }

  private def nonNeg(e:IExpr): BExpr = e >= IVal(0)
  private def nonNeg(e1:IExpr,e2:IExpr): BExpr =
    nonNeg(e1) & nonNeg(e2)
  private def nonNeg(i:Interface): BExpr = nonNeg(interfaceSem(i))
  private def nonNeg(i1:Interface,i2:Interface): BExpr =
    nonNeg(i1) & nonNeg(i2)

  private def check(gamma:Context, con:Connector, postVisit:Connector => Unit = _=>{}): Type =con match {
    case Seq(c1, c2) =>
      val t1@Type(args1,i1,j1,phi1,isG1) = check(gamma,c1)
      val Type(args2,i2,j2,phi2,isG2) = alphaEquiv(t1,check(gamma,c2))
//      if (!(args1 disjoint args2))
//        throw new TypeCheckException(s"arguments of ${Show(c1)} and ${Show(c2)} are not disjoint.")
      Type(args1 ++ args2, i1, j2, EQ(interfaceSem(j1),interfaceSem(i2)) & phi1 & phi2, isG1 && isG2)
    case Par(c1, c2) =>
      val t1@Type(args1,i1,j1,phi1,isG1) = check(gamma,c1)
      val Type(args2,i2,j2,phi2,isG2) = alphaEquiv(t1,check(gamma,c2))
//      if (!(args1 disjoint args2))
//        throw new TypeCheckException(s"arguments of ${Show(c1)} and ${Show(c2)} are not disjoint.")
      Type(args1 ++ args2, i1 * i2, j1 * j2, phi1 & phi2, isG1 && isG2)
    case Id(i:Interface) =>
      Type(Arguments(), i, i, BVal(b=true))
    case Symmetry(i, j) =>
      Type(Arguments(), i*j, j*i, BVal(b=true))
    case Trace(i, c) =>
      val Type(args,i1,j1,phi,isG) = check(gamma,c)
      val x = Port(Var(fresh())) // gen unique name
      val y = Port(Var(fresh())) // gen unique name
      Type(args, x, y, EQ(interfaceSem(x * i), interfaceSem(i1)) &
                       EQ(interfaceSem(y * i), interfaceSem(j1)) &
                       nonNeg(x,y) &
                       phi, isG)
    case Prim(name,i,j,_) =>
      check(gamma,interfaceSem(i))
      check(gamma,interfaceSem(j))
      Type(Arguments(), i, j, nonNeg(i,j), isGeneral=true)
    case Exp(a, c) =>
      check(gamma,a)
      val Type(args,i,j,phi,isG) = check(gamma,c)
      Type(args, Repl(i,a), Repl(j,a), nonNeg(a) & phi,isG)
    // ExpX is a TRICKY CASE - add complex constraint!
    //  - c^(x<a) imposes a>=0
    //  - c^(x<a) imposes, for each constr. b of c, that "And_{v<a} b.[v/x]"
    case ExpX(x, a, c) =>
      check(gamma,a)
      val (Type(args,i,j,phi,isG),newx) = checkAndAddVar(gamma,x,IntType,c) //check(gamma.addVar(x),c)
                      // phi may contain "x" - need to replace it by all its possible values.
      val phi2 = AndN(newx,IVal(0),a,phi)
      val ci = Sum(newx,IVal(0),a,interfaceSem(Eval(i))) // 0<=x<a
      val cj = Sum(newx,IVal(0),a,interfaceSem(Eval(j)))
      Type(args, Port(ci), Port(cj), /*nonNeg(newi,newj)*/ nonNeg(a) & phi2,isG)
    // END OF TRICKY CASE

    case SubConnector(_, c, _) => check (gamma, c)


    case Choice(b, c1, c2) =>
      val Type(args1,i1,j1,phi1,isG1) = check(gamma,c1)
      val Type(args2,i2,j2,phi2,isG2) = check(gamma,c2)
      check(gamma,b)
      Type(args1++args2, Cond(b,i1,i2), Cond(b,j1,j2), phi1 & phi2,isG1 && isG2)
    case Abs(x,et, c) =>
      val (Type(args,i,j,phi,isG),newx) = checkAndAddVar(gamma,x,et,c) //check(gamma.addVar(x),c)
      Type(args + (newx,et) ,i,j,phi,isG)
    case App(c, a) =>
      val Type(args,i,j,phi,isG) = check(gamma,c)
      val isInt = isIExpr(gamma,a)
      args.vars match {
        case Nil =>
          throw new TypeCheckException(s"application: ${Show(c)} is applied to ${Show(a)} but it does not expect arguments")
        case (x,IntType)::xs if isInt =>
          val s = Substitution(x, a)
          Type(Arguments(xs),s(i),s(j),s(phi),isG)
        case (x,BoolType)::xs if !isInt =>
          val s = Substitution(x, a)
          Type(Arguments(xs),s(i),s(j),s(phi),isG)
        case (Var(x),et)::_ =>
          throw new TypeCheckException(s"application: expected '${if (isInt) "Int" else "Bool"}', found $x : $et.")
      }
    case Restr(c,phi) =>
      check(gamma,phi)
      val Type(args,i,j,psi,isG) = check(gamma,c)
      Type(args,i,j,psi & phi,isG)
  }

  def isIExpr(gamma:Context,a:Expr): Boolean = a match {
    case Var(x) => gamma(Var(x))
    case _: IExpr => true
    case _: BExpr => false
  }

  def check(gamma:Context,a:Expr):Unit = a match {

    case v@Var(x)   => if (!gamma(v)) throw new TypeCheckException(s"$x not in the context ($gamma)")

    case IVal(_)     =>
    case Add(e1, e2) => check(gamma,e1); check(gamma,e2)
    case Sub(e1, e2) => check(gamma,e1); check(gamma,e2)
    case Mul(e1, e2) => check(gamma,e1); check(gamma,e2)
    case Div(e1, e2) => check(gamma,e1); check(gamma,e2)
    case Sum(x,from,to,e) => check(gamma,from) ; check(gamma,to) ; checkAndAddVar(gamma,x,IntType,e) //check(gamma.addVar(x),e)
    case ITE(b,ift,iff)   => check(gamma,b) ; check(gamma,ift) ; check(gamma,iff)

    case BVal(_)     =>
    case EQ(e1, e2)  => check(gamma,e1); check(gamma,e2)
    case GT(e1, e2)  => check(gamma,e1); check(gamma,e2)
    case LT(e1, e2)  => check(gamma,e1); check(gamma,e2)
    case GE(e1, e2)  => check(gamma,e1); check(gamma,e2)
    case LE(e1, e2)  => check(gamma,e1); check(gamma,e2)
    case And(Nil)    =>
    case And(e::es)  => check(gamma,e); check(gamma,And(es))
    case Or(e1, e2)  => check(gamma,e1); check(gamma,e2)
    case Not(e1)     => check(gamma,e1)
    case AndN(x,from,to,e) => check(gamma,from) ; check(gamma,to) ; checkAndAddVar(gamma,x,IntType,e) //check(gamma.addVar(x),e)
  }


  private def alphaEquiv(t1:Type,t2:Type): Type = {
    val rep = t1.args.vars intersect t2.args.vars
    var sub = Substitution()
    for ((x,et) <- rep) x match {
      case v => sub += (v,Var(fresh(x)))
    }
    sub.alphaEquiv(t2)
  }

  // Checks if `gamma,x |- c`, returns its type and a rename for `x` if `x` already exists in `gamma`.
  private def checkAndAddVar(gamma:Context,x:Var,et: ExprType,c:Connector): (Type,Var) = {
    if (gamma contains x.x) {
      val y = Var(fresh(x))
      (check(gamma.addVar(y,et),Substitution(x,y)(c)),y)
    }
    else
      (check(gamma.addVar(x,et),c),x)
  }
  private def checkAndAddVar(gamma:Context,x:Var,et:ExprType,e:Expr): Unit = {
    if (gamma contains x.x) {
      val y = Var(fresh(x))
      e match {
        case e2:IExpr => check(gamma.addVar(y,et),Substitution(x,y)(e2))
        case e2:BExpr => check(gamma.addVar(y,et),Substitution(x,y)(e2))
        case _ =>
      }
    }
    else
    check(gamma.addVar(x,et), e)
  }
}
