package preo.frontend

import preo.ast._
import preo.common.{TypeCheckException, Utils}

import scala.util.Random.{nextBoolean, nextInt}

/**
 * Created by jose on 18/05/15.
 */
object Eval {

  /**
    * Evaluates an interface by performing operations over known values
    *
    * @param itf interface being evaluated
    * @return
    */
  def apply(itf: Interface): Interface =
    Port(apply(Utils.interfaceSem(itf)))

  /**
    * Evaluates an expression by performing operations over known values
    *
    * @param e expression being evaluated
    * @return
    */
  def apply(e: Expr): Expr = e match {
    case ex: IExpr => apply(ex)
    case ex: BExpr => apply(ex)
  }

  /**
    * Evaluates an expression by performing operations over known values
    *
    * @param e expression being evaluated
    * @return
    */
  def apply(e: IExpr): IExpr = e match {
    case Var(_) => e

    case IVal(_) => e
    case Add(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(a), IVal(b)) => IVal(a + b)
      case (IVal(0), e3) => e3
      case (e3, IVal(0)) => e3
      case (ev1:IExpr, ev2:IExpr) => Add(ev1, ev2)
    }
    case Sub(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(a), IVal(b)) => IVal(a - b)
      case (e3, IVal(0)) => e3
      case (ev1:IExpr, ev2:IExpr) => Sub(ev1, ev2)
    }
    case Mul(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(a), IVal(b)) => IVal(a * b)
      case (IVal(0), _) => IVal(0)
      case (_, IVal(0)) => IVal(0)
      case (IVal(1), e3) => e3
      case (e3, IVal(1)) => e3
      case (ev1:IExpr, ev2:IExpr) => Mul(ev1, ev2)
    }
    case Div(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(a), IVal(b)) => IVal(a / b) // integer/eucledian division
      case (IVal(0), _) => IVal(0)
      case (_, IVal(0)) => throw new TypeCheckException("Invalid constraints: division by 0 - " + Show(Div(e1, e2)))
      case (IVal(1), _) => IVal(0) // eucledian division of 1 by an integer is always 0
      case (e3, IVal(1)) => e3
      case (ev1:IExpr, ev2:IExpr) => Div(ev1, ev2)
    }
    case Sum(x, from, to, newe) => (apply(from), apply(to), apply(newe)) match {
      case (IVal(a), IVal(b),ev:IExpr) =>
        var res: IExpr = IVal(0)
        //        println(" ## eval of "+PrettyPrint.show(e))
        //        println(s" ## sum from $a to $b")
        if (b > a)
          for (y <- a until b)
            res += Substitution(x, IVal(y))(ev)
        else // consistent with the simplification of integrals
          for (y <- a until b by -1)
            res += Substitution(x, IVal(-y))(ev)
        apply(res) // e(a) + ... + e(b)
      case (ev1:IExpr, ev2:IExpr,ev:IExpr) => Sum(x, ev1, ev2, ev)
    }
    case ITE(b, ifTrue, ifFalse) => (apply(b),apply(ifTrue), apply(ifFalse)) match {
      case (BVal(bv),itr,ifa) => if (bv) itr else ifa
      case (other:BExpr,itr:IExpr,ifa:IExpr) =>
        if (ifTrue == ifFalse) itr
        else ITE(other, itr, ifa)
    }
  }

  /**
    * Evaluates an expression by performing operations over known values
    *
    * @param e expression being evaluated
    * @return
    */
  def apply(e: BExpr): BExpr = e match {
    case Var(_) => e

    case BVal(b) => e
    case EQ(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(i1), IVal(i2)) => BVal(i1 == i2)
      case (a, b) if a == b => BVal(b = true)
      case (a:IExpr, b:IExpr) => EQ(a, b)
    }
    case GT(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(i1), IVal(i2)) => BVal(i1 > i2)
      case (a, b) if a == b => BVal(b = false)
      case (a:IExpr, b:IExpr) => GT(a, b)
    }
    case LT(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(i1), IVal(i2)) => BVal(i1 < i2)
      case (a, b) if a == b => BVal(b = false)
      case (a:IExpr, b:IExpr) => LT(a, b)
    }
    case GE(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(i1), IVal(i2)) => BVal(i1 >= i2)
      case (a, b) if a == b => BVal(b = true)
      case (a:IExpr, b:IExpr) => GE(a, b)
    }
    case LE(e1, e2) => (apply(e1), apply(e2)) match {
      case (IVal(i1), IVal(i2)) => BVal(i1 <= i2)
      case (a, b) if a == b => BVal(b = true)
      case (a:IExpr, b:IExpr) => LE(a, b)
    }
    case And(Nil) => e
    case And(e1 :: es) => (apply(e1), apply(And(es))) match {
      case (BVal(true), ev) => ev
      case (BVal(false), ev) => BVal(b = false)
      case (ev, BVal(true)) => ev
      case (ev, BVal(false)) => BVal(b = false)
      case (a:BExpr, b:BExpr) => a & b
    }
    case Or(e1, e2) => (apply(e1), apply(e2)) match {
      case (BVal(true), ev) => BVal(b = true)
      case (BVal(false), ev) => ev
      case (ev, BVal(true)) => BVal(b = true)
      case (ev, BVal(false)) => ev
      case (a:BExpr, b:BExpr) => Or(a, b)
    }
    case Not(e2) => apply(e2) match {
      case BVal(b) => BVal(!b)
      case Not(e3) => e3
      case e3:BExpr => Not(e3)
    }
    case AndN(x, f, t, e1) => (apply(f), apply(t), apply(e1)) match {
      case (IVal(a), IVal(b), e2) =>
        var res: BExpr = BVal(true)
        if (b > a)
          for (y <- a until b)
            res &= Substitution(x, IVal(y))(e2)
        else // consistent with the simplification of integrals
          for (y <- a until b by -1)
            res &= Substitution(x, IVal(-y))(e2)
        apply(res) // e(a) + ... + e(b)
      case (f2:IExpr, t2:IExpr, e2:BExpr) => AndN(x, f2, t2, e2)
    }
  }

  /**
    * Evaluates a type by performing operations over known values
    *
    * @param t type being evaluated
    * @return type after evaluation
    */
  def apply(t: Type): Type = apply(t.const) match {
    case c2: BExpr => Type(t.args, apply(t.i), apply(t.j), c2, t.isGeneral)
  }

  /**
    * Creates an instance of a type by using the constraint solver
    * and by adding default values of still undefined arguments.
    *
    * @param t type to be instantiated
    * @return instance of the type t
    */
  def instantiate(t: Type): Type = {
    val s = Solver.solve(t.const)
    if (s.isEmpty)
      throw new TypeCheckException("no solutions found for " + Show(t.const))
    val subst = expandSubstitution(t.args, s.get)
    val unchanged = (t.i == subst(t.i)) && (t.j == subst(t.j))
    apply(subst(Type(Arguments(), t.i, t.j, t.const, t.isGeneral && unchanged)))
  }

  /**
    * Adds default values to arguments (vars) to a substitution
    * that may already define some of these arguments.
    *
    * @param args variables that will be (partially) added to the substitution
    * @param s    original substitution
    * @return new (extended) substitution
    */
  def expandSubstitution(args: Arguments, s: Substitution): Substitution = {
    var subst = s
    for (vt <- args.vars) {
      vt match {
        case (x:Var,IntType) =>
          if (subst(x:Expr) == x) subst += (x, IVal(1))
        case (x:Var,BoolType) =>
          if (subst(x:Expr) == x) subst += (x, BVal(true))
      }
    }
    subst
  }

  //todo: verificar o problema do instanciate no nexrouters
  /**
    * instantiates a connector by finding a suitable substitution and applying it to the connector
    *
    * @param c
    * @return
    */
  def instantiate(c: Connector): Connector = {
    // 1 - build derivation tree
    val type1 = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst1, rest1) = Unify.getUnification(type1.const)
    // 3 - apply substitution to the type
    val rest2 = subst1(rest1)
    val type2b = Type(type1.args, subst1(type1.i), subst1(type1.j), rest2, type1.isGeneral)
    // 4 - extend with lost constraints over argument-variables
    val rest3 = subst1.getConstBoundedVars(type2b)
    val type_3 = Type(type2b.args, type2b.i, type2b.j, rest2 & rest3, type2b.isGeneral)
    // 4.1 - evaluate and simplify type
    val type4 = Simplify(type_3)
    // 5 - solve constraints
    val subst2 = Solver.solve(type4)
    if (subst2.isEmpty) throw new TypeCheckException("Solver failed")
    var subst = subst2.get //subst_1 ++ subst2.get
    if (rest3 != BVal(true)) subst = subst.mkConcrete

    var res = c
    for ((a,etype) <- type4.args.vars) {
      var (expr, subst_) = subst.pop(a)
      subst = subst_
      expr match {
        case Some(e: Expr) => res = res.apply(e)
        case None => res = if (etype==IntType) res.apply(IVal(1))
                           else                res.apply(BVal(true))
      }
    }
    subst(res)
  }

  /**
    * Finds an instance of the connector, and simplifies it
    *
    * @param c connector to be reduced
    * @return reduced connector
    */
  def reduce(c: Connector): CoreConnector = conn2CoreConn(Simplify(instantiate(c)),reduce)

  def simpleReduce(c: Connector): CoreConnector = conn2CoreConn(Simplify(c), reduce)

  /**
    * Unsafe version of [[reduce()]] - without using constraint solver, and selecting
    * default values for unsolved variables.
    *
    * @param c connector to be reduced
    * @return reduced connector
    */
  def unsafeReduce(c: Connector): CoreConnector = unsafeInstantiate(c) match {
    case Some(c2) => conn2CoreConn(Simplify.unsafe(c2),unsafeReduce)
    case None => throw new TypeCheckException("Failed to reduce connector (with unsafe operator): " + Show(c))
  }

  private def conn2CoreConn(c:Connector,red:Connector=>CoreConnector): CoreConnector = c match {
    case Seq(c1, c2) => CSeq(red(c1), red(c2))
    case Par(c1, c2) => CPar(red(c1), red(c2))
    case Id(i) => CId(reduce(i))
    case Symmetry(i, j) => CSymmetry(reduce(i), reduce(j))
    case Trace(i, c) => CTrace(reduce(i), red(c))
    case Prim(name, i, j, extra) => CPrim(name, reduce(i), reduce(j), extra)
    case SubConnector(name, sub) => CSubConnector(name, red(sub))
    case c2 => throw new TypeCheckException("Failed to reduce connector " + Show(c2))
  }

  private def reduce(i: Interface): CoreInterface = i match {
    case Tensor(i, j) => CoreInterface(reduce(i).ports + reduce(j).ports)
    case Port(IVal(n)) => CoreInterface(n)
    case _ => throw new TypeCheckException("Failed to reduce interface " + Show(i))
  }


  /**
    * Simplified version of [[instantiate()]], skipping the constraint solving phase,
    * and performing only the simplifications.
    * Used when the solver cannot be used, e.g., when producing JavaScript code.
    *
    * @param c connector
    * @return instantiated connector
    */
  def unsafeInstantiate(c:Connector): Option[Connector] = {
    // 1 - build derivation tree
    val type1 = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst2a,rest2a) = Unify.getUnification(type1.const)
    // 3 - apply substitution to the type
    val rest3a = subst2a(rest2a)
    val type3a = Simplify(Type(type1.args,subst2a(type1.i),subst2a(type1.j),rest3a,type1.isGeneral))
    // 4 - try to get intervals - if success, update substitution
//    val (interval,rest4a) = Solver.varIntIntervals(type3a.const)
    val (subst4a,rest4b) = Solver.guessSol(type3a.const)
    // update subst and type if succeeded
    val (subst4c,rest4c,type4c) = if (rest4b==BVal(true) || rest4b==And(List()))
                                       (subst2a.compose(subst4a),rest4b,subst4a(type3a))
                                  else (subst2a,rest3a,type3a)
//    throw new TypeCheckException(s"Guessed: ${(subst4c,rest4c,type4c)}")
    // 5 - extend with lost constraints over argument-variables
    val rest5a = subst4c.getConstBoundedVars(type4c)
    val type5a = Type(type4c.args,type4c.i,type4c.j,rest4c & rest5a,type4c.isGeneral)
    // 6 - evaluate and simplify type
//    val type6a = Simplify(type5a)
    // 7 - apply subst4c
    val type7a = Simplify(type5a)


    // UNSAFE -> not checking for constraint solving first. Constraints may evaluate to false.
    var subst = subst4c
    var res = c
    for ((a,etype) <- type7a.args.vars) {
      var (expr, subst_) = subst.pop(a) // a -> expr, and rest is subst_
      subst = subst_
      expr match {
        case Some(e: Expr) => // if e has free variables, give them default values, and update the substitution
          val e2 = Eval(addDefaults(e,etype))
          subst = subst.update(a, e2)
          res = res.apply(e2)
        case None => res = if (etype==IntType) res.apply(IVal(1)) else res.apply(BVal(true))
      }
    }
    val reduced = Simplify.unsafe(subst(res))

    def noFalseRestr(c: Connector): Boolean = c match {
      case Seq(c1, c2) => noFalseRestr(c1) && noFalseRestr(c2)
      case Restr(c, BVal(false)) => false
      case _ => true
    }

    Some(reduced) filter noFalseRestr
  }

  def unsafeInstantiateOld(c: Connector): Option[Connector] = {
    // 1 - build derivation tree
    val type1 = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst1, rest1) = Unify.getUnification(type1.const)
    // 3 - apply substitution to the type
    val rest2 = subst1(rest1)
    val type2b = Type(type1.args, subst1(type1.i), subst1(type1.j), rest2, type1.isGeneral)

    // 3a- solve simple constraints with intervals
    val (subst3a,rest3a) = Solver.guessSol(type2b.const)
    // 3b- apply substitution to the type
    val rest3b = Simplify(subst3a(rest3a))
    val type3b = Type(type2b.args, subst3a(type2b.i), subst3a(type2b.j), rest3b, type2b.isGeneral)
    // guessing done - now unification can fail if rest3a is not empty (guessed with partial information)
    var type3d = type2b
    var subst3e = subst1
    var rest3d = rest2
    if (rest3b == BVal(true) || rest3b == And(List())) {
      // 3c- repeat unification
      val (subst3c, rest3c): (Substitution, BExpr) = Unify.getUnification(type3b.const)
      // 3d - apply substitution to the type
      rest3d = subst3c(rest3c)
      type3d = Type(type3b.args, subst3c(type3b.i), subst3c(type3b.j), rest3c, type3b.isGeneral)
      // 3e - join substitutions
      subst3e = subst1 ++ subst3c
    }

    // 4 - extend with lost constraints over argument-variables
    val rest3 = subst3e.getConstBoundedVars(type2b)
    val type_3 = Type(type3d.args, type3d.i, type3d.j, rest3d & rest3, type3d.isGeneral)
    // 4.1 - evaluate and simplify type
    val type4 = Simplify(type_3)
    // 5 - solve constraints - SKIPPED
    //    val subst2 = Solver.solve(type4)
    //    if (subst2.isEmpty) throw new TypeCheckException("Solver failed")
    //    var subst = subst2.get //subst_1 ++ subst2.get
    //    if (rest3 != BVal(true)) subst = subst.mkConcrete

    // repeat unification

    // UNSAFE -> not checking for constraint solving first. Constraints may evaluate to false.
    var subst = subst3e

    var res = c
    for ((a,etype) <- type4.args.vars) {
      var (expr, subst_) = subst.pop(a) // a -> expr, and rest is subst_
      subst = subst_
      expr match {
        case Some(e: Expr) => // if e has free variables, give them default values, and update the substitution
          val e2 = Eval(addDefaults(e,etype))
          subst = subst.update(a, e2)
          res = res.apply(e2)
//        case Some(e: BExpr) =>
//          val e2 = Eval(addDefaultsB(e))
//          subst = subst.update(a, e2)
//          res = res.apply(e2)
        case None => res = if (etype==IntType) res.apply(IVal(1)) else res.apply(BVal(true))
//          a match {
//          case x: IVar => res = res.apply(IVal(1)) // default values
//          case x: BVar => res = res.apply(BVal(true)) // default values
//
//        }
      }
    }
    val reduced = Simplify.unsafe(subst(res))

    def noFalseRestr(c: Connector): Boolean = c match {
      case Seq(c1, c2) => noFalseRestr(c1) && noFalseRestr(c2)
      case Restr(c, BVal(false)) => false
      case _ => true
    }

    Some(reduced) filter noFalseRestr

  }

  def getInstances(c: Connector): List[Connector] = {
    val type1 = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst1, rest1) = Unify.getUnification(type1.const)
    // 3 - apply substitution to the type
    val rest2 = subst1(rest1)
    val type2b = Type(type1.args, subst1(type1.i), subst1(type1.j), rest2, type1.isGeneral)
    // 4 - extend with lost constraints over argument-variables
    val rest3 = subst1.getConstBoundedVars(type2b)
    val type_3 = Type(type2b.args, type2b.i, type2b.j, rest2 & rest3, type2b.isGeneral)
    // 4.1 - evaluate and simplify type
    val type4 = Simplify(type_3)
    // 5 - solve constraints
    var solutions = Solver.getSolutions(3, type4)
    if(solutions != null) {
      val n = if(solutions.size > 0) solutions(solutions.keys.head).length else 3
      var connectors = List() : List[Connector]
      var i = 0

      //var results = Map(): Map[Var, List[Either[IVal, BVal]]]
      while (i < n) {
        var res = c
        for ((Var(x), t) <- type4.args.vars) {
          val solution = solutions.get(x)
          if (solution.isDefined) {
            res = res.apply(solution.get.head)
            solutions = solutions + (x -> solution.get.tail)
          }
          else {
            //n solutions
            if (t == IntType) {
              res = res.apply(IVal(nextInt(4) +1))
            }
            else {
              res = res.apply(BVal(nextBoolean()))
            }
          }
        }
        connectors = res :: connectors
        i += 1
      }
      connectors
    }
    else{
      throw new TypeCheckException("Solver failed")
    }
  }

  private def addDefaults(expr: Expr,etype:ExprType): Expr = expr match {
    case Var(x) => if (etype==IntType) addDefaultsI(Var(x)) else addDefaultsB(Var(x))
    case i: IExpr => addDefaultsI(i)
    case b: BExpr => addDefaultsB(b)
  }

  private def addDefaultsI(e:IExpr, except:Set[Var] = Set()): IExpr = e match {
    case Var(x) if !except(Var(x)) => IVal(1)
    case Add(e1, e2) => Add(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case Sub(e1, e2) => Sub(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case Mul(e1, e2) => Mul(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case Div(e1, e2) => Div(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case Sum(x, from, to, e) => Sum(x,addDefaultsI(from,except),addDefaultsI(from,except),addDefaultsI(e,except+x))
    case ITE(b, ifTrue, ifFalse) => ITE(addDefaultsB(b,except),addDefaultsI(ifTrue,except),addDefaultsI(ifFalse,except))
    case _ => e
  }
  private def addDefaultsB(e:BExpr, except:Set[Var] = Set()): BExpr = e match {
    case Var(x) if !except(Var(x))=> BVal(true)
    case And(es) => And(es map (addDefaultsB(_,except)))
    case Or(e1, e2) => Or(addDefaultsB(e1,except),addDefaultsB(e2,except))
    case Not(e2) => Not(addDefaultsB(e2,except))
    case EQ(e1, e2) => EQ(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case GT(e1, e2) => GT(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case LT(e1, e2) => LT(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case LE(e1, e2) => LE(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case GE(e1, e2) => GE(addDefaultsI(e1,except),addDefaultsI(e2,except))
    case AndN(x, from, to, e2) => AndN(x, addDefaultsI(from,except), addDefaultsI(to,except), addDefaultsB(e2,except+x))
    case _ => e
  }


}