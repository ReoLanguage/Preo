package preo.frontend

import org.chocosolver.solver.constraints.Constraint
import org.chocosolver.solver.search.strategy.strategy.IntStrategy
import preo.common.TypeCheckException
//import org.chocosolver.solver.constraints.IntConstraintFactory._
//import org.chocosolver.solver.constraints.LogicalConstraintFactory._
//import org.chocosolver.solver.search.loop.monitors.SearchMonitorFactory
//import org.chocosolver.solver.search.strategy.IntStrategyFactory
import org.chocosolver.solver.variables.{BoolVar, IntVar, IVariableFactory}
import org.chocosolver.solver.{Model, Solver => CSolver}
import preo.ast._
import preo.common.Utils


/**
  * Created by jose on 07/06/15.
 */
object Solver {

  class UnhandledOperException(s:String) extends RuntimeException(s)

  private var seed = 0
  private var boolVars: scala.collection.mutable.Map[String,BoolVar] = null
  private var intVars: scala.collection.mutable.Map[String,IntVar] = null
  private var solver: CSolver = null

  private def MAX_INT:Int =  1000//VariableFactory.MAX_INT_BOUND
  private def MIN_INT:Int = 0//VariableFactory.MIN_INT_BOUND
  private def MAX_INT_TMP:Int = 1000//VariableFactory.MAX_INT_BOUND
  private def MIN_INT_TMP:Int = -1000//VariableFactory.MIN_INT_BOUND
  private def TIME_LIMIT = 5000 // 5 seconds


  /**
    * returns the number of solutions required or as many as possible
    * NOTE: need to be fixed - used by Eval.getInstances, and indirectly by Mcrl2FamilyModel.apply.
    * @param typ type used to extract constraints and relevant vars
    * @param n number of desired solutions
    * @return (boolVars, intVars) after solverAux function applies
    */

  def getSolutions(n:Int, typ: Type): List[Map[String, List[Expr]]] = {
    if (typ.const == BVal(true) || typ.const == And(List()))
      return List(Map())

    var res = List[Map[String, List[Expr]]]()
    var counter = n

    val solver = solveAux(typ.const)
    while (solver.solve() && counter >= 0) {
      var values = Map(): Map[String, List[Expr]]
      //add the values to our map
      for ((x, v) <- intVars)
        values += (x -> List())
      for ((x, v) <- boolVars)
        values += (x -> List())

      if(Math.random <= 0.4){
        for((x, v) <- intVars){
          val valueX = values(x)
          values = values +  (x -> (valueX.take(n-counter) ++ List(IVal(v.getValue)) ++ valueX.drop(n-counter) ))
        }
        for((x, v) <- boolVars){
          val valueX = values(x)
          values = values + (x -> (valueX.take(n-counter) ++ List(BVal(v.getValue == 1)) ++ valueX.drop(n-counter)) )
        }
        counter -= 1
      }
      else{
        for((x, v) <- intVars){
          values += (x -> (values(x) ++ List(IVal(v.getValue))))
        }
        for((x, v) <- boolVars){
          values += (x -> (values(x) ++ List(BVal(v.getValue == 1))))
        }
      }

      for((x, v) <- values){
        values += (x -> v.take(n))
      }
      res ::= values
    }
    res
  }

  /**
   * Solve a boolean constraint with integers using the Choco library.
    *
    * @param bExpr boolean constraint to be solved
   * @return a substitution if a solution is found, or None otherwise.
   *         The substitution is marked as "concrete" if more than 1 solution exist.
   */
  def solve(bExpr: Expr): Option[Substitution] = {
    // optimisation
    if (bExpr == BVal(true))
      return Some(Substitution())

    val solver = solveAux(bExpr)
    val solved = solver.solve()
    // build reply (substitution) and return value
    if (solved) {
      var res = Substitution()
      for ((x, v) <- intVars)
        res +=(Var(x), IVal(v.getValue))
      for ((x, v) <- boolVars)
        res +=(Var(x), BVal(v.getValue == 1))
      // a substitution is concrete if the constraints have more than 1 solution (more common)
//      if (sol.get.nextSolution())
      if (solver.solve())
        Some(res.mkConcrete)
      else Some(res)
      //      for (v <- boolVars.values ++ intVars.values)
      //        if (v.isInstantiated)
      //          println(s" - var ${v.getName} = ${v.getValue}")
      //        else
      //          println(s" * var ${v.getName} = [not instantiated]")
    } else {
      //      println(solver.getCstrs.mkString("  &\n"))
      None
    }
  }

  //TO-TEST: new "solve" method that checks if a given set of variables can take more than 1 value;
  // (by including constraints "x != subs(x)" for every relevant variable "x" and previous solution "subs".)
  // (can even go var by var, and write "forall x" or "exist x" if "x" has more solutions.)
  /**
   * EXPERIMENTAL: Same as "solve(bExpr: BExpr)", but marks the result as "concrete" only if the relevant vars are not unique.
   * The relevant vars are given by the free variables (not quantified) in the interface of the type.
   * CORRECTION: relevant vars are all vars in the interface.
   * Possible problem: second search for more solutions can be expensive!
    *
    * @param typ type used to extract constraints and relevant vars
   * @return substitution if a solution is found, or None otherwise, marked as "concrete" if applicable.
   */
  def solve(typ:Type)
      : Option[Substitution] = {
    // optimisation
    if (typ.const == BVal(true) || typ.const == And(List()))
      return Some(Substitution())

    val solver = solveAux(typ.const)
    val solved = solver.solve()
    if (solved) {
//      if (sol.isEmpty)
//        return Some(Substitution())

      var res = Substitution()
      for ((x, v) <- intVars)
        res += (Var(x), IVal(v.getValue))
      for ((x, v) <- boolVars)
        res += (Var(x), BVal(v.getValue == 1))

      if (!typ.isGeneral)
        return Some(res)

      // set concrete if negating the relevant vars yields more solutions
//      var newExp = typ.const
      var toNeg = List[BExpr]()
      val vars:Iterable[Var] = Utils.freeVars(Tensor(typ.i,typ.j)) ++ typ.args.vars.map(_._1) //-- typ.args.vars
//      println(s"#### got relevant vars: ${vars.map(Show.showVar)}")
      for (v <- vars) v match {
        case Var(x) if intVars contains x =>
            toNeg ::= EQ(Var(x),IVal(intVars(x).getValue))
        case Var(x) if boolVars contains x =>
            if (boolVars(x).getValue == 1)
              toNeg ::= Var(x)
            else
              toNeg ::= Not(Var(x))
        case _ => {}
      }
      if (vars.nonEmpty) {
        val newExp = typ.const & Not(And(toNeg))
//        println(s"#### got new expression: ${Show(newExp)}")
        val sndSolver = solveAux(newExp)
        val sndSolved = sndSolver.solve()
        if (sndSolved)
          res = res.mkConcrete
//        else println(s"#### 2nd solution not found (general).")
      }
      // return the result
      Some(res)
    }
    else
      None
  }

  private def solveAux(bExpr: Expr): CSolver = {

    seed = 0
    boolVars = scala.collection.mutable.Map[String,BoolVar]()
    intVars  = scala.collection.mutable.Map[String,IntVar]()
//..    solver = new CSolver()
//..    SearchMonitorFactory.limitTime(solver,TIME_LIMIT)
    val model: Model = new Model("bExpr")

    val c = bexpr2choco(bExpr,model)
    model.post(c)
//    solver.post(c)

    val solver = model.getSolver
    val vars = model.retrieveIntVars(true)
//    if (intVars.isEmpty && boolVars.isEmpty)
//      solver.setSearch(new IntStrategy(Array(),))

    // set strategy and finds solution
//    if (intVars.isEmpty)
//      solver.set(IntStrategyFactory.lexico_LB())
//    else
//      solver.set(IntStrategyFactory.domOverWDeg(intVars.values.toArray,0))

//    val vars = solver.retrieveIntVars()
//        if (intVars.isEmpty && boolVars.isEmpty)
//          solver.set(IntStrategyFactory.lexico_LB())
//        else
//        solver.set(IntStrategyFactory.domOverWDeg(vars,0))

//    val solution = solver.findSolution()
//
//    if (solution!=null) Some(solver)
//    else None

    solver
  }

  // get a choco variable for an internal (intermediate) variable
  private def genFreshIVar(m:Model): IntVar = genFreshIVar(MIN_INT_TMP,MAX_INT_TMP,m)
  private def genFreshIVar(from:Int,to:Int,m:Model): IntVar = {
    seed += 1
    // note: not added to list of cached variables.
    //VariableFactory.bounded("__"+(seed-1),from,to,solver)
    m.intVar(from,to) // NOTE: no name in internal variables now!
  }

//  // get a non-negative choco variable
//  private def genFreshPosIVar(): IntVar = genFreshIVar(MIN_INT,MAX_INT)

  // get a choco variable for a user-defined variable
  private def getIVar(v:String,model:Model): IntVar = {
    if (intVars contains v) intVars(v)
    else {
      val res = model.intVar(v,MIN_INT,MAX_INT)  //VariableFactory.bounded(v,MIN_INT,MAX_INT,solver)
      intVars(v) = res
      res
    }
  }
  private def getIVar(exp:IExpr,m:Model): IntVar = exp match {
    case IVal(n) => m.intVar(n) // VariableFactory.fixed(n,solver)
    case Var(x) => getIVar(x,m)
    case Add(e1, e2) => combineIExpr(e1,e2,"+",m)
    case Sub(e1, e2) => combineIExpr(e1,e2,"-",m)
    case Mul(e1, e2) => combineIExpr(e1,e2,"*",m)
    case Div(e1, e2) => combineIExpr(e1,e2,"/",m)
    case Sum(x, IVal(from), IVal(to), e) =>
      if (from < to){ // "from" did not reach "to" yet
        val e1 = Substitution(x,IVal(from)).apply(e)
        getIVar(Add(e1,Sum(x,IVal(from+1),IVal(to),e)),m)
      }
      else {
        val v = genFreshIVar(0,0,m)
//        val c = arithm(v,"=",0)
//        solver.post(c)
        v
      }
    case ITE(b, ifTrue, ifFalse) => // if b then v=intval1 else v=intval2; v
      val v = genFreshIVar(m)
      val bv = bexpr2choco(b,m)
      val ct = m.arithm(v,"=",getIVar(ifTrue,m))
      val cf = m.arithm(v,"=",getIVar(ifFalse,m))
//      val c =  ifThenElse_reifiable(bv,ct,cf)
//      m.post(c)
      m.ifThenElse(bv,ct,cf)
      v
    case Sum(_, f, t, _) =>
      throw new UnhandledOperException(s"Case not handled - neither ${Show(f)} nor ${Show(t)} can have variables, in:\n  "+Show.apply(exp))
    case _ =>
      throw new UnhandledOperException("Case not handled: "+Show.apply(exp))
  }

  private def combineIExpr(e1:IExpr,e2:IExpr,op:String,m:Model): IntVar = (e1,e2) match {
    case (IVal(i1),IVal(i2)) => // i1 'op' i2
      val v = genFreshIVar(m)
//      var c: Constraint = null
      op match {
        case "+" => m.arithm(v, "=", i1+i2).post()
        case "-" => m.arithm(v, "=", i1-i2).post()
        case "*" => m.arithm(v, "=", i1*i2).post()
        case "/" => m.arithm(v, "=", i1/i2).post()
        case _ => throw new UnhandledOperException("unexpected operator: "+op)
      }
//      solver.post(c)
      v
    case (Var(x),IVal(i)) => // x 'op' i
      val v = genFreshIVar(m)
      op match {
        case "+" | "-" =>
          m.arithm(v,"=",getIVar(x,m),op,i).post()
        case "*" =>
          m.times(getIVar(x,m),i,v).post()
        case "/" =>
          m.div(getIVar(x,m), m.intVar(i),v).post()
//          solver.post(eucl_div(getIVar(x),VariableFactory.fixed(i,solver),v))
      }
      v
    case (IVal(i),Var(x)) => // i 'op' x (3-x --> -x + 3)
      val v = genFreshIVar(m)
      op match {
        case "+" =>
          m.arithm(v,"=",getIVar(x,m),op,i).post()
        case "-" =>
          val vconst = m.intVar(i)
          m.arithm(v,"=",vconst,"-",getIVar(x,m)).post()
//          val v2 = genFreshIVar(m)
//          m.arithm(v2,"=", getIVar(x,m))
//          m.arithm(v,"=", getIVar(x,m))
//          m.arithm(v,"=",  VariableFactory.minus(getIVar(x)),"+",i))
        case "*" =>
          m.times(getIVar(x,m),i,v).post()
        case "/" =>
//          solver.post(eucl_div(VariableFactory.fixed(i,solver),getIVar(x),v))
      }
      v
    case _ => // exp1 'op' exp2
      val v = genFreshIVar(m)
      m.arithm(v,"=",getIVar(e1,m),op,getIVar(e2,m)).post()
//      op match {
//        case "+" => sum(List(getIVar(e1,m),getIVar(e2,m)).toArray,"=",v).post()
//        case "-" =>
//        case "+" => solver.post(sum(List(getIVar(e1),getIVar(e2)).toArray, v))
//        case "-" => solver.post(sum(
//          List(getIVar(e1),VariableFactory.minus(getIVar(e2))).toArray, v))
//        case "*" => solver.post(times(getIVar(e1),getIVar(e2), v))
//        case "/" => solver.post(eucl_div(getIVar(e1),getIVar(e2), v))
//      }
      v
  }

  private def getBVar(v:String,m:Model): BoolVar = {
    if (boolVars contains v) boolVars(v)
    else {
      val res = m.boolVar(v)  // VariableFactory.bool(v,solver)
      boolVars(v) = res
      res
    }
  }

  def bexpr2choco(bExpr: Expr, m: Model): Constraint = bExpr match {
    case BVal(b) => if (b) m.trueConstraint() else m.falseConstraint() // FALSE(solver)
    case Var(x) => m.arithm(getBVar(x,m).intVar(),"=",1) //getBVar(x).extension() //??
      //m.reification(getBVar(x),m.trueConstraint()) //reification_reifiable(getBVar(x),TRUE(solver))
    case And(Nil) => m.trueConstraint() //TRUE(solver)
    case And(e::es) => m.and(bexpr2choco(e,m),bexpr2choco(And(es),m))
    case Or(e1, e2) => m.or(bexpr2choco(e1,m),bexpr2choco(e2,m))
    case Not(e1) => m.not(bexpr2choco(e1,m))
    case EQ(e1,e2) => comp(e1,e2,"=","=",_==_,m)
    case GT(e1,e2) => comp(e1,e2,">","<",_>_,m)
    case LT(e1,e2) => comp(e1,e2,"<",">",_<_,m)
    case GE(e1,e2) => comp(e1,e2,">=","<=",_>=_,m)
    case LE(e1,e2) => comp(e1,e2,"<=",">=",_<=_,m)
    case AndN(_, f, t, _) =>
      throw new UnhandledOperException(s"Case not handled - neither ${Show(f)} nor ${Show(t)} can have variables, in:\n  "+Show.apply(bExpr))
    case _ =>
      throw new UnhandledOperException(s"Could not encode expression as a boolean expression (in Choco):\n  "+Show.apply(bExpr))
  }

  private def comp(e1:IExpr,e2:IExpr,op:String,revop:String,test:(Int,Int)=>Boolean,m:Model): Constraint =
    (e1,e2) match {
      case (IVal(i1), IVal(i2)) => if (test(i1,i2)) m.trueConstraint() else m.falseConstraint()
      case (Var(x), IVal(i)) => m.arithm(getIVar(x,m),op,i)
      case (IVal(i), exp) => m.arithm(getIVar(exp,m),revop,i)
      case (exp1, exp2) => m.arithm(getIVar(exp1,m),op,getIVar(exp2,m))
    }

  /// Guessing simple intervals
  sealed abstract class Interval
  case class Range(from:Option[Int],to:Option[Int]) extends Interval {
    override def toString: String = s"(${myval(from)},${myval(to)})"
    private def myval(v:Option[_]): String = v match {
      case Some(x) => x.toString
      case None => "inf"
    }
  }
  case class Bools(bools:Set[Boolean]) extends Interval

  def guessSol(bExpr: BExpr): (Substitution,BExpr) = {
    val  g = varIntIntervals(bExpr)
    val (x,rest) = g
    var res = Substitution()
    for ((v,i) <- x) i match {
      case Range(Some(n), _) => res += (v,IVal(n))
      case Range(_, Some(n)) => res += (v,IVal(n))
      case Range(None, None) => res += (v,IVal(1))
      case Bools(bs) => res += (v,BVal(bs.head)) // only non-empty options
    }
    (res,Simplify(rest))
  }

  /**
    * Tries to infer a domain interval for each variable.
    * It only considers some cases - untreated cases are kept in the second part of the return value.
    * @param bEXpr
    * @return Some(a,b) if it finds intervals in "a" and fails to treat cases in "b".
    */
  def varIntIntervals(bEXpr:BExpr): (Map[Var,Interval],BExpr) = bEXpr match {
    case Var(x) => (Map(Var(x) -> Bools(Set(true))),BVal(true))
    case Not(Var(x)) => (Map(Var(x) -> Bools(Set(false))),BVal(true))
    case And(Nil) => (Map(),BVal(true))
    case And(b::bs) =>
      val (map1,rest1) = varIntIntervals(b)
      val (map2,rest2) = varIntIntervals(And(bs))
      val map3 = mergeMap(map1,map2)
      (map3,And(List(rest1,rest2)))
    case EQ(Var(x), IVal(n)) => mkRange(x,Some(n),Some(n))
    case GT(Var(x), IVal(n)) => mkRange(x,Some(n+1),None)
    case LT(Var(x), IVal(n)) => mkRange(x,None,Some(n-1))
    case LE(Var(x), IVal(n)) => mkRange(x,None,Some(n))
    case GE(Var(x), IVal(n)) => mkRange(x,Some(n),None)
    //
    case EQ(IVal(n), Var(x)) => varIntIntervals(EQ(Var(x),IVal(n)))
    case GT(IVal(n), Var(x)) => varIntIntervals(GT(Var(x),IVal(n)))
    case LT(IVal(n), Var(x)) => varIntIntervals(LT(Var(x),IVal(n)))
    case LE(IVal(n), Var(x)) => varIntIntervals(LE(Var(x),IVal(n)))
    case GE(IVal(n), Var(x)) => varIntIntervals(GE(Var(x),IVal(n)))
    //
    case GT(Mul(IVal(n1),Var(x)),IVal(n)) =>
      if (n1>0) varIntIntervals(GT(Var(x),IVal(Math.floor((n:Float)/(n1:Float)).toInt)))
      else      varIntIntervals(LT(Var(x),IVal(Math.floor((n:Float)/(n1:Float)).toInt)))
    case LT(Mul(IVal(n1),Var(x)),IVal(n)) =>
      if (n1>0) varIntIntervals(LT(Var(x),IVal(Math.ceil((n:Float)/(n1:Float)).toInt)))
      else      varIntIntervals(GT(Var(x),IVal(Math.ceil((n:Float)/(n1:Float)).toInt)))
    case LE(Mul(IVal(n1),Var(x)),IVal(n)) =>
      if (n1>0) varIntIntervals(LE(Var(x),IVal(Math.floor((n:Float)/(n1:Float)).toInt)))
      else      varIntIntervals(GE(Var(x),IVal(Math.floor((n:Float)/(n1:Float)).toInt)))
    case GE(Mul(IVal(n1),Var(x)),IVal(n)) =>
      if (n1>0) varIntIntervals(GE(Var(x),IVal(Math.ceil((n:Float)/(n1:Float)).toInt)))
      else      varIntIntervals(LE(Var(x),IVal(Math.ceil((n:Float)/(n1:Float)).toInt)))
    case _ => (Map(),bEXpr)
  }
  private def mkRange(x:String,from:Option[Int],to:Option[Int]): (Map[Var,Interval],BExpr) =
    (Map(Var(x) -> Range(from,to)),BVal(true))
  private def mergeMap(m1:Map[Var,Interval],m2:Map[Var,Interval]) : Map[Var,Interval] =
    m1.toList./:(m2)(insertPair)
  private def insertPair(m:Map[Var,Interval],vi:(Var,Interval)): Map[Var,Interval] = {
    var changed = false
    var newmap = Map[Var,Interval]()
    for ((v2,i2) <- m) {
      if (vi._1 == v2) {
        val i3 = mergeInterval(vi._2,i2,v2.x)
        newmap += (v2 -> i3)
        changed = true
      }
      else newmap += (v2 -> i2)
    }
    if (!changed) newmap += vi
    newmap
  }
  private def mergeInterval(i1:Interval,i2:Interval,v:String): Interval = (i1,i2) match {
    case (Range(r1,r2),Range(r3,r4)) =>
      checkValidity(Range(myMax(r1,r3),myMin(r2,r4)),v)
    case (Bools(s1),Bools(s2)) =>
      val s3 = s1 intersect s2
      if (s3.isEmpty) throw new TypeCheckException(s"Incompatible boolean domains for $v:" +
        s"${s1.mkString("{",",","}")} and ${s2.mkString("{",",","}")}.")
      else Bools(s3)
    case _ => throw new TypeCheckException(s"Incompatible intervals for $v: $i1 and $i2.")
  }

  // None means -inf
  private def myMax(r1:Option[Int],r2:Option[Int]): Option[Int] = (r1,r2) match {
    case (None,_) => r2
    case (_,None) => r1
    case (Some(n1),Some(n2)) => Some(Math.max(n1,n2))
  }
  // None means +inf
  private def myMin(r1:Option[Int],r2:Option[Int]): Option[Int] = (r1,r2) match {
    case (None,_) => r2
    case (_,None) => r1
    case (Some(n1),Some(n2)) => Some(Math.min(n1,n2))
  }
  private def checkValidity(r:Range,v:String):Range = r match {
    case Range(Some(n1),Some(n2)) if n1>n2 => throw new TypeCheckException(s"Incompatible domains: fails $n1 <= $v <= $n2")
    case _ => r
  }


  /// OLD EXPERIMENTS FROM HERE
//  val p = new Parser()
//  val pt = p.parse("x + y")
//  val nd: ParseToken = Parser.DEFAULT.parse("2*a_\\mu-b_\\mu/(c*x)*x[x,y]");
//  val pt: ParseToken = Parser.DEFAULT.parse("x + y + 3 + 5 = 0");
//  val pt2: ParseToken = Parser.DEFAULT.parse("2 + 3");
//
//  println("---- token: "+pt.toString())
//  println("---- token: "+pt.toTensor.toString(OutputFormat.WolframMathematica))
//  println("---- indices: "+pt.getIndices.toArray.mkString("[",",","]"))



//  // 1. Create a Solver
////  val solver = new CSolver("my first problem");
////  // 2. Create variables through the variable factory
//  val x: IntVar = VariableFactory.bounded("X", 0, 5, solver);
//  val y: IntVar = VariableFactory.bounded("Y", 0, 5, solver);
//  val a: BoolVar = VariableFactory.bool("A", solver);
//  val b: BoolVar = VariableFactory.bool("B", solver);
////  // 3. Create and post constraints by using constraint factories
////  solver.post(IntConstraintFactory.arithm(x, "+", y, ">", 5));
////  // 4. Define the search strategy
////  solver.set(IntStrategyFactory.lexico_LB(x, y));
////  // 5. Launch the resolution process
////  solver.findSolution();
////  //6. Print search statistics
////  Chatterbox.printStatistics(solver);
////  // print solution
////  for (v <- solver.getVars)
////    println(s"${v.getName} --> $v" )
////
////  for (v <- solver.retrieveIntVars())
////    println(s"int  var ${v.getName} = ${v.getValue}")
////  for (v <- solver.retrieveBoolVars())
////    println(s"bool var ${v.getName} = ${v.getValue}")
//
//  // x == 3
//  val c1: Constraint = arithm(x,"=",3)
//  val ff: Constraint = FALSE(solver)
//  // if a==FALSE then x < y
//  val c2: Constraint = ifThen_reifiable(reification_reifiable(a,ff),arithm(x,"<",y))
//  solver.post(c1,c2)
//
//  println("has solution? "+solver.findSolution())
//  for (v <- solver.retrieveIntVars())
//    println(s"int  var ${v.getName} = ${v.getValue}")
//  for (v <- solver.retrieveBoolVars())
//    println(s"bool var ${v.getName} = ${v.getValue}")
}
