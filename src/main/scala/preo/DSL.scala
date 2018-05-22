package preo

import java.io.File

import ast.{Var, _}
import frontend._
import lang.Parser
import common.TypeCheckException

import scala.language.implicitConversions
import scala.util.control.NonFatal

/**
 * Created by jose on 17/05/15.
 */
object DSL {
  // goal: e.g., to write "fifo" * id & id^2
  implicit def str2conn(s:String): Connector = Prim(s,1,1)
  implicit def str2Var(s:String): ast.Var = ast.Var(s)
  implicit def bool2BExp(b:Boolean): BExpr= BVal(b)
  implicit def int2IExp(n:Int): IExpr= IVal(n)
  implicit def int2Interface(n:Int): Interface = Port(IVal(n))
  implicit def exp2Interface(e:IExpr): Interface= Port(e)

  implicit def str2B(s:String): B = B(s)
  implicit def str2I(s:String): I = I(s)
  implicit def b2BExpr(b:B):BExpr = Var(b.x)
  implicit def i2IExpr(i:I):IExpr = Var(i.x)
  implicit def i2Interface(i:I):Interface = Port(ast.Var(i.x))

  sealed abstract class TypedVar {
    def getName:String
    def unary_! = LamWrap(List(this)) // helper to DSL (lambdas: !x -> conn)
    def <--(to:IExpr) = ExpWrap(this,to) // helper to DSL
    def =<(to:IExpr)  = ExpWrap(this,to) // helper to DSL
  }
  case class I(x:String) extends TypedVar {def getName: String = x}
  case class B(x:String) extends TypedVar {def getName: String = x}

  // helper for DSL
  case class ExpWrap(x:TypedVar, to:IExpr)
  case class LamWrap(vs:List[TypedVar]) { // !x - y -> conn
    def ->(c:Connector): Connector = vs match {
      case Nil => c
      case (I(v)::t) => DSL.lam(v,IntType,LamWrap(t)->c)
      case (B(v)::t) => DSL.lam(v,BoolType,LamWrap(t)->c)
    }
    def -(v2:TypedVar): LamWrap =  LamWrap(vs:::List(v2))
    def -(v2c:(TypedVar,Connector)): Connector = LamWrap(vs:::List(v2c._1)) -> v2c._2
  }

  type Itf = Interface
  def lam(v:String,et:ExprType,c:Connector): Connector =  Abs(Var(v),et,c)
  def lam(v:TypedVar, c:Connector): Connector = v match {
    case I(name) => Abs(Var(name),IntType,c)
    case B(name) => Abs(Var(name),BoolType,c)
  }
  def not(b:BExpr) = Not(b)

  def sym(i1: IExpr,i2: IExpr)  = Symmetry(i1,i2)
  def sym(i1: Interface,i2: Interface)  = Symmetry(i1,i2)
  val Tr   = Trace
//  val Prim = preo.ast.Prim

  val one:Itf = 1
  val swap = Symmetry(1,1)
  val id = Id(1)
  val nil = Id(0)
  val fifo = Prim("fifo",1,1)
  val fifofull = Prim("fifofull",1,1)
  val lossy = Prim("lossy",1,1)
  val dupl = Prim("dupl",1,2)
  val merger = Prim("merger",2,1)
  val drain = Prim("drain",2,0)

  // included for the demo at FACS'15
  val x:I="x"; val y:I="y"; val z:I="z"; val n:I="n"; val b:B="b"; val c:B="c"

  // methods to (instantiate and) simplify a connector //
  /**
    * Instantiate (make concrete) and simplify a connector
    *
    * @param c connector to be  simplified
    */
  def simplify(c:Connector) = Simplify(c)


  /**
    * Build a dot-graph of a connector
    *
    * @param c connector
    * @return dot graph
    */
  def draw(c:Connector) = backend.Dot(Eval.reduce(c))

  /**
    * Build an html graph of a connector that uses the Springy JavaScript library
    * (http://getspringy.com)
    *
    * @param c connector
    * @param file file name to output the html document
    */
  def genHTML(c:Connector, file:String): Unit = backend.Springy.toFile(Eval.reduce(c),new File(file))

  /**
    * Parses a string into a connector.
    * @param s string representing a connector
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parseWithError(s:String): Parser.ParseResult[Connector] = Parser.parse(s)

  /**
    * Parses a string into a connector.
    * @param s string representing a connector
    * @return parsed connector
    */
  def parse(s:String): Connector =  Parser.parse(s) match {
    case Parser.Success(result, next) => result
    case f: Parser.NoSuccess => throw new TypeCheckException("Parser failed: "+f)
  }

  // overall methods to typecheck
  /**
   * Type check a connector (build tree, unify, and solve constraints)
    *
    * @param c connector to be type-checked
   * @return the (general) type of the connector - if constraint solving gives a concrete type, return the general type instead.
   */
  def typeOf(c:Connector): Type = {
    // 1 - build derivation tree
    val type1 = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst1,rest1) = Unify.getUnification(type1.const)
    // 3 - apply substitution to the type
    val rest2 = subst1(rest1)
    val type2b = Type(type1.args,subst1(type1.i),subst1(type1.j),rest2,type1.isGeneral)
    // 4 - extend with lost constraints over argument-variables
    val rest3 = subst1.getConstBoundedVars(type2b)
    val type3 = Type(type2b.args,type2b.i,type2b.j,rest2 & rest3,type2b.isGeneral)
    // 4.1 - evaluate and simplify type
    val type4 = Simplify(type3)
    // 5 - solve constraints
    val subst2 = Solver.solve(type4)
    val subst3 = subst2 match {
      case None => throw new TypeCheckException("No solution found: " + Show(type4.const))
      case Some(s2) => if (rest3 == BVal(true)) s2
                       else s2.mkConcrete
    }
    // 6 - apply subst3
    val type5 = subst3(type4)
    val rest4 = subst3.getConstBoundedVars(type5)
    val type6 = Eval(Type(type5.args,type5.i,type5.j,type5.const & rest4,type5.isGeneral))
    // 7 - return type from solver ONLY if it is general
    if (type6.isGeneral)
      type6
    else type4
  }

  /**
    * Type check a connector WITHOUT the constraint solving - only using unification and simplifications.
    * @param c connector to be type-checked
    * @return the inferred type after simplifications and unification, and the constraint that still need to be checked.
    */
  def unsafeTypeOf(c:Connector): (Type,BExpr) = {
    // 1 - build derivation tree
    val type1 = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst1,rest1) = Unify.getUnification(type1.const)
    // 3 - apply substitution to the type
    val rest2 = subst1(rest1)
    val type2b = Simplify(Type(type1.args,subst1(type1.i),subst1(type1.j),rest2,type1.isGeneral))
    // 3b -
    val (_,leftovers1) = Solver.guessSol(type2b.const) // will throw an error if inconsistent
    val leftovers2= Simplify(leftovers1)
    val leftovers3 = if (leftovers2==And(List())) BVal(true) else leftovers2
    // 4 - extend with lost constraints over argument-variables
    val rest3 = subst1.getConstBoundedVars(type2b)
    val type3 = Type(type2b.args,type2b.i,type2b.j,rest2 & rest3,type2b.isGeneral)
    // 4.1 - evaluate and simplify type
    val type4 = Simplify(type3)
    // return the final type, without solving the missing constraints
    (type4,leftovers3)
  }

  /**
   * Type check a connector (build tree, unify, and solve constraints)
    *
    * @param c connector to be type-checked
   * @return the type of the connector - return a concrete type if one is found, otherwise the general one
   */
  def typeInstance(c:Connector): Type = {
    // 1 - build derivation tree
    val type1 = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst1,rest1) = Unify.getUnification(type1.const)
    // 3 - apply substitution to the type
    val rest2 = subst1(rest1)
    val type2b = Type(type1.args,subst1(type1.i),subst1(type1.j),rest2,type1.isGeneral)
    // 4 - extend with lost constraints over argument-variables
    val rest3 = subst1.getConstBoundedVars(type2b)
    val type3 = Type(type2b.args,type2b.i,type2b.j,rest2 & rest3,type2b.isGeneral)
    // 4.1 - evaluate and simplify type
    val type4 = Simplify(type3)
    // 5 - solve constraints
    val subst2 = Solver.solve(type4)
    if (subst2.isEmpty) throw new TypeCheckException("Solver failed")
    else if (rest3 != BVal(true))
      Eval.instantiate(subst2.get.mkConcrete(type4))
    else
      Eval.instantiate(subst2.get(type4))
  }

  /**
   * Type check a connector (build tree, unify, and solve constraints)
    *
    * @param c connector to be type-checked
   * @return a substitution used applied to the derivation tree to get an instance of a type
   */
  def typeSubstitution(c:Connector): Substitution = {
    // 1 - build derivation tree
    val oldtyp = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst,rest) = Unify.getUnification(oldtyp.const)
    // 3 - apply substitution to the type
    val tmptyp = subst(oldtyp)
    val newrest = subst.getConstBoundedVars(oldtyp)
    val typ = Type(tmptyp.args,tmptyp.i,tmptyp.j,tmptyp.const & newrest,tmptyp.isGeneral)
    // 4 - evaluate (and simplify) resulting type (eval also in some parts of the typecheck).
    val typev = Simplify(typ)
    // 5 - solve rest of the constraints
    val s = Solver.solve(typev.const)
    if (s.isEmpty)
      throw new TypeCheckException("Solver failed: no solutions found for "+Show(typev.const))
    val moreSubst = Eval.expandSubstitution(typev.args,s.get)
    val unchanged = (typev.i == moreSubst(typev.i)) && (typev.j == moreSubst(typev.j))
//    println(s"type unchanged ${Show(typev)} with $s")
    if (!(typev.isGeneral && unchanged))
      subst ++ moreSubst.mkConcrete
    else
      subst ++ moreSubst
  }

  /**
   * Build the derivation tree of a connector (if it exists)
    *
    * @param c connector from which the tree is built
   * @return type representing the tree
   */
  def typeTree(c:Connector): Type = {
    // 1 - build derivation tree
    val typ = TypeCheck.check(c)
    // evaluate (simplify) without substituting
//    Eval(typ)
    typ
  }

  /**
   * Type-check a connector just using unification (no constraint solving).
    *
    * @param c connector to be type-checked
   * @return type after unification
   */
  def typeUnify(c:Connector): Type = {
    // 1 - build derivation tree
    val oldtyp = TypeCheck.check(c)
    // 2 - unify constraints and get a substitution
    val (subst,rest) = Unify.getUnification(oldtyp.const)
    // 3 - apply substitution to the type
    val tmptyp = subst(oldtyp)
    val newrest = subst.getConstBoundedVars(oldtyp)
    val typ = Type(tmptyp.args,tmptyp.i,tmptyp.j,tmptyp.const & newrest,tmptyp.isGeneral)
    // 4 - evaluate (and simplify) resulting type (eval also in some parts of the typecheck).
    Simplify(typ)
  }

  /**
    * Type check a connector (build tree, unify, and solve constraints), and print all intermediate results
    *
    * @param c connector to be type-checked
    * @return the type of the connector - return a concrete type if one is found, otherwise the general one
    */
  def debug(c:Connector): Unit = {
    try{
      println(Show(c))
      // 1 - build derivation tree
      val type1 = TypeCheck.check(c)
      println(s" - type-rules:    $type1")
      // 2 - unify constraints and get a substitution
      val (subst1,rest1) = Unify.getUnification(type1.const)
      println(s" - [ unification: $subst1 ]")
      println(s" - [ missing:     ${Show(rest1)} ]")
      // 3 - apply substitution to the type
      val rest2 = subst1(rest1)
      val type2b = Type(type1.args,subst1(type1.i),subst1(type1.j),rest2,type1.isGeneral)
      println(s" - substituted:   $type2b")
      // 4 - extend with lost constraints over argument-variables
      val rest3 = subst1.getConstBoundedVars(type2b)
      val type3 = Type(type2b.args,type2b.i,type2b.j,rest2 & rest3,type2b.isGeneral)
      println(s" - extended with: $rest3")
      // 4 - evaluate and simplify type
      val type4 = Simplify(type3)
      println(s" - simplified:    $type4")
      // 5-alpha
      val (interval,rest23) = Solver.varIntIntervals(type2b.const)
      println(s" - got interval (pre-extension): ${interval.mkString("/")} - ${Show(rest23)}")
      val (subst12,rest22) = Solver.guessSol(type2b.const)
      println(s" - guessing solution: $subst12 - ${Show(rest22)}")
      // 5 - solve constraints
      val subst2 = Solver.solve(type4)
      val subst3 =
        if (subst2.isEmpty) throw new TypeCheckException("Solver failed")
        else if (rest3 == BVal(true)) subst2.get
        else subst2.get.mkConcrete
      println(s" - [ solution:    $subst3 ]")
      // 6 - apply subst3 if solver is an abstract solution
      val type5 = subst3(type4)
      if (type5.isGeneral) {
        val rest4 = subst3.getConstBoundedVars(type5)
        println(s" - extended with: $rest4")
        val type6 = Eval(Type(type5.args, type5.i, type5.j, type5.const & rest4, type5.isGeneral))
        println(s" - post-solver:   $type6")
      }
      else println(s" - solution yields a concrete instance only.")
      // 7 - apply the new substitution to the previous type and eval
      val type5b = Eval.instantiate(subst3(type4))
      println(s" - instantiation: $type5b")
      // 8 - show final type
      println(" : "+Show(typeOf(c)))
    }
    catch {
      case e:TypeCheckException => println(s" ! type checking error: ${e.getMessage}")
      case NonFatal(e) => throw e
//      case x : Throwable => throw x
    }
  }

  /**
    * Parses a connector, and produces several intermediate steps when type checking and instantiating
    * @param c connector to be parsed and analysed
    */
  def debug(c:String): Unit = debug(parse(c))

  def unsafeDebug(c:String): Unit = unsafeDebug(parse(c))

  def unsafeDebug(c:Connector): Unit = try {
    println(Show(c))
    // 1 - build derivation tree
    val type1 = TypeCheck.check(c)
    println(s" - type-rules:    $type1")
    // 2 - unify constraints and get a substitution
    val (subst2a,rest2a) = Unify.getUnification(type1.const)
    println(s" - [ unification: $subst2a ]")
    println(s" - [ missing:     ${Show(rest2a)} ]")
    // 3 - apply substitution to the type
    val rest3a = subst2a(rest2a)
    val type3a = Type(type1.args,subst2a(type1.i),subst2a(type1.j),rest3a,type1.isGeneral)
    println(s" - substituted:   $type3a")

    // 4 - try to get intervals - if success, update substitution
    val (interval,rest4a) = Solver.varIntIntervals(Simplify(type3a).const)
    println(s" - got interval (pre-extension): ${interval.mkString("/")} - ${Show(rest4a)}")
    val (subst4a,rest4b) = Solver.guessSol(Simplify(type3a).const)
    println(s" - guessing solution: $subst4a - ${Show(rest4b)}")
    // update subst and type if succeeded
    val (subst4c,rest4c,type4c) = if (rest4b==BVal(true) || rest4b==And(List()))
                                       (subst2a.compose(subst4a),rest4b,subst4a(type3a))
                                  else (subst2a,rest3a,type3a)
    println(s" - combined subst: $subst4c")
    println(s" - combined type: $type4c")

    // 5 - extend with lost constraints over argument-variables
    val rest5a = subst4c.getConstBoundedVars(type4c)
    val type5a = Type(type4c.args,type4c.i,type4c.j,rest4c & rest5a,type4c.isGeneral)
    println(s" - extended with: $rest5a")
    // 6 - evaluate and simplify type
    val type6a = Simplify(type5a)
    println(s" - simplified:    $type6a")

    // 7 - apply subst3 if solver is an abstract solution
    val type7a = subst4c(type6a)
    if (type7a.isGeneral) {
      val rest7a = subst4c.getConstBoundedVars(type7a)
      println(s" - extended with: $rest7a")
      val type7b = Eval(Type(type7a.args, type7a.i, type7a.j, type7a.const & rest7a, type7a.isGeneral))
      println(s" - post-solver:   $type7b")
    }
    else println(s" - solution yields a concrete instance only.")
    // 8 - apply the new substitution to the previous type and eval
    val type8a = Eval.instantiate(subst4c(type7a))
    println(s" - instantiation: $type8a")
    // 9 - show final type
    println(" - real type : "+Show(typeOf(c)))
  }
  catch {
    case e:TypeCheckException => println(s" ! type checking error: ${e.getMessage}")
    case NonFatal(e) => throw e
    //      case x : Throwable => throw x

  }
  /**
    * Checks if a connector type checks, using typeOf
    *
    * @param c connector to type check
    * @return
    */
  def typeCheck(c:Connector): Option[Type] = try {
    Some(typeOf(c))
  }
  catch {
    case _:TypeCheckException => None
    case NonFatal(e) => throw e
//    case e: Throwable => throw e
  }

  /**
    * Typechecks every subconnector, to pinpoint the source of a type error.
    * @param c connector to be type-checked
    * @return the type of the connector, or throws a descriptive error.
    */
  def checkVerbose(c:Connector): Type = Deconstructor.check(c,typeCheck)

  /**
    * Same as [[checkVerbose(c:Connector)]], but parses an input string first
    * @param s string with the connector to be parsed and type checked
    * @return the type, or throws a descriptive error
    */
  def checkVerbose(s:String): Type = checkVerbose(parse(s))


  /**
    * Typechecks every subconnector without using an SMT solver, to pinpoint the source of a type error.
    * @param c connector to be type-checked
    * @return the type of the connector, or throws a descriptive error.
    */
  def unsafeCheckVerbose(c:Connector): Type = Deconstructor.check(c,unsafeTypeCheck)


  /**
    * Same as [[unsafeCheckVerbose(c:Connector)]], but parses an input string first
    * @param s string with the connector to be parsed and type checked
    * @return the type, or throws a descriptive error
    */
  def unsafeCheckVerbose(s:String): Type = unsafeCheckVerbose(parse(s))

  /**
    * Unsafe version of [[typeCheck()]], using [[unsafeTypeOf()]] that
    * skips the constraint solving phase.
    *
    * @param c connector to type check
    * @return
    */
  def unsafeTypeCheck(c:Connector): Option[Type] = try {
    Some(unsafeTypeOf(c)._1)
  }
  catch {
    case _:TypeCheckException => None
    case NonFatal(e) => throw e
    //    case e: Throwable => throw e
  }

  /**
    * Checks if a given connector has parameters (i.e., it is a family)
    *
    * @param c
    * @return
    */
  def isFamily(c:Connector): Boolean = {
    val typ = TypeCheck.check(c)
    typ.args.vars.nonEmpty
  }

  /**
    * Finds an instance of the connector, and removes applications, lambdas, and restrictions
    *
    * @param c connector to be reduced
    * @return rediced connector
    */
  def reduce(c:Connector):CoreConnector = Eval.reduce(c)

  /**
    * Unsafe version of [[reduce()]], using [[Eval.unsafeReduce()]] that
    * skips the constraint solving phase.
    *
    * @param c connector to be reduced
    * @return rediced connector
    */
  def unsafeReduce(c:Connector):CoreConnector = Eval.unsafeReduce(c)

//  def stepwiseTyping(c:Connector,typing: Connector => (Type,BExpr),ctx: List[Var]): (Type,BExpr) = c match {
//    case Seq(c1, c2) =>
//      stepwiseTyping(c1,typing,ctx) // try to type c1
//      stepwiseTyping(c2,typing,ctx) // try to type c2
//      updateError(s"Failed to type sequence:\n  - ${Show(c1)}  ;\n  - ${Show(c2)}",c,typing)
//    case Par(c1, c2) =>
//      stepwiseTyping(c1,typing,ctx) // try to type c1
//      stepwiseTyping(c2,typing,ctx) // try to type c2
//      updateError(s"Failed to type parallel composition:\n  - ${Show(c1)}  ;\n  - ${Show(c2)}",c,typing)
//    case Trace(i, c) =>
//      stepwiseTyping(c,typing,ctx) // try to type c
//      updateError(s"Failed to type trace content:\n  - ${Show(c)}",c,typing)
//    case Exp(a, c) =>
//      stepwiseTyping(c,typing,ctx) // try to type c
//      updateError(s"Failed to type exponential:\n  - ${Show(c)}",c,typing)
//    case ExpX(x, a, c2) =>
//      addVar(x,c,c2,typing,ctx)
////      updateError(s"Failed to type exponential:\n  - ${Show(c)}",
////        lam(x.x:I,c),stepwiseTyping(_,typing)) // try to type c for any
//      updateError(s"Failed to type exponential:\n  - ${Show(c)}",c,typing)
//    case Choice(b, c1, c2) =>
//      stepwiseTyping(c1,typing) // try to type c1
//      stepwiseTyping(c2,typing) // try to type c2
//      updateError(s"Failed to type choice:\n  - ${Show(b)} ?\n  - ${Show(c1)}  +\n  - ${Show(c2)}",c,typing)
//    case Abs(x, et, c) =>
//      stepwiseTyping(c,typing) // try to type c
//      updateError(s"Failed to type lambda:\n  - ${Show(c)}",c,typing)
//    case App(c, a) =>
//      stepwiseTyping(c,typing) // try to type c
//      updateError(s"Failed to type lambda application:\n  - ${Show(c)}",c,typing)
//    case Restr(c, phi) =>
//    case Id(i) =>
//    case Symmetry(i, j) =>
//    case Prim(name, i, j, extra) =>
//    case SubConnector(name, c1) =>
//  }
//
//  private def updateError(str: String, c: Connector, typing: Connector => (Type, BExpr)) =
//    try  typing(c)
//    catch {
//      case e: TypeCheckException => throw new TypeCheckException(str + "\n" + e.getMessage)
//      case e: Throwable => throw e
//    }

}
