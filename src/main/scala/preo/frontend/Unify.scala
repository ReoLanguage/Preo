package preo.frontend

import preo.ast._
import preo.common.{TypeCheckException, Utils}

/**
 * Created by jose on 20/05/15.
 */
object Unify {


  /**
   * Simplifies a given constraint and searches for a unification.
   * The result is a general unification (a substitution) and a constraint that can be later fed to a solver.
   * It throws an exception if the constraint is clearly unsatisfiable.
   *
   * @param const constraint where a unification is searched for.
   * @return a general unification and postponed constraints.
   */
  def getUnification(const:BExpr): (Substitution,BExpr) = {
    val simpler = Simplify(const)
    getUnification(simpler, BVal(true)) match {
      case Some(pair) => pair
      case None => throw new TypeCheckException(s"Unification failed: ${Show(simpler)} (based on ${Show(const)}).")
    }
  }

  private def getUnification(const:BExpr,rest:BExpr): Option[(Substitution,BExpr)] = const match {
    case And(BVal(true)::exps) => getUnification(And(exps),rest)
    case And(BVal(false)::_)   => None
      //throw new TypeCheckException("Search for unification failed - found 'false'.")
    case And((Not(x@Var(_)))::exps) =>
      val s = Substitution(x,BVal(b=false))
      for ((news,newrest) <- getUnification(Simplify(s(And(exps))),rest))
        yield (news + (x,BVal(b=false)), newrest)
    case And((x@Var(_))::exps) =>
      val s = Substitution(x,BVal(b=true))
      for ((news,newrest) <- getUnification(Simplify(s(And(exps))),rest))
        yield (news + (x,BVal(b=true)), newrest)
    case And(EQ(e1, e2)::exps) if e1 == e2 => getUnification(And(exps),rest)
    case And(EQ(x@Var(_), y@Var(_))::exps) if x!=y =>
      if (Utils.isGenVar(x.x))
        substVar(x,y,exps,rest)
      else if (Utils.isGenVar(y.x) || Utils.isAlphaEquivVar(y.x))
        substVar(y,x,exps,rest)
      else
        substVar(x,y,exps,rest)
    case And(EQ(x@Var(_), e2)::exps) if Utils.isFree(x,e2) =>
      substVar(x,e2,exps,rest)
    case And(EQ(e1,x@Var(_))::exps) if Utils.isFree(x,e1) =>
      substVar(x,e1,exps,rest)
    // "equality"/"or" of general int expresions - postpone
    case And((eq@EQ(_,_))::exps) => getUnification(And(exps),rest & eq)
    case And((or@Or(_,_))::exps) => getUnification(And(exps),rest & or)
    case And((gt@GT(_,_))::exps) => getUnification(And(exps),rest & gt)
    case And((lt@LT(_,_))::exps) => getUnification(And(exps),rest & lt)
    case And((ge@GE(_,_))::exps) => getUnification(And(exps),rest & ge)
    case And((le@LE(_,_))::exps) => getUnification(And(exps),rest & le)
    case And((nt@Not(_))::exps)  => getUnification(And(exps),rest & nt)
    case And((an@AndN(_,_,_,_))::exps)  => getUnification(And(exps),rest & an)
    //
    case And(And(e1)::exps) => getUnification(And(e1:::exps),rest)
    case And(Nil) => Some((Substitution(),rest))
    //
    case _:BVal | _:Var | _:EQ | _:GT | _:LT | _:GE | _:LE | _:Or | _:Not | _:AndN =>
      getUnification(And(const :: Nil), rest)
  }

  private def substVar(x:Var,e:IExpr,exps:List[BExpr],rest:BExpr): Option[(Substitution,BExpr)] = {
    e match {
      case IVal(n) if n<0 => throw new TypeCheckException(s"Variable $x cannot take the negative value $n.")
      case _ => {}
    }
    val s = Substitution(x , e)
    for ((news,newrest) <- getUnification( Simplify(s(And(exps))),rest))
//    println(s"### checking if ${Show(x)} == ${Show(e)} has vars in $bounded.")
//    if (bounded contains x) {
//      newrest = newrest & EQ(x, e)
//      println("##### yes!")
//    }
//    for (v <- Utils.freeVars(e))
//      if (bounded contains v) {
//        newrest = newrest & EQ(x,e) //avoid repetition (not a big problem)
//        println("##### yes!")
//      }
      yield (news + (x,e), newrest)

  }
}
