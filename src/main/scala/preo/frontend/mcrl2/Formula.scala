package preo.frontend.mcrl2

import preo.common.GenerationException

import scala.collection.mutable

sealed abstract class Formula

case object TrueF                         extends Formula
case object FalseF                        extends Formula
case class Diamond(r:RegularF, f:Formula) extends Formula
case class MBox(r:RegularF, f:Formula)    extends Formula
case class At(c:Container,f:Formula)      extends Formula
case class Up(f:Formula)                  extends Formula
case class AndF(f1:Formula, f2:Formula)   extends Formula

sealed abstract class RegularF {
  def +(that:RegularF): OrsRegular = (this,that) match {
    case (OrsRegular(f1),OrsRegular(f2)) => OrsRegular(f1++f2)
    case (OrsRegular(f1),f2) => OrsRegular(f1++List(f2))
    case (f1,OrsRegular(f2)) => OrsRegular(f1::f2)
    case _ => OrsRegular(List(this,that))
  }
}
case class Kleene(fp:ActionF)                extends RegularF
case class SeqRF(r1:RegularF,r2:RegularF)   extends RegularF
case class OrsRegular(paths:List[RegularF]) extends RegularF

sealed abstract class ActionF               extends RegularF {
  def &(that:ActionF): ActionF = (this,that) match {
    case (AndsFP(f1),AndsFP(f2)) => AndsFP(f1++f2)
    case (AndsFP(f1),f2) => AndsFP(f1++List(f2))
    case (f1,AndsFP(f2)) => AndsFP(f1::f2)
    case _ => AndsFP(List(this,that))
  }
  def ||(that:ActionF): ActionF = (this,that) match {
    case (OrsFP(f1),OrsFP(f2)) => OrsFP(f1++f2)
    case (OrsFP(f1),f2) => OrsFP(f1++List(f2))
    case (f1,OrsFP(f2)) => OrsFP(f1::f2)
    case _ => OrsFP(List(this,that))
  }
}
case object AllA  extends ActionF
case object NoneA extends ActionF
case class NegFP(l:ActionF)             extends ActionF
case class AndsFP(fpaths:List[ActionF]) extends ActionF
case class OrsFP(fpaths:List[ActionF])  extends ActionF
case class Container(name:String)       extends ActionF
case object Tau                         extends ActionF


object Formula {

  def formula2mCRL2(f:Formula, nameContainer: List[Container] => String): String =
    formula2mCRL2(f,Nil,nameContainer)

  private def formula2mCRL2(f:Formula, pref:List[Container],
                            naming: List[Container] => String): String = f match {
    case TrueF => "true"
    case FalseF => "false"
    case Diamond(reg,f) => s"<${regularF2mCRL2(reg,pref,naming)}> ${formula2mCRL2(f,pref,naming)}"
    case MBox(reg,f) => s"[${regularF2mCRL2(reg,pref,naming)}] ${formula2mCRL2(f,pref,naming)}"
    case At(c,f) => formula2mCRL2(f,c::pref,naming)
    case Up(f) => pref match {
      case Nil => throw new GenerationException(s"Failed to hide $f. Already in top level")
      case _::rest => formula2mCRL2(f,rest,naming)
    }
    case AndF(f1,f2) => formula2mCRL2(f1,pref,naming)+" && "+
                        formula2mCRL2(f2,pref,naming)
  }

  private def regularF2mCRL2(f: RegularF, pref: List[Container],
                             naming: List[Container] => String): String = f match {
    case Kleene(fp) => s"(${regularF2mCRL2(fp,pref,naming)})*"
    case SeqRF(r1, r2) => s"(${regularF2mCRL2(r1,pref,naming)}).(${regularF2mCRL2(r2,pref,naming)})"
    case OrsRegular(paths) => paths match {
      case Nil => "false"
      case _ => paths.map("("+regularF2mCRL2(_,pref,naming)+")").mkString("+")
    }
    case AllA => "true"
    case NoneA => "false"
    case NegFP(l) => "!("+regularF2mCRL2(l,pref,naming)+")"
    case AndsFP(paths) => paths match {
      case Nil => "true"
      case _ => paths.map("("+regularF2mCRL2(_,pref,naming)+")").mkString("&&")
    }
    case OrsFP(paths) => paths match {
      case Nil => "false"
      case _ => paths.map("("+regularF2mCRL2(_,pref,naming)+")").mkString("||")
    }
    case c:Container => naming(c::pref)
    case Tau => "tau"
  }


  def formula2mCRL2(f:Formula,names: mutable.Map[String, Set[Set[Action]]],
                    error: String => Unit ): String =
    formula2mCRL2(f,list => getMNames(list.map(_.name).reverse.mkString("_"),names,error))


  /**
    * replace a given container name by a "or" of channel names
    * in the mCRL2 spec.
    * @param str name of the container
    * @param names mapping from container names to channels where they are used
    * @return "Or" list of the associated channels
//    */
  private def getMNames(str: String, names: mutable.Map[String, Set[Set[Action]]],
                        error: String => Unit): String = {
    val actions = str.split(" *[|] *")
    var res: Option[Set[Set[Action]]] = None
    //println(s"actions: $actions, getting names: ${actions.mkString(",")}")
    for (a <- actions) {
      //println(s"action $a")
      (names.get(a),res) match {
        case (Some(mas),Some(acc)) =>
          res = Some(acc intersect mas)
          //println(s"updated - names($a) = $mas, acc=$acc, res=${res.mkString(".")}")
        case (Some(mas),None) =>
          res = Some(mas)
          //println(s"reset - names($a)=$mas. res=${res.mkString(".")}")
        case (None,_) =>
          if (a=="id") Some("sync")
          //println(s"## left res - ${res.mkString(".")}")
      }
    }
    res match {
      case None =>
        error(s"unknown container: ${actions.mkString("/")}") // in ${names.mkString("\n")}")
        str //s"##${str}/${actions.mkString("/")}##"
      case Some(set) =>
        if (set.isEmpty) "false"
        else set.map(_.mkString("|")).mkString("("," || ",")")
    }
  }

  def notToHide(f:Formula, par:List[Container]=Nil): List[List[Container]] = f match {
    case TrueF => List()
    case FalseF => List()
    case Diamond(reg,f) => par.reverse :: notToHide(f,par)
    case MBox(reg,f) => par.reverse :: notToHide(f,par)
    case At(c,f) => notToHide(f,c::par)
    case Up(f) => notToHide(f,if (par.isEmpty) Nil else par.tail)
    case AndF(f1,f2) => notToHide(f1,par) ::: notToHide(f2,par)
  }

//  def notToHide(f: RegularF) : List[List[Container]] = f match {
//    case Kleene(fp) => notToHide(fp)
//    case SeqRF(r1, r2) => notToHide(r1) ::: notToHide(r2)
//    case OrsRegular(paths) => paths.flatMap(notToHide)
//    case AllA => List(Nil) // ?
//    case NoneA => List(Nil) // ?
//    case NegFP(l) => notToHide(l)
//    case AndsFP(paths) =>  paths.flatMap(notToHide)
//    case OrsFP(paths) =>  paths.flatMap(notToHide)
//    case c:Container => List(Nil) // ?
//    case Tau => List(Nil) //?
//  }



}