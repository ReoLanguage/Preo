package preo.frontend.mcrl2

import preo.common.GenerationException

import scala.collection.mutable

sealed abstract class Formula

case object TrueF                         extends Formula
case object FalseF                        extends Formula
case class Diamond(r:RegularF, f:Formula) extends Formula
case class MBox(r:RegularF, f:Formula) extends Formula
case class At(c:Container,f:Formula)      extends Formula
case class Up(f:Formula)                  extends Formula

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

}