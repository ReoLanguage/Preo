package preo.ast

sealed abstract class CoreConnector {
  // helpers for the DSL
  def &(that:CoreConnector) = CSeq(this,that)
  def *(that:CoreConnector) = CPar(this,that)

  def toConnector: Connector = this match {
    case CSeq(c1, c2) => Seq(c1.toConnector,c2.toConnector)
    case CPar(c1, c2) => Par(c1.toConnector,c2.toConnector)
    case CId(i) => Id(i.toInterface)
    case CSymmetry(i, j) => Symmetry(i.toInterface,j.toInterface)
    case CTrace(i, c) => Trace(i.toInterface,c.toConnector)
    case CPrim(name, i, j, extra) => Prim(name,i.toInterface,j.toInterface,extra)
    case CSubConnector(name, sub, a) => SubConnector(name, sub.toConnector, a)
  }

  override def toString: String = toConnector.toString
}

case class CSeq(c1:CoreConnector, c2:CoreConnector) extends CoreConnector
case class CPar(c1:CoreConnector, c2:CoreConnector) extends CoreConnector
case class CId(i:CoreInterface) extends CoreConnector
case class CSymmetry(i:CoreInterface,j:CoreInterface) extends CoreConnector
case class CTrace(i:CoreInterface,c:CoreConnector) extends CoreConnector
case class CPrim(name:String,i:CoreInterface,j:CoreInterface,extra:Option[Any]=None) extends CoreConnector

case class CSubConnector(name:String, c:CoreConnector, anotation: List[Annotation]) extends CoreConnector

case class CoreInterface(ports:Int) {
  def *(other:CoreInterface) = CoreInterface(ports + other.ports)

  def toInterface:Interface = Port(IVal(ports))
}

