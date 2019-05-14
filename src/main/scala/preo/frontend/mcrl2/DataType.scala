package preo.frontend.mcrl2

/**
  * Created by guillecledou on 2019-05-14
  */


trait DataType {}

trait BoolDT extends DataType {
  def &(other:BoolDT):BoolDT = LAnd(this,other)
  def |(other:BoolDT):BoolDT = LOr(this,other)
}

case object LTrue extends BoolDT {
  override def toString: String = "true"
}
case class LAnd(e1:BoolDT,e2:BoolDT) extends BoolDT {
  override def toString: String = s"(${e1.toString}) && (${e2.toString})"
}
case class LOr(e1:BoolDT,e2:BoolDT) extends BoolDT {
  override def toString: String = s"(${e1.toString}) || (${e2.toString})"
}
case class LNot(e:BoolDT) extends BoolDT {
  override def toString: String = s"!(${e.toString})"
}
case class LIn[T<:Object](e:T, listName:String) extends BoolDT {
  override def toString: String = e.toString + " in " + listName
}
