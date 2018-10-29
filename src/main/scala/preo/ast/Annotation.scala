package preo.ast

case class Annotation(name: String, value: Option[Expr])

object Annotation {
  def hidden(a:Annotation): Boolean = a.name.equalsIgnoreCase("hide")
  def hidden(as:Iterable[Annotation]): Boolean = as exists hidden
}
