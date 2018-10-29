package preo

import org.scalatest.FlatSpec
import DSL._
import preo.ast.{Abs, Connector, IntType, Trace}
import preo.frontend.Show

class TestShow extends FlatSpec {

  def testPrint(c:Connector,res:String) {
//    println(show(c))
    s"Connector ${Show(c)}" should s"be printed as $res"
    assertResult(res)(Show(c))
  }

  val c1 = "fifo"
  val c2 = "fifo" * id
  val c3 = id^3
  val c4 = Trace(2,"fifo" & id)

  testPrint("fifo",
            "fifo")
  testPrint("fifo" * id,
            "fifo ⊗ id")
  testPrint(id^3,
            "id^3")
  testPrint(Trace(2,"fifo" & id),
            "loop(2)(fifo ; id)")
  testPrint(Abs("x",IntType,"fifo"^"x"),
            "\\x:I.(fifo^x)")
  testPrint(Trace(2,("fifo"^3) & (id * (id^3))),
            "loop(2)((fifo^3) ; (id ⊗ (id^3)))")
  testPrint(lam(x,lam("b":B,drain^x)),
    "\\x:I b:B.(drain^x)")

  testPrint(!("x":I) -> (fifo^x)    , "\\x:I.(fifo^x)")
  testPrint(!x - ("y":I) -> (fifo^x), "\\x:I y:I.(fifo^x)")
  testPrint(!x-y-z-("b":B)->(fifo^x), "\\x:I y:I z:I b:B.(fifo^x)")
}