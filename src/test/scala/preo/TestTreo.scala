package preo

import org.scalatest.FlatSpec
import preo.DSL._
import preo.ast._
import preo.frontend.{Eval, Show, Simplify}

/**
  * Created by jose on 13/05/16.
  */
class TestTreo extends FlatSpec {

  private def testOK(str:String,res:String): Unit = {
    s"Connector $str" should s"produce the preo connector: $res" in {
//      val input: String   = s"${str.takeWhile(_!='(')} { $str }"
      val conn: Connector = parse(str)
      assertResult(res)(Show(Simplify(conn)))
    }
  }

  testOK("f {f(x?,y!) = fifo(x,y)}", "f{fifo}")
  testOK("""alternator { alternator(a?,b?,c!) =
           |   drain(a, b)
           |   sync(b, x)
           |   fifo(x, c)
           |   sync(a, c) }""".stripMargin
        ,"alternator{loop(1)(((((dupl ⊗ Id(2)) ; (id ⊗ (sym(1,1) ⊗ id))) ; (id ⊗ (dupl ⊗ sym(1,1)))) ; (drain ⊗ (sync ⊗ (fifo ⊗ sync)))) ; (((sym(1,1) ⊗ id) ; (id ⊗ sym(1,1))) ; (merger ⊗ id)))}")
  testOK("""f {
           | c1 = lossy*lossy ,
           | f(a?,c!) =
           |   c1(a,x,c,y)
           |   sync(y,x)
           |}""".stripMargin
        ,"f{loop(2)((c1{lossy ⊗ lossy} ⊗ sync) ; (id ⊗ sym(1,1)))}")
}
