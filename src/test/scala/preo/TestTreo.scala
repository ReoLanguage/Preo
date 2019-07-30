package preo

import org.scalatest.FlatSpec
import preo.DSL._
import preo.ast._
import preo.frontend.{Eval, Show, Simplify}

/**
  * Created by jose on 13/05/16.
  */
class TestTreo extends FlatSpec {

  private def testOK(str:String,res1:String,res2:String): Unit = {
    s"Connector $str" should s"produce the preo connector: $res1" in {
//      val input: String   = s"${str.takeWhile(_!='(')} { $str }"
      val conn: Connector = parse(str)
      assertResult(res1)(Show(Simplify(conn)))
      val conn2 = Simplify(DSL.unfoldTreo(DSL.reduce(conn)).toConnector)
      assertResult(res2)(Show(conn2))
    }
  }

  testOK("f {f(x?,y!) = fifo(x,y)}"
    ,"f{[x?,y!]=>fifo(x?,y!)}"
    ,"f{fifo}")
    //"f{fifo}")
  testOK("""alternator { alternator(a?,b?,c!) =
           |   drain(a, b)
           |   sync(b, x)
           |   fifo(x, c)
           |   sync(a, c) }""".stripMargin
        ,"alternator{[a?,b?,c!]=>drain(a?,b?)  sync(b?,x!)  fifo(x?,c!)  sync(a?,c!)}"
        ,"alternator{loop(1)(((((dupl ⊗ Id(2)) ; (id ⊗ (sym(1,1) ⊗ id))) ; (id ⊗ (dupl ⊗ sym(1,1)))) ; (drain ⊗ (sync ⊗ (fifo ⊗ sync)))) ; (((sym(1,1) ⊗ id) ; (id ⊗ sym(1,1))) ; (merger ⊗ id)))}")
  testOK("""f {
           | c1 = lossy*lossy ,
           | f(a?,c!) =
           |   c1(a,x,c,y)
           |   sync(y,x)
           |}""".stripMargin
        ,"f{[a?,c!]=>c1{lossy ⊗ lossy}(a?,x?,c!,y!)  sync(y?,x!)}"
        ,"f{loop(2)((c1{lossy ⊗ lossy} ⊗ sync) ; (id ⊗ sym(1,1)))}")
  testOK("""main {
           |  dd(a?,b?) = drain(a,b)
           |  ,
           |  main(a?,b?) =
           |    dd(a,c) lossy(b,c)
           |}
           |""".stripMargin
      ,"main{[a?,b?]=>dd{[a?,b?]=>drain(a?,b?)}(a?,c?)  lossy(b?,c!)}"
      ,"main{loop(1)((id ⊗ sym(1,1)) ; (dd{drain} ⊗ lossy))}")

}
