package preo

import org.scalatest.FlatSpec
import preo.DSL._
import preo.ast.Connector
import preo.examples.Repository.{seqfifo, sequencer, zip}
import preo.frontend.{Eval, Show}

/**
  * Created by jose on 14/03/16.
  */
class TestReduce extends FlatSpec {

  val x: I = "x"
  val y: I = "y"
  val n: I = "n"



  testOK(fifo ^ 3, "fifo ⊗ (fifo ⊗ fifo)")
  testOK(seqfifo,"fifo")
  //    testOK(zip,"Id(2)")
  testOK(zip(1),"Id(2)")
  testOK(lam(n,lam(x,id^x)),"id")
  testOK(lam(x,id^x) & lam(y,id^y),"nil")
  testOK(lam(n,lam(x,id^x) & lam(y,id^y)),"nil")
  testOK(sequencer,"(dupl ⊗ Tr_1{fifofull ; dupl}) ; (id ⊗ drain)")
  // (dupl ⊗ Tr_1{fifo ; dupl}) ; (id ⊗ drain)


  testInstOK(lam(x,id^x) & lam(y,id^y),"((\\x:I.(id^x)) ; (\\y:I.(id^y)))(0)(0)")
  testInstOK(lam(n,lam(x,id^x) & lam(y,id^y)),"(\\n:I.((\\x:I.(id^x)) ; (\\y:I.(id^y))))(1)(0)(0)")


  private def testOK(con:Connector,str:String) = {
    Thread.sleep(100)
    val c = Eval.reduce(con)
    s"Connector ${Show(con)}" should s"be reduced to $str" in
      assertResult(str)(Show(c))
  }

  private def testInstOK(con:Connector,str:String) = {
    Thread.sleep(100)
    val c = Eval.instantiate(con)
    s"Connector ${Show(con)}" should s"instantiate to $str" in {
      assert(typeCheck(c))
      assertResult(str)(Show(c))
    }
  }

}
