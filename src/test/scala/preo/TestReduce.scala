package preo

import org.junit.Assert._
import org.junit.Test
import preo.DSL._
import preo.ast.Connector
import preo.examples.Repository.{seqfifo, sequencer, zip}
import preo.frontend.{Eval, Show}

/**
  * Created by jose on 14/03/16.
  */
class TestReduce {

  val x: I = "x"
  val y: I = "y"
  val n: I = "n"


  @Test def testReductions() {
    testOK(fifo ^ 3, "fifo ⊗ (fifo ⊗ fifo)")
    testOK(seqfifo,"fifo")
    //    testOK(zip,"Id(2)")
    testOK(zip(1),"Id(2)")
    testOK(lam(n,lam(x,id^x)),"id")
    testOK(lam(x,id^x) & lam(y,id^y),"nil")
    testOK(lam(n,lam(x,id^x) & lam(y,id^y)),"nil")
    testOK(sequencer,"(dupl ⊗ Tr_1{fifofull ; dupl}) ; (id ⊗ drain)")
    // (dupl ⊗ Tr_1{fifo ; dupl}) ; (id ⊗ drain)
  }

  @Test def testInstantiate(): Unit = {
    testInstOK(lam(x,id^x) & lam(y,id^y),"((\\x:I.(id^x)) ; (\\y:I.(id^y)))(0)(0)")
    testInstOK(lam(n,lam(x,id^x) & lam(y,id^y)),"(\\n:I.((\\x:I.(id^x)) ; (\\y:I.(id^y))))(1)(0)(0)")

  }

  private def testOK(con:Connector,str:String) = {
    val c = Eval.reduce(con)
    assertEquals(Show(c), str)
  }

  private def testInstOK(con:Connector,str:String) = {
    val c = Eval.instantiate(con)
    assert(typeCheck(c))
    assertEquals(Show(c), str)
  }

}
