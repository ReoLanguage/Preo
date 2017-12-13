package preo.modelling

import org.junit.Assert._
import org.junit.Test

class TestProcess {
  @Test
  def testAction(): Unit ={
    //assert Prints
    assert(Action(1, 1).toString == "X1''")
    assert(Action(1, 2).toString == "X1'")
    assert(Action(1, 3).toString == "X1")
    assert(Action(1, 4).toString == "In1")
    assert(Action(1, 5).toString == "Out1")
    assert(Action(1, 6).toString == "Null")
    //assert Equals
    assertNotEquals(Action(1, 6), Action(1, 3))
    assertEquals(Action(1, 6), Action(1,6))
  }

  @Test
  def testMultiAction: Unit={
    //assert prints for 0, 1 and 2 Actions
    assertEquals(MultiAction().toString, "Null")
    assertEquals(MultiAction(Action(1, 1)).toString, Action(1, 1).toString)
    assertEquals(MultiAction(Action(1, 1), Action(2, 1)).toString, s"${Action(1, 1).toString} | ${Action(2, 1).toString}")
  }



}
