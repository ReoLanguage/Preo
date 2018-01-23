package preo.modelling

import org.junit.Assert._
import org.junit.Test

class TestProcess {
  @Test
  def testAction(): Unit ={
    //assert Prints
    assertEquals(Action("X", 1, 1, 1).toString , "X1in1''")
    assertEquals(Action("X", 1, 1, 2).toString , "X1in2''")
    assertEquals(Action("X", 1, 2, 3).toString , "X1out1'")
    assertEquals(Action("X", 1, 2, 4).toString , "X1out2'")
    assertEquals(Action("X", 1, 4, 1).toString , "Null")
    assertEquals(Action("X", 1, 3, 5).toString , "X1")
    //assert Equals
    assertNotEquals(Action("X", 1, 1, 4), Action("X", 1, 3, 4))
    assertEquals(Action("X", 1, 3, 5), Action("X", 1,3, 5))
  }

  @Test
  def testMultiAction: Unit={
    //assert prints for 0, 1 and 2 Actions
    assertEquals(MultiAction().toString, "Null")
    assertEquals(MultiAction(Action(1, 1)).toString, Action(1, 1).toString)
    assertEquals(MultiAction(Action(1, 1), Action(2, 1)).toString, s"${Action(1, 1).toString} | ${Action(2, 1).toString}")
  }



}
