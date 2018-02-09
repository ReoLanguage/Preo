package preo.modelling

import org.junit.Assert._
import org.junit.Test

class TestProcess {
  @Test
  def testAction(): Unit ={
    //assert Prints
    assertEquals(Action("X", 1, TwoLine, In1).toString , "X1in1''")
    assertEquals(Action("X", 1, TwoLine, In2).toString , "X1in2''")
    assertEquals(Action("X", 1, OneLine, Out1).toString , "X1out1'")
    assertEquals(Action("X", 1, OneLine, Out2).toString , "X1out2'")
    assertEquals(Action.nullAction.toString , "Null")
    assertEquals(Action("X", 1, NoLine, Nothing).toString , "X1")
    //assert Equals
    assertNotEquals(Action("X", 1, TwoLine, Out2), Action("X", 1, NoLine, Out2))
    assertEquals(Action("Null", -1, NoLine, Nothing), Action.nullAction)
  }

  @Test
  def testMultiAction: Unit={
    //assert prints for 0, 1 and 2 Actions
    assertEquals(MultiAction().toString, "Null")
    assertEquals(MultiAction(Action("X", 1, NoLine, Nothing)).toString, Action("X",1, NoLine, Nothing).toString)
    assertEquals(MultiAction(Action("X", 1, NoLine, Nothing), Action("X", 2, TwoLine, In1)).toString, s"${Action("X", 1, NoLine, Nothing).toString} | ${Action("X", 2, TwoLine, In1).toString}")
  }



}
