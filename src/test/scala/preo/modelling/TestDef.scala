package preo.modelling

import org.junit.Test
import org.junit.Assert._

class TestDef {

  @Test
  def testNode(): Unit ={
    val node1 = Mcrl2Node(1, null, null)
    assertEquals(node1.toString,"Node1 = (Null | Null) . Node1")
    assertEquals(node1.getName.toString, "Node1")
    node1.setLeft("X", 1, 5)
    node1.setRight("X", 3, 5)
    assertEquals(node1.toString,"Node1 = (X1' | X3') . Node1")
    assertEquals(node1.getVars.length, 2)
    val node2 = Mcrl2Node(2, null, null)
    assertEquals(node2.getVars.length, 1)
  }

  @Test
  def testChannel(): Unit ={
    val channel = Mcrl2Channel("Fifo", 2, Nil, Nil, Seq(Action(1, 1), Action(2, 1)))
    assertEquals(channel.toString, s"Fifo2 = (${Seq(Action(1, 1), Action(2, 1)).toString}) . Fifo2")
    assertEquals(channel.getName.toString, "Fifo2")
    assert(channel.getVars.isEmpty)
    channel.addLeft(1)
    channel.addRight(2)
    assertEquals(channel.getVars.length, 2)
  }

  @Test
  def testInit(): Unit ={
    val init1 = Mcrl2Init(1, "X",  3, 1,  ProcessName("Fifo1"), ProcessName("Node1"))
    val init2 = Mcrl2Init(2, "X", 4, 1, ProcessName("node2"), init1.getName)
    assertEquals(init1.getVars.length, 1)
    assertEquals(init2.getVars.length, 1)
    assertEquals(init1.getName.toString, "Init1")
    assertEquals(init2.toString, s"Init2 = ${init2.operator.toString}")
    assertTrue(init2.toString.contains("Init1"))
  }
}
