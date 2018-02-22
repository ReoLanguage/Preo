package preo.modelling

import org.junit.Test
import org.junit.Assert._

class TestDef {

  @Test
  def testNode(): Unit ={
    val action1 = Action("X", 1, TwoLine, Nothing)
    val node1 = Mcrl2Node(1, action1, Action.nullAction)
    assertEquals(node1.toString,"Node1 = (X1'' | Null) . Node1")
    assertEquals(node1.getName.toString, "Node1")
    val action2 = Action("X", 2, OneLine, Nothing)
    val node2 = Mcrl2Node(2, Action.nullAction, action2)
    assertEquals(node2.toString,"Node2 = (Null | X2') . Node2")
    assertEquals(node1.getVars.length, 2)
    assertEquals(node2.getVars.length, 2)
    val node3 = node1 ++ node2
    assertEquals(node3.toString,"Node1 = (X1'' | X2') . Node1")
    assertEquals(node1.getVars.length, 2)
  }

  @Test
  def testChannel(): Unit ={
    val action1 = Action("X", 1, OneLine, In(1))
    val action2 = Action("X", 2, TwoLine, In(2))
    val channel = Mcrl2Channel("Fifo", 2, List(action1), List(action2), Seq(action1, action2))
    assertEquals(channel.toString, s"Fifo2 = (${Seq(action1, action2).toString}) . Fifo2")
    assertEquals(channel.getName.toString, "Fifo2")
    assertEquals(channel.getVars.length, 2)

  }

  @Test
  def testInit(): Unit ={
    val init1 = Mcrl2Init(1, "X",  3, In(1),  ProcessName("Fifo1"), ProcessName("Node1"))
    val init2 = Mcrl2Init(2, "X", 4, In(2), ProcessName("Node2"), init1.getName)
    assertEquals(init1.getVars.length, 1)
    assertEquals(init2.getVars.length, 1)
    assertEquals(init1.getName.toString, "Init1")
    assertEquals(init2.toString, s"Init2 = ${init2.operator.toString}")
    assertTrue(init2.toString.contains("Init1"))
  }

  @Test
  def testMcrl2StarterNode(): Unit = {
    val action1 = Action("X", 1, TwoLine, Nothing)
    val node1 = Mcrl2Node(1, action1, Action.nullAction)
    val starterNode = Mcrl2StarterNode(1, node1)
    assertEquals(1, starterNode.getVars.length)
    assertEquals(s"StarterNode1 = ${starterNode.getVars.head.toString} . ${node1.getName}", starterNode.toString)
  }

  @Test
  def testMcrl2Manager(): Unit = {
    val action1 = Action("X", 1, TwoLine, In(1))
    val action2 = Action("X", 2, OneLine, In(2))
    val action3 = Action("X", 3, NoLine, Out(1))
    val action4 = Action("X", 4, TwoLine, Out(2))
    val action5 = Action("X", 5, OneLine, Nothing)
    val action6 = Action("X", 6, NoLine, In(1))
    val m1 = MultiAction(action1, action2)
    val m2 = MultiAction(action3, action4, action5)
    val m3 = MultiAction(action6)
    val groups = List(m1, m2, m3)
    val manager = Mcrl2Manager(groups)
    val operation = Choice(m2, Choice(m3, m1))
    assertEquals(s"Manager = ${operation.toString}", manager.toString)
    assertEquals(6, manager.getVars.length)
  }

  @Test
  def testMcrl2Starter(): Unit = {
    val starter1 = Mcrl2Starter(1,  ProcessName("Fifo1"), ProcessName("Node1"))
    assertEquals(starter1.getVars.length, 1)
    val a1 = Action("a", 1, TwoLine, Nothing)
    val a2 = Action("a", 1, OneLine, Nothing)
    val a3 = Action("a", 1, NoLine, Nothing)
    val operator1 = Par(ProcessName("Fifo1"), ProcessName("Node1"))
    val operator2 = Comm((a1, a2, a3), operator1)
    val operator3 = Block(List(a2, a1), operator2)
    assertEquals(starter1.toString, s"Starter1 = ${Hide(List(a3), operator3).toString}")
  }
}
