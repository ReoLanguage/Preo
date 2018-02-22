package preo.modelling

import org.junit.Test
import org.junit.Assert._
import preo.DSL.parse


class TestFamily {
  @Test
  def test1(): Unit = {
    //graph creation
    val core = parse("\\n.fifo^n")

    val instances = Mcrl2FamilyModel.testApply(core)
    val familyModel = Mcrl2FamilyModel.restApply(instances)
    val models = instances.map(x => Mcrl2Model(x))
    testStarters(familyModel)
    testActions(familyModel, models)
    testDefs(familyModel, models)
  }

  @Test
  def test2(): Unit = {
    //graph creation
    val core = parse("\\n.(fifo ; lossy)^n")

    val instances = Mcrl2FamilyModel.testApply(core)
    val familyModel = Mcrl2FamilyModel.restApply(instances)
    val models = instances.map(x => Mcrl2Model(x))
    testStarters(familyModel)
    testActions(familyModel, models)
    testDefs(familyModel, models)
  }

  @Test
  def test3(): Unit = {
    //graph creation
    val core = parse("(\\n.fifo^n) ; (\\m.lossy^m)")

    val instances = Mcrl2FamilyModel.testApply(core)
    val familyModel = Mcrl2FamilyModel.restApply(instances)
    val models = instances.map(x => Mcrl2Model(x))
    testStarters(familyModel)
    testActions(familyModel, models)
    testDefs(familyModel, models)
  }

  @Test
  def test4(): Unit = {
    //graph creation
    val core = parse(
      """
        |(\n. nexrouter n)
        |{
        |  unzip =
        |    \n.Tr((2*n)*(n - 1))
        |    (((((id^(x+1))*(sym(1,1)^((n-x)-1)))*(id^(x+1)))^(x<--n));
        |     sym((2*n)*(n-1),2*n))
        |  ,
        |  dupls =
        |    \n.Tr(n-1)(id*(dupl^(n-1)) ; sym(1,(n-1)*2))
        |  ,
        |  mergers =
        |    \n.Tr(n-1)(sym((n-1)*2,1) ; (id*(merger^(n-1))))
        |  ,
        |  nexrouter =
        |    \n. (
        |      (dupls(n+1)) ;
        |      ((((lossy ; dupl)^n) ; (unzip(n)))*id) ;
        |      ((id^n)*(mergers(n))*id) ;
        |      ((id^n)*drain))
        |}
        |
      """.stripMargin)

    val instances = Mcrl2FamilyModel.testApply(core)
    val familyModel = Mcrl2FamilyModel.restApply(instances)
    val models = instances.map(x => Mcrl2Model(x))


    testStarters(familyModel)
    testActions(familyModel, models)
    testDefs(familyModel, models)
  }


  @Test
  def test5(): Unit = {
    //graph creation
    val core = parse("(\\n. (Tr(n-1)(sym(n-1, 1); fifo^n)))")

    val instances = Mcrl2FamilyModel.testApply(core)
    val familyModel = Mcrl2FamilyModel.restApply(instances)
    val models = instances.map(x => Mcrl2Model(x))
    testStarters(familyModel)
    testActions(familyModel, models)
    testDefs(familyModel, models)
  }


  private def testStarters(model: Mcrl2FamilyModel): Unit = {
    val starterNodes = model.getStarterNodes

    assertEquals(s"Expected: ${starterNodes} \n Got: ${model.getStarters}", model.getStarters.length, starterNodes.length)
    val nodes = model.getNodes.filter{case Mcrl2Node(_, a, _, _, _) => a == Action.nullAction}
    assertEquals(nodes.length, starterNodes.length)
    for (x <- starterNodes){
      for (y <- starterNodes){
        assert(x == y || x.node != y.node, s"X: ${x.toString}, Y: ${y.toString}")
      }
    }
  }

  private def testActions(f: Mcrl2FamilyModel, models: List[Mcrl2Model]): Unit = {
    val max_size = models.tail.foldRight(models.head.getActions.size)((m, maximum) =>{
      val actions = m.getActions.size
      if (actions > maximum) actions else maximum
    })
    assertEquals(max_size + 3* f.getStarterNodes.length, f.getActions.size)
    models.foreach(f => f.getActions.foreach(a => f.getActions.contains(a)))
  }

  private def testDefs(f: Mcrl2FamilyModel, models: List[Mcrl2Model]): Unit = {
    val max_size = models.tail.foldRight(models.head.getChannels.length)((m, maximum) =>{
      val channels = m.getChannels.length
      if (channels > maximum) channels else maximum
    })
    assertEquals(max_size, f.getChannels.size)

    val nnodes = models.foldRight(0)((m, count) => count + m.getNodes.length)
    assertEquals(nnodes, f.getNodes.length)

    val ninits = models.foldRight(0)((m, count) => count + m.getInits.length)
    assertEquals(ninits, f.getInits.length)

  }

}
