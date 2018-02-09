package preo.modelling

import org.junit.Test
import org.junit.Assert._
import preo.DSL.parse
import preo.Main.readFromFile
import preo.ast._
import preo.backend.ReoGraph
import preo.backend.ReoGraph.Edge
import preo.frontend.Eval
import preo.modelling._


class TestModel {
    @Test
    def test1(): Unit = {
      //graph creation
      val ccore = Eval.reduce(parse("Tr(1)(fifo ; fifo ; fifo)"))
      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test2(): Unit = {

      val ccore = Eval.reduce(parse("reader"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)

    }

    @Test
    def test3(): Unit = {
      val ccore = Eval.reduce(parse("writer"))

      val model = Mcrl2Model(ccore)


      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test4(): Unit = {
      val ccore = Eval.reduce(parse("lossy"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test5(): Unit = {
      val ccore = Eval.reduce(parse("drain"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test6(): Unit = {
      val ccore = Eval.reduce(parse("fifo * fifo"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test7(): Unit = {
      val ccore = Eval.reduce(parse("dupl ; (fifo * fifofull)"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test8(): Unit = {
      val ccore = Eval.reduce(parse("(fifo * fifofull) ; merger"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test9(): Unit = {
      val ccore = Eval.reduce(parse("writer ; fifo ; reader"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test10(): Unit = {
      val ccore = Eval.reduce(parse("(writer ; fifo ; reader) * (writer ; fifo ; reader)"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test11(): Unit = {
      val ccore = Eval.reduce(parse("writer^3 ; sequencer 3 ; reader^3 { zip =   \\n.Tr((2*n)*(n-1))  ((id^(n-x)*sym(1,1)^x*id^(n-x))^x<--n;   sym((2*n)*(n-1),2*n)),unzip =  \\n.Tr((2*n)*(n-1))  (((id^(x+1)*sym(1,1)^((n-x)-1)*id^(x+1))^x<--n);   sym((2*n)*(n-1),2*n)), sequencer =  \\n.((dupl^n;unzip(n:I)) *    Tr(n)(sym(n-1,1);((fifofull;dupl)*((fifo ; dupl)^(n-1)));         unzip(n:I))) ;    (id^n*(zip(n:I) ; drain^n))}"))

      val model = Mcrl2Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }


    def testChannels(model:Mcrl2Model, ccon: CoreConnector): Unit = {
      val nprims = getNumberOfPrims(ccon)
      assertEquals(nprims, model.getChannels.length)
    }

    def testActions(model: Mcrl2Model, ccon: CoreConnector): Unit ={
      val beginers_ends = getReadersAndWriters(ccon)
      assert(6* model.getChannels.length + beginers_ends.length <= (model.getActions - Action.nullAction).size)
      assert(9* model.getChannels.length + beginers_ends.length >= (model.getActions - Action.nullAction).size)
      assert(model.getActions.count{case Action(name, n, _, _) => n != -1 && name != "reader" && name != "writer"} % 3 == 0)
    }


    def testInits(model:Mcrl2Model): Unit = {
      val vars = model.getActions.filter{case Action(name, n, _, _) => n != -1 && name != "reader" && name != "writer"}
      val inits = model.getInits
      assertEquals(vars.size / 3, inits.length)
      val filtered_nodes = model.getNodes.count(n => n.getBefore.name != "Null" && n.getBefore.name != "reader" && n.getBefore.name != "writer")
      assert(model.getInits.length >= model.getChannels.length + filtered_nodes, "got: " + model.getInits.length.toString + " Expected: " + (model.getChannels.length + model.getNodes.length).toString)
    }

    private def getReadersAndWriters(ccon: CoreConnector): List[CPrim] = ccon match{
      case CSeq(c1, c2) => getReadersAndWriters(c1) ++ getReadersAndWriters(c2)

      case CPar(c1, c2) => getReadersAndWriters(c1) ++ getReadersAndWriters(c2)

      case CSymmetry(CoreInterface(i), CoreInterface(j)) => Nil

      case CTrace(CoreInterface(i), c) => getReadersAndWriters(c)

      case CId(CoreInterface(i)) => Nil

      case CSubConnector(_, c) => getReadersAndWriters(c)
      case x@CPrim(name , _, _, _) => if (name == "reader" || name == "writer") List(x) else Nil
      case _ =>  Nil
    }


    private def getNumberOfPrims(ccon: CoreConnector): Int = ccon match{
      case CSeq(c1, c2) => getNumberOfPrims(c1) + getNumberOfPrims(c2)

      case CPar(c1, c2) => getNumberOfPrims(c1) + getNumberOfPrims(c2)

      case CSymmetry(CoreInterface(i), CoreInterface(j)) => i+j

      case CTrace(CoreInterface(i), c) => getNumberOfPrims(c)

      case CId(CoreInterface(i)) => i

      case CSubConnector(_, c) => getNumberOfPrims(c)
      case x@CPrim(name , _, _, _) => if (name != "reader" && name != "writer") 1 else 0
      case _ =>  0
    }


}
