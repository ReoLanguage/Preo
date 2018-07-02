package preo.modelling

import org.junit.Test
import org.junit.Assert._
import preo.DSL.parse
import preo.ast._
import preo.frontend.Eval

//todo: reestructure tests
class TestModel {
    @Test
    def test1(): Unit = {
      //graph creation
      val ccore = Eval.reduce(parse("Tr(1)(fifo ; fifo ; fifo)"))
      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test2(): Unit = {

      val ccore = Eval.reduce(parse("reader"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)

    }

    @Test
    def test3(): Unit = {
      val ccore = Eval.reduce(parse("writer"))

      val model = Model(ccore)


      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test4(): Unit = {
      val ccore = Eval.reduce(parse("lossy"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test5(): Unit = {
      val ccore = Eval.reduce(parse("drain"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test6(): Unit = {
      val ccore = Eval.reduce(parse("fifo * fifo"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test7(): Unit = {
      val ccore = Eval.reduce(parse("dupl ; (fifo * fifofull)"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test8(): Unit = {
      val ccore = Eval.reduce(parse("(fifo * fifofull) ; merger"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test9(): Unit = {
      val ccore = Eval.reduce(parse("writer ; fifo ; reader"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test10(): Unit = {
      val ccore = Eval.reduce(parse("(writer ; fifo ; reader) * (writer ; fifo ; reader)"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }

    @Test
    def test11(): Unit = {
      val ccore = Eval.reduce(parse("writer^3 ; sequencer 3 ; reader^3 { zip =   \\n.Tr((2*n)*(n-1))  ((id^(n-x)*sym(1,1)^x*id^(n-x))^x<--n;   sym((2*n)*(n-1),2*n)),unzip =  \\n.Tr((2*n)*(n-1))  (((id^(x+1)*sym(1,1)^((n-x)-1)*id^(x+1))^x<--n);   sym((2*n)*(n-1),2*n)), sequencer =  \\n.((dupl^n;unzip(n:I)) *    Tr(n)(sym(n-1,1);((fifofull;dupl)*((fifo ; dupl)^(n-1)));         unzip(n:I))) ;    (id^n*(zip(n:I) ; drain^n))}"))

      val model = Model(ccore)

      testActions(model, ccore)
      testInits(model)
      testChannels(model, ccore)
    }


    def testChannels(model:Model, ccon: CoreConnector): Unit = {
      val nprims = getNumberOfPrims(ccon)
      assertEquals(nprims, model.getChannels.length)
    }

    def testActions(model: Model, ccon: CoreConnector): Unit ={
      assert(2 * model.getChannels.length <= model.getActions.size)
      assert(3.5* model.getChannels.length >= model.getActions.size)
    }


    def testInits(model: Model): Unit = {
      val vars = model.getActions
      val inits = model.getInits
      assertEquals(vars.size / 3, inits.length)
//      assert(model.getInits.length >= model.getChannels.length + filtered_nodes, "got: " + model.getInits.length.toString + " Expected: " + (model.getChannels.length + model.getNodes.length).toString)
    }

//    private def getReadersAndWriters(ccon: CoreConnector): List[CPrim] = ccon match{
//      case CSeq(c1, c2) => getReadersAndWriters(c1) ++ getReadersAndWriters(c2)
//
//      case CPar(c1, c2) => getReadersAndWriters(c1) ++ getReadersAndWriters(c2)
//
//      case CSymmetry(_, _) => Nil
//
//      case CTrace(_, c) => getReadersAndWriters(c)
//
//      case CId(_) => Nil
//
//      case CSubConnector(_, c, _) => getReadersAndWriters(c)
//      case x@CPrim(name , _, _, _) => if (name == "reader" || name == "writer") List(x) else Nil
//      case _ =>  Nil
//    }


    private def getNumberOfPrims(ccon: CoreConnector): Int = ccon match{
      case CSeq(c1, c2) => getNumberOfPrims(c1) + getNumberOfPrims(c2)

      case CPar(c1, c2) => getNumberOfPrims(c1) + getNumberOfPrims(c2)

      case CSymmetry(CoreInterface(i), CoreInterface(j)) => i+j

      case CTrace(_, c) => getNumberOfPrims(c)

      case CId(CoreInterface(i)) => i

      case CSubConnector(_, c, _) => getNumberOfPrims(c)
      case CPrim(name , _, _, _) =>  1
      case _ =>  0
    }


}
