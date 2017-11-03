package preo

import org.scalatest.FlatSpec
import preo.DSL._
import preo.ast.{Connector, IVal}
import preo.frontend.Show
import preo.lang.Parser


/**
  * Created by jose on 08/03/2017.
  */
class TestParser extends FlatSpec {

  testOK("fifo",fifo)
  testOK("fifo & drain",fifo & drain) // connectors do not need to typecheck (parser=syntax)
  testOK("""\x:I . fifo^2""",lam(x,fifo^2))
  testOK("""\x y z:B . fifo^x""",lam(x, lam(y, lam("z":B,fifo^x)))) // variables, by default, are integers
  testOK("(id * abc) & drain ",(id * "abc") & drain)
  testOK("""\x b:B. b? (fifo^x) + (drain^(x-1))""",lam(x,lam(b,b ? (fifo^x) + (drain^(x-1)))))
  testOK("""(\x. fifo^x) (1+2)""",lam(x,fifo^x)(IVal(1)+2))


  private def testOK(in:String,out:Connector) =
    s"""The string "$in"""" should s"""produce the connector "${Show(out)}"""" in {
      Parser.parse(in) match {
        case Parser.Success(result, _) =>
          assertResult(out)(result)
        //          assert(s"Wrong parsed value. Got\n  $result\nexpected\n  $out",result,out)
        case err: Parser.NoSuccess =>
          fail("Parse error: "+err)
      }
    }

}

//package preo
//
//import org.junit.Assert._
//import org.junit.Test
//import preo.DSL._
//import preo.ast.{Connector, IVal}
//import preo.lang.Parser
//
//
//
///**
//  * Created by jose on 08/03/2017.
//  */
//class TestParser {
//
//  @Test def parseExamples(): Unit = {
//    testOK("fifo",fifo)
//    testOK("fifo & drain",fifo & drain) // connectors do not need to typecheck (parser=syntax)
//    testOK("""\x:I . fifo^2""",lam(x,fifo^2))
//    testOK("""\x y z:B . fifo^x""",lam(x, lam(y, lam("z":B,fifo^x)))) // variables, by default, are integers
//    testOK("(id * abc) & drain ",(id * "abc") & drain)
//    testOK("""\x b:B. b? (fifo^x) + (drain^(x-1))""",lam(x,lam(b,b ? (fifo^x) + (drain^(x-1)))))
//    testOK("""(\x. fifo^x) (1+2)""",lam(x,fifo^x)(IVal(1)+2))
//  }
//
//  private def testOK(in:String,out:Connector) = {
//    Parser.parse(in) match {
//      case Parser.Success(result, _) =>
//        assertEquals(s"Wrong parsed value. Got\n  $result\nexpected\n  $out",result,out)
//      case err: Parser.NoSuccess =>
//        fail("Parse error: "+err)
//    }
//  }
//
//}
