package preo.lang

import preo.DSL._
import preo.ast._
import preo.common.TypeCheckException
import preo.examples.Repository
import preo.frontend.{Show, Substitution}

import scala.util.matching.Regex
import scala.util.parsing.combinator._

/**
  * Parser for Connectors, using parsing combinators.
  * For examples,check the unit tests - [[preo.TestParser]]
  * Created by jose on 07/03/2017.
  */
object Parser extends RegexParsers {

  /**
    * Main function that parses a string.
    * @param c string representing a connector
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parse(c:String): ParseResult[Connector] = parseAll(prog,c)
  def pa(c:String): ParseResult[BExpr] = parseAll(bexpr,c)


  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f\n]+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r

  /** Parses basic primitives */
  def inferPrim(s:String): Connector = s match {
    case "fifo"     => fifo
    case "fifofull" => fifofull
    case "drain"    => drain
    case "id"       => id
    case "dupl"     => dupl
    case "lossy"    => lossy
    case "merger"   => merger
    case "swap"     => swap
    case "writer"   => Prim("writer",Port(IVal(0)),Port(IVal(1)))
    case "reader"   => Prim("reader",Port(IVal(1)),Port(IVal(0)))
    case "node"     => SubConnector(s,Repository.node)
    case "dupls"    => SubConnector(s,Repository.dupls)
    case "mergers"  => SubConnector(s,Repository.mergers)
    case "zip"      => SubConnector(s,Repository.zip)
    case "unzip"    => SubConnector(s,Repository.unzip)
    case "exrouter" => SubConnector(s,Repository.exrouter)
    case "exrouters"=> SubConnector(s,Repository.nexrouter)
    case _          => str2conn(s)
  }



  ///////////////
  /// Program ///
  ///////////////

  def prog: Parser[Connector] =
    connP~opt("{"~whereP~"}")  ^^ {
      case co ~ Some(_~p~_) => p(co)
      case co ~ None => co
    }

  def whereP: Parser[Connector=>Connector] =
    identifier~"="~connP~opt(","~whereP) ^^ {
      case s~_~co2~Some(_~w) => (co:Connector) => Substitution.replacePrim(s,w(co),SubConnector(s, co2))
      case s~_~co2~None      => (co:Connector) => Substitution.replacePrim(s,  co ,SubConnector(s, co2))
    }

  ///////////////
  // Connector //
  ///////////////

  def connP: Parser[Connector] = lamP

  def lamP: Parser[Connector] =
    "\\"~identifier~lamCont ^^ { case _~ s ~ cont => cont(s,IntType)}  |
    seq

  def lamCont: Parser[(String,ExprType)=>Connector] =
    identifier ~ lamCont ^^ { case v ~ f  => lam(_:String,_:ExprType,f(v,IntType)) }          |
    ":" ~ "I"  ~ lamCont ^^ { case _~ _ ~ cont => (v:String,et:ExprType) => cont(v,et) }      |  // IntType is the default
    ":" ~ "B"  ~ lamCont ^^ { case _~ _ ~ cont => (v:String,_:ExprType) => cont(v,BoolType) } |  // IntType is the default
    "." ~ connP ~ opt("|"~bexpr) ^^ {
      case _ ~ c ~ Some(_~e)  => lam(_:String,_:ExprType,c | e)
      case _ ~ c ~ None       => lam(_:String,_:ExprType,c )
    }

  def seq: Parser[Connector] =
    prod~opt(";"~seq) ^^ {
      case co ~ Some(_~p) => co & p
      case co ~ None => co
    }

  def prod: Parser[Connector] =
    appl~opt("*"~prod) ^^ {
      case co ~ Some(_~p) => co * p
      case co ~ None => co
    }

  def appl: Parser[Connector] =
    pow~rep(ilit) ^^ {
      case co~es => es.foldLeft(co)((co2,e)=>co2(e))
    }

  def pow: Parser[Connector] =
    elemP~opt("^"~exponP) ^^ {
      case e~Some(_~a) => a(e)
      case e~None => e
    }

  def exponP: Parser[Connector=>Connector] =
    ilit~opt("<--"~ilit) ^? ({
      case Var(v) ~ (Some(_ ~ e)) => (_: Connector).:^(Var(v), e)
      case ie ~ None => (_: Connector) ^ ie
    }, {
      case ie1 ~ Some(_ ~ ie2) => s"${Show(ie1)} should be a variable, in '${Show(ie1)} <-- ${Show(ie2)}'."
      case s => s"unknown exponent: '$s'."
    }
      ) |
    "("~exponP~")" ^^ { case _~e~_ => e }

  def elemP: Parser[Connector] =
    "Tr"~"("~iexpr~")"~"("~connP~")" ^^ { case _~_~ie~_~_~c~_ => Trace(ie,c) }   |
    "sym"~"("~iexpr~","~iexpr~")"    ^^ { case _~_~ie1~_~ie2~_ => sym(ie1,ie2) } |
    bexpr~"?"~connP~"+"~connP        ^^ { case b~_~c1~_~c2 => (b ? c1) + c2 }    |
    litP~opt("!")                    ^^ { case l~o => if (o.isDefined) lam(n,l^n) else l}

  def litP: Parser[Connector] =
    "("~connP~")" ^^ { case _~c~_ => c } |
    identifier ^^ inferPrim


  ////////////////
  // expression //
  ////////////////

  def expr = iexpr | bexpr

  // boolean expressions

  def bexpr: Parser[BExpr] =
    disjP ~ opt("&"~bexpr) ^^ {
      case e1~Some(_~e2) => e1 & e2
      case e1~None       => e1
    }
  def disjP: Parser[BExpr] =
    equivP ~ opt("|"~disjP) ^^ {
      case e1~Some(_~e2) => e1 | e2
      case e1~None       => e1
    }
  def equivP: Parser[BExpr] =
    compP ~ opt("<->"~equivP) ^^ {
      case e1~Some(_~e2) => e1 | e2
      case e1~None       => e1
    } |
    "("~equivP~")" ^^ { case _~e~_ => e }
  def compP: Parser[BExpr] =
    ilit ~ bcontP ^^ { case e ~ co => co(e) } |
    blit
  def bcontP: Parser[IExpr=>BExpr] =
    "<=" ~ ilit ^^ { case _~e2 => (e1:IExpr) => e1 <= e2 } |
    ">=" ~ ilit ^^ { case _~e2 => (e1:IExpr) => e1 >= e2 } |
    "<"  ~ ilit ^^ { case _~e2 => (e1:IExpr) => e1 < e2 }  |
    ">"  ~ ilit ^^ { case _~e2 => (e1:IExpr) => e1 > e2 }  |
    "==" ~ ilit ^^ { case _~e2 => (e1:IExpr) => e1 === e2 }

  def blit: Parser[BExpr] =
    "true"     ^^ {_=>BVal(true)}               |
    "false"    ^^ {_=>BVal(false)}              |
    "!" ~ bexpr ^^ {case _ ~ e => Not(e)}       |
    identifier~":"~"B" ^^ {case s~_~_=>Var(s) } |
    identifier ^^ Var                           |
    "(" ~ bexpr ~ ")" ^^ {case _ ~ e ~ _ => e }

  // integer expressions
  def iexpr: Parser[IExpr] =
    ilit ~ ibop ~ iexpr ^^ {case l ~ op ~ r => op(l,r)} |
      ilit
  def ilit: Parser[IExpr] =
    intVal                                       |
      identifier~":"~"I" ^^ {case s~_~_=>Var(s) } |
      identifier ^^ Var                           |
      "(" ~ iexpr ~ ")" ^^ {case _ ~ e ~ _ => e }
  def intVal: Parser[IExpr] =
    """[0-9]+""".r ^^ { (s:String) => int2IExp(s.toInt) }
  def ibop: Parser[(IExpr,IExpr)=>IExpr] =
    "+"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 + e2 } |
      "-"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 - e2 } |
      "*"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 * e2 } |
      "/"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 / e2 }


}
