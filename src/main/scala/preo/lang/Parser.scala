package preo.lang

import preo.DSL._
import preo.ast._
import preo.frontend.Substitution

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
  def parse(c:String): ParseResult[Connector] = parseAll(conn,c)
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
    case _          => str2conn(s)
  }

  def conn: Parser[Connector] =
    lit ~ combinator ^^ {case l ~ f => f(l) }

  def combinator: Parser[Connector => Connector] =
    "&" ~ conn   ^^ {case _~ c => (_:Connector) & c} |
      "*" ~ conn   ^^ {case _~ c => (_:Connector) * c} |
      "!"          ^^ {_ => (c:Connector) =>  lam("n":I,c^"n") } | //  IAbs(IVar("n"),c^IVar("n"))} |
      "^" ~ "("~identifier ~ "<--" ~ iexpr ~")" ^^
        {case _~_~x~_~a~_=>(_:Connector)^(x<--a)}|
      "^" ~ iexpr  ^^ {case _~ i => (_:Connector) ^ i} |
      "|" ~ bexpr  ^^ {case _~ b => (_:Connector) | b} |
      bexpr        ^^ {b => (_: Connector)(b)}         |
      iexpr        ^^ {e => (_: Connector)(e)}         |
      ""           ^^ { _ => x:Connector => x}

  // Connector Literals:
  def lit: Parser[Connector] =
    "Tr_"~iexpr~conn                ^^ {case _~e~c     => Trace(Port(e),c)}                 |
    "sym"~"("~iexpr~","~iexpr~")"   ^^ {case _~_~e1~_~e2~_ => Symmetry(Port(e1),Port(e2))}  |
    bexpr ~ "?" ~ conn ~ "+" ~ conn ^^ {case b~_~c1~_~c2 => Choice(b,c1,c2)}                |
    "\\" ~ identifier ~ lambdaCont  ^^ {case _~ s ~ cont => cont(str2IVar(s))}              |
    "(" ~ conn ~ ")"                ^^ {case _ ~ c ~ _ => c}                                |
    "(" ~ conn ~")"~"!"             ^^ {case _~c~_~_ => Abs(IVar("n"),c^IVar("n"))}        |
    identifier~"!"                  ^^ {case s~_ => Abs(IVar("n"),inferPrim(s)^IVar("n"))} |
    identifier~"="~conn~";"~conn    ^^ {case s~_~c1~_~c2 => Substitution.replacePrim(s,c2,c1)} |
    identifier                      ^^ { inferPrim }

  def lambdaCont: Parser[Var=>Connector] =
    "." ~ conn                   ^^ {case _~ c   => lam(_:Var,c)}                   |
    identifier ~ lambdaCont      ^^ { case v ~ f => lam(_:Var,f(str2IVar(v))) } |
    ":" ~ "I" ~ lambdaCont ^^ { case _~ _ ~ cont => (v:Var) => cont(v) } | // IVar is the default
    ":" ~ "B" ~ lambdaCont ^^ { case _~ _ ~ cont => (v:Var) => cont(str2BVar(v.x)) } // IVar is the default
//    ":" ~ ("I"|"B") ~ lambdaCont ^^ {
//      case _~ "I" ~ cont => (v:Var) => cont(v) // IVar is the default
//      case _~ "B" ~ cont => (v:Var) => cont(str2BVar(v.x))
//    }

  // boolean expressions
  def bexpr: Parser[BExpr] =
    blit ~ bbop ~ bexpr ^^ {case l ~ op ~ r => op(l,r)} |
    ilit ~ bibop ~ iexpr ^^ {case l ~ op ~ r => op(l,r)} |
    "!" ~ bexpr ^^ {case _ ~ e => Not(e)} |
    blit
  def blit: Parser[BExpr] =
    "true"     ^^ {_=>BVal(true)}                |
    "false"    ^^ {_=>BVal(false)}               |
    identifier~":"~"B" ^^ {case s~_~_=>BVar(s) } |
    identifier ^^ BVar                           |
    "(" ~ bexpr ~ ")" ^^ {case _ ~ e ~ _ => e }
  def bbop: Parser[(BExpr,BExpr)=>BExpr] =
    "&"  ^^ {_ => (e1:BExpr,e2:BExpr) => e1 & e2 } |
    "|"  ^^ {_ => (e1:BExpr,e2:BExpr) => e1 | e2 } |
    "<->" ^^ {_ => (e1:BExpr,e2:BExpr) => e1 === e2 }
  def bibop: Parser[(IExpr,IExpr)=>BExpr] =
    "<=" ^^ {_ => (e1:IExpr,e2:IExpr) => e1 <= e2 } |
    ">=" ^^ {_ => (e1:IExpr,e2:IExpr) => e1 >= e2 } |
    "<"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 < e2 }  |
    ">"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 > e2 }  |
    "==" ^^ {_ => (e1:IExpr,e2:IExpr) => e1 === e2 }

  // integer expressions
  def iexpr: Parser[IExpr] =
    ilit ~ ibop ~ iexpr ^^ {case l ~ op ~ r => op(l,r)} |
    ilit
  def ilit: Parser[IExpr] =
    intVal                                       |
    identifier~":"~"I" ^^ {case s~_~_=>IVar(s) } |
    identifier ^^ IVar                           |
    "(" ~ iexpr ~ ")" ^^ {case _ ~ e ~ _ => e }
  def intVal: Parser[IExpr] =
    """[0-9]+""".r ^^ { (s:String) => int2IExp(s.toInt) }
  def ibop: Parser[(IExpr,IExpr)=>IExpr] =
    "+"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 + e2 } |
    "-"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 - e2 } |
    "*"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 * e2 } |
    "/"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 / e2 }


  /*
  conn       := lit combinator
  combinator :=  "&" ~ conn   ^^ {case _~ c => (_:Connector) & c} |
      "*" ~ conn   ^^ {case _~ c => (_:Connector) * c} |
      "!"          ^^ {_ => (c:Connector) =>  lam("n":I,c^"n") } | //  IAbs(IVar("n"),c^IVar("n"))} |
      "^" ~ "("~identifier ~ "<--" ~ iexpr ~")" ^^
        {case _~_~x~_~a~_=>(_:Connector)^(x<--a)}|
      "^" ~ iexpr  ^^ {case _~ i => (_:Connector) ^ i} |
      "|" ~ bexpr  ^^ {case _~ b => (_:Connector) | b} |
      bexpr        ^^ {b => (_: Connector)(b)}         |
      iexpr        ^^ {e => (_: Connector)(e)}         |
      ""           ^^
   */
}
