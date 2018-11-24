package preo.lang

import preo.frontend.mcrl2._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object FormulaParser extends RegexParsers {

  /**
    * Main function that parses a string.
    * @param c string representing a connector
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parse(c:String): ParseResult[Formula] = parseAll(formulas,c)

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  ///////////////
  /// Formula ///
  ///////////////

  def formulas: Parser[Formula] =
    rep1(formula) ^^ (_.reduce(AndF))

  def formula: Parser[Formula] =
      "true" ^^^ TrueF  |
      "false" ^^^ FalseF |
      "<" ~ regForm ~ ">" ~ formula ^^
        {case _~p~_~f => Diamond(p,f)} |
      "[" ~ regForm ~ "]" ~ formula ^^
        {case _~p~_~f => MBox(p,f)} |
      "@.." ~> formula ^^
          Up |
      "@" ~> container  ~ formula ^^
        {case c~f => At(c,f)}

  def container: Parser[Container] =
    identifier ^^ Container

  def regForm: Parser[RegularF] =
    regSum ~ opt("."~>regForm) ^^ {
      case f1 ~ Some(f2) => SeqRF(f1,f2)
      case f1 ~ _ => f1
    }

  def regSum: Parser[RegularF] =
    starRF ~ opt("+"~>regSum) ^^ {
      case f1 ~ Some(f2) => f1 + f2
      case f1 ~ _ => f1
    }
  def starRF: Parser[RegularF] =
    actionLit<~"*" ^^ Kleene |
    actionFormNoOr

  def actionLit: Parser[ActionF] =
    "tau" ^^^ Tau |
    "all" ^^^ AllA |
    "none" ^^^ NoneA |
    "!"~>actionLit ^^ NegFP |
    "("~>actionForm<~")" |
    container

  // used to parse regular expressions -- use parenthesis to use the ActionOr.
  def actionFormNoOr: Parser[RegularF] =
    actionLit~opt("&"~>actionForm) ^^ {
      case a~None => a
      case a~Some(a2) => a & a2
    }


  def actionForm: Parser[ActionF] =
    actionOr~opt("&"~>actionForm) ^^ {
      case a~None => a
      case a~Some(a2) => a & a2
    }

  def actionOr: Parser[ActionF] =
    actionLit~opt("+"~>actionOr) ^^ {
      case a~None => a
      case a~Some(a2) => a || a2
    }

}