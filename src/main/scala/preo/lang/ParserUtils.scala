package preo.lang

import preo.DSL
import preo.ast.{Connector, CoreConnector}
import preo.common.{GenerationException, TypeCheckException}
import preo.frontend.{Eval, Show}
import preo.frontend.mcrl2.{Formula, Model}

object ParserUtils {
  /**
    * Parses a string, returning either an error message or a core connector.
    */
  def parseCoreConnector(in:String): Either[String,CoreConnector] = {
    DSL.parseWithError(in) match {
      case preo.lang.Parser.Success(result,_) =>
        try {
          val reduced: Connector = Eval.instantiate(result)
          Right(Eval.reduce(reduced))
        } catch {
          case e: TypeCheckException =>
            Left("Type error: " + e.getMessage)
          case e: GenerationException =>
            Left("Generation failed: " + e.getMessage)
        }
      case preo.lang.Parser.Failure(emsg,_) =>
        Left("Parser failure: " + emsg + " in "+in)
      case preo.lang.Parser.Error(emsg,_) =>
        Left("Parser error: " + emsg + " in "+in)
    }
  }

  /**
    * Parses a string, returing either an error message or a formula
    */
  def parseFormula(c:String): Either[String,Formula] = {
    FormulaParser.parse(c) match {
      case FormulaParser.Success(result, _) => Right(result)
      case f: FormulaParser.NoSuccess       => Left(f.msg)
    }
  }

  /**
    * Parse a core connector and a formula, and
    * Build a mCRL2 specification (Model) and a mCRL2 expanded formula (String),
    * by hiding in the model what is needed by the formula.
    */
  def parseAndHide(raw_conn: String, raw_form: String): Either[String,(Model,String)] = {
    val form = ParserUtils.parseFormula(raw_form)
    val conn = ParserUtils.parseCoreConnector(raw_conn)
    (form,conn) match {
      case (Left(s),_) => Left(s)
      case (_,Left(s)) => Left(s)
      case (Right(f),Right(c)) =>
        hideBasedOnFormula(f, c)
    }
  }

  /**
    * Build a mCRL2 specification (Model) and a mCRL2 expanded formula (String),
    * by hiding in the model what is needed by the formula.
    */
  def hideBasedOnFormula(form: Formula, conn: CoreConnector): Either[String, (Model, String)] = {
    // we can collect hide info here...
//    println(s"# have ${Show(conn)}")
    val prefixes = Formula.notToHide(form)
//    println(s"# not hiding ${prefixes.map(_.mkString("[", "/", "]")).mkString(",")}")
    // we can override hide info here...
    val c2 = Model.unhideUntilPrefix(conn, prefixes)
//    println(s"# got ${Show(c2)}")
    var err = ""
    val model = preo.frontend.mcrl2.Model(c2)
    val mcrl2form = Formula.formula2mCRL2(form, model.getMultiActionsMap, err = _)
    if (err == "")
      Right((model, mcrl2form))
    else
      Left(err)
  }
}
