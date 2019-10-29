package preo.lang

import preo.DSL
import preo.DSL._
import preo.ast._
import preo.examples.Repository
import preo.examples.Repository.unzip
import preo.frontend._

import scala.util.matching.Regex
import scala.util.parsing.combinator._

/**
  * Parser for Connectors, using parsing combinators.
  * For examples,check the unit tests - [[preo.TestParser]]
  * Created by jose on 07/03/2017.
  */
object Parser {
  def preoParser:Parser = new Parser {}

  type Result[T] = Either[String,T]

  def parse(c:String): Result[Connector] = preoParser.parse(c)
  def pa(c:String): Result[BExpr]        = preoParser.pa(c)
}

trait Parser extends RegexParsers {


  type Result[T] = Either[String,T]

  /**
    * Main function that parses a string.
    * @param c string representing a connector
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parse(c:String): Result[Connector] = toEither(parseAll(preo,c))
  def pa(c:String): Result[BExpr] = toEither(parseAll(bexpr,c))

  private def toEither[T](pr:ParseResult[T]): Either[String,T] = pr match {
    case Success(result, _) => Right(result)
    case f: NoSuccess       => Left(f.msg)
  }


  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  val keywords = Set("if","then","else")

  /** Parses basic primitives */
  def inferPrim(s:String): Connector =
    inferCorePrim.applyOrElse(s,str2conn)

  def inferCorePrim: PartialFunction[String,Connector] =  {
    case "fifo"       => fifo
    case "fifofull"   => fifofull
    case "drain"      => drain
    case "id"         => id
    case "ids"        => lam(n,id^n)
    case "dupl"       => dupl
    case "vdupl"      => vdupl
    case "lossy"      => lossy
    case "merger"     => merger
    case "vmerger"    => vmerger
//    case "timer"      => timer
    case "swap"       => swap
    case "xor"        => xor
    case "noSrc"      => Prim("noSrc",Port(IVal(1)),Port(IVal(0)))
    case "noSnk"      => Prim("noSnk",Port(IVal(0)),Port(IVal(1)))
    case "writer"     => Prim("writer",Port(IVal(0)),Port(IVal(1)),Set("component"))
    case "reader"     => Prim("reader",Port(IVal(1)),Port(IVal(0)),Set("component"))
    case s@"node"     => SubConnector(s,Repository.node, Nil)
    case s@"dupls"    => SubConnector(s,Repository.dupls, Nil)
    case s@"vdupls"   => SubConnector(s,Repository.vdupls, Nil)
    case s@"mergers"  => SubConnector(s,Repository.mergers, Nil)
    case s@"vmergers" => SubConnector(s,Repository.vmergers, Nil)
    case s@"xors"     => SubConnector(s,Repository.xors, Nil)
    case s@"zip"      => SubConnector(s,Repository.zip, Nil)
    case s@"unzip"    => SubConnector(s,Repository.unzip, Nil)
    case s@"exrouter" => SubConnector(s,Repository.exrouter, Nil)
    case s@"exrouters"=> SubConnector(s,Repository.nexrouter, Nil)
    case s@"fifoloop" => SubConnector(s,Repository.fifoloop, Nil)
    case s@"sequencer"=> SubConnector(s,Repository.sequencer, Nil)
    case s@"barrier"  => SubConnector(s,Repository.barrier, Nil)
    case s@"barriers" => SubConnector(s,Repository.barriers, Nil)
//    case _          => str2conn(s)
  }

  // rejects keywords
  def primitiveName: Parser[Connector] =
    identifier ^? ({
      case s if !keywords.contains(s) => inferPrim(s)
    },
      s => s"'$s' cannot be a connector name - reserved keyword")


  ///////////////
  /// Program ///
  ///////////////

  /** Top rule - already performs some semantic analysis (TreoLite.expand)
    * to replace primitives denoting Treo definitions by ther Preo counterparts. */
  def preo: Parser[Connector] =
    prog ^^ {p => TreoLite.treoASTToTreo(p,inferPrim)}

  def prog: Parser[Connector] =
    opt(annotate)~connP~opt("{"~>whereP<~"}")  ^^ {
      case Some(annotation) ~ co ~ Some(p) => SubConnector("", p(co), annotation)
      case Some(annotation) ~co ~ None     => SubConnector("", co, annotation)
      case None~ co ~ Some(p) =>  p(co)
      case None~ co ~ None    => co
    }

  def whereP: Parser[Connector=>Connector] =
    opt(annotate)~identifier~"="~prog~opt(","~>whereP) ^^ {
      case anns~s~_~co2~where => (co:Connector) =>
        Substitution.replacePrim(s, where.getOrElse((x:Connector)=>x)(co),
                                 SubConnector(s,co2, anns.getOrElse(Nil)))
    } |
    opt(annotate)~treoLite~opt(","~>whereP) ^^ {
      case anns~treoPrim~where => (co:Connector) =>
        // For each TreoLite, create a special Prim with its definition in the "extra" field.
        Substitution.replacePrim(treoPrim.name,  where.getOrElse((x:Connector)=>x)(co) ,
                                 SubConnector(treoPrim.name, treoPrim, anns.getOrElse(Nil)))
  //                                 SubConnector(s, TreoLite.treo2preo(co2).toConnector, anns.getOrElse(Nil)))
    }


  ////////////////
  /// TreoLite ///
  ////////////////

  /**
    * Parses a TreoLite definition "conn(p1?,p2!) = ..."
     * @return a special Prim, with the full TreoLite in the "extra" argument.
    */
  def treoLite: Parser[Prim] =
    identifier~"("~opt(trTypedArgs)~")"~"="~trConns ^^ {
      case s~_~args~_~_~cons =>
        Prim(s,1,1,Set(TreoLiteAST(args.getOrElse(Nil),cons)))
    }
  def trTypedArgs: Parser[List[TVar]] =
    trTypedArg ~ rep(","~>trTypedArg) ^^ {
      case a~more => a::more
    }
  def trTypedArg: Parser[TVar] =
    identifier ~ "\\?|\\!".r ^^ {
      case a~inout => TVar(a,inout=="?")}
  def trConns: Parser[List[TConnAST]] = rep(trExpr)
  def trExpr: Parser[TConnAST] =
    "task"~"<"~identifierCap~">"~"("~taskTreoParams~")" ^^ {
      case _~_~name~_~_~ps~_ =>
        val args = ps.map(p=> p._2)
        val conn = ps.map(p=>p._1)
        TConnAST(Right(SubConnector(name,conn.tail.foldLeft(conn.head)(_*_),List(Annotation("hide",None),Annotation("task",None)))),args)
    }|
    "timer|timeout".r~opt("<"~>intVal<~">")~"("~opt(trArgs)~")" ^^ {
      case name~Some(v)~_~args~_ => TConnAST(Right(SubConnector(name,Prim(name,1,1,Set("to:"+v.n)),List())),args.getOrElse(Nil))
      case name~None~_~args~_ => TConnAST(Right(SubConnector(name,Prim(name,1,1,Set("to:"+0)),List())),args.getOrElse(Nil))
    }|
    "await".r~opt("<"~>intVal<~">")~"("~opt(trArgs)~")" ^^ {
      case name~Some(v)~_~args~_ => TConnAST(Right(SubConnector(name,Prim(name,2,0,Set("to:"+v.n)),List())),args.getOrElse(Nil))
      case name~None~_~args~_ => TConnAST(Right(SubConnector(name,Prim(name,2,0,Set("to:"+0)),List())),args.getOrElse(Nil))
    }|
    identifier~"("~opt(trArgs)~")" ^^ {
      case name~_~args~_ => TConnAST(Left(name),args.getOrElse(Nil))
    }
  def trArgs: Parser[List[String]] =
    identifier~rep(","~>identifier) ^^ {
      case name~more => name :: more
    }

  def taskTreoParams:Parser[List[(Connector,String)]] =
    taskTreoParam ~ rep("," ~> taskTreoParam) ^^ { case p~ps => p::ps}

  val syncmode:Parser[Either[String,IVal]] =
    "NW|W".r ^^ {case m => Left(m)} |
    intVal ^^ {case v => Right(v)}

  def taskTreoParam:Parser[(Connector,String)] =
    syncmode ~ identifier ~ "?" ^^ {
      case Left(m)~name~_   => (if (m =="W") wget(Some(name)) else nwget(Some(name)),name)
      case Right(to)~name~_ => (toget(to.n,Some(name)),name)
    } |
    syncmode ~ identifier ~ "!" ~ opt("="~>intVal) ^^ {
      case Left(m)~name~_~Some(intval)    => (if (m =="W") wput(Some(intval.n),Some(name)) else nwput(Some(intval.n),Some(name)),name)
      case Left(m)~name~_~None            => (if (m =="W") wput(None,Some(name)) else nwput(None,Some(name)),name)
      case Right(to)~name~_~Some(intval)  => (toput(to.n,Some(intval.n),Some(name)),name)
      case Right(to)~name~_~None          => (toput(to.n,None,Some(name)),name)
    }


  ///////////////
  // Connector //
  ///////////////

  def connP: Parser[Connector] = lamP

  def lamP: Parser[Connector] =
    "\\"~>identifier~lamCont ^^ { case s ~ cont => cont(s,IntType)}  |
    seq

  def lamCont: Parser[(String,ExprType)=>Connector] =
    identifier ~ lamCont ^^ { case v ~ f  => lam(_:String,_:ExprType,f(v,IntType)) }          |
    ":" ~ "I"  ~> lamCont ^^ (cont => (v: String, et: ExprType) => cont(v, et)) |  // IntType is the default
    ":" ~ "B"  ~> lamCont ^^ (cont => (v: String, _: ExprType) => cont(v, BoolType)) |  // IntType is the default
    "." ~> connP ~ opt("|"~>bexpr) ^^ {
      case con ~ Some(e)  => lam(_:String,_:ExprType,con | e)
      case con ~ None     => lam(_:String,_:ExprType,con )
    }

  def seq: Parser[Connector] =
    ite~opt(";"~>seq) ^^ {
      case co ~ Some(p) => co & p
      case co ~ None => co
    }

  def ite: Parser[Connector] =
    "if"~bexpr~"then"~connP~"else"~connP ^^ { case _~e~_~c1~_~c2 => (e ? c1) + c2 }    |
    sum

  def sum: Parser[Connector] =
    prod~opt("+"~>sum) ^^ {
      case co ~ Some(co2) => lam(b,Choice(b,co,co2))
      case co ~ None => co
    }

  def prod: Parser[Connector] =
    appl~opt("*"~>prod) ^^ {
      case co ~ Some(p) => co * p
      case co ~ None => co
    }

  def appl: Parser[Connector] =
    // pow~rep(ilit) ^^ {
    //   case co~es => es.foldLeft(co)((co2,e)=>co2(e))
    // }  
    // pow~rep(blit) ^^ {
    //   case co~es => es.foldLeft(co)((co2,e)=>co2(e))
    // }
    pow~rep(expr) ^^ {
      case co~es => es.foldLeft(co)(
        (co2,e)=>co2(e)
      )
    }



  def pow: Parser[Connector] =
    elemP~opt("^"~>exponP) ^^ {
      case e~Some(a) => a(e)
      case e~None => e
    }

  def exponP: Parser[Connector=>Connector] =
    ilit~opt("<--"~>ilit) ^? ({
      case Var(v) ~ Some(e) => (_: Connector).:^(Var(v), e)
      case ie ~ None => (_: Connector) ^ ie
    }, {
      case ie1 ~ Some(ie2) => s"${Show(ie1)} should be a variable, in '${Show(ie1)} <-- ${Show(ie2)}'."
      case s => s"unknown exponent: '$s'."
    }
      ) |
    "("~>exponP<~")"

  def elemP: Parser[Connector] =
//    bexpr~"?"~connP~"+"~connP        ^^ { case e~_~c1~_~c2 => (e ? c1) + c2 }    |
    litP~opt("!")                    ^^ { case l~o => if (o.isDefined) lam(n,l^n) else l}


  def litP: Parser[Connector] =
    "loop"~"("~iexpr~")"~"("~connP~")" ^^ { case _~_~ie~_~_~con~_ => Trace(ie,con) }   |
    "sym"~"("~iexpr~","~iexpr~")"    ^^ { case _~_~ie1~_~ie2~_ => sym(ie1,ie2) } |
    "wr"~"("~nameP~")"               ^^ { case _~_~name~_ => Prim(name,Port(IVal(0)),Port(IVal(1)),Set("component"))} |
    "rd"~"("~nameP~")"               ^^ { case _~_~name~_ => Prim(name,Port(IVal(1)),Port(IVal(0)),Set("component"))} |
    "("~>connP<~")" |
    "timer|timeout".r~"("~intVal~")" ^^ {case name~_~ival~_ => Prim(name,1,1,Set("to:"+ival.n))} |
    "await"~opt("("~>intVal<~")")    ^^ {
      case name~Some(ival) => Prim(name,2,0,Set("to:"+ival.n))
      case name~None => Prim(name,2,0,Set("to:"+0))} |
    "task"~opt("<"~>identifierCap<~">")~"("~ taskParams ~")" ^^ {case _~name~_~ps~_ =>SubConnector(name.getOrElse("Task"),ps,List(Annotation("hide",None)))}|
    primitiveName

  def taskParams: Parser[Connector] =
    taskParam ~ rep(","~> taskParams) ^^ {case p~ps => ps.foldLeft(p)(_*_)}

//  def makeSequencerTask(cons:List[(String,String)]):Connector = {
//    val sequencer:Connector = Repository.eventSequencer(cons.size)
//    sequencer & (cons.map(c=>mkConFromOut(c)))
//  }

//  def mkConFromOut(c: (String, String)):Connector = c match {
//    case ("NW","!") => Prim("nbtimer",1,1,Set("to:"+0))
//    case ("NW","?") => (Prim("nbtimer",1,1,Set("to:"+0)) * id) & drain
//    case ("W","?") => (id * dupl) & (drain * id)
//    case ("W","!") => id
//    case (n,"!") if n.matches("""[0-9]+""") => Prim("nbtimer",1,1,Set("to:"+n.toInt))
//    case (n,">") if n.matches("""[0-9]+""") => (Prim("nbtimer",1,1,Set("to:"+n.toInt)) * id) & drain
//  }

  def taskParam: Parser[Connector] =
    syncmode ~ "?" ^^ {
      case Left(m)~_   => if (m =="W") wget(None) else nwget(None)
      case Right(to)~_ => toget(to.n,None)
    } |
    syncmode ~ "!" ~ opt("="~>intVal) ^^ {
      case Left(m)~_~Some(intval)   => if (m =="W") wput(Some(intval.n),None) else nwput(Some(intval.n),None)
      case Left(m)~_~None           => if (m =="W") wput(None,None) else nwput(None,None)
      case Right(to)~_~Some(intval) => toput(to.n,Some(intval.n),None)
      case Right(to)~_~None         => toput(to.n,None,None)
    }
  //    "NW" ~ "\\?|\\!".r   ^^ { case _~inout => if (inout =="!") nwput else nwget} |
//    "W" ~ "\\?|\\!".r    ^^ { case _~inout => if (inout =="!") wput else wget} |
//    intVal ~ "\\?|\\!".r ^^ { case to~inout => if (inout =="!") toput(to.n) else toget(to.n)}




  ////////////////
  // expression //
  ////////////////


  def expr: Parser[Expr] = // redundancy to give priority to true/false over variable "true"/"false"
    "(" ~> expr <~ ")" |
    identifierOrBool |
    iexpr | bexpr

  def identifierOrBool: Parser[Expr] =
    identifier ^? {
      case "true" => BVal(true)
      case "false" => BVal(false)
      case x if !keywords.contains(x) => Var(x)}

  // boolean expressions
  def bexpr: Parser[BExpr] =
    disjP ~ opt("&"~>bexpr) ^^ {
      case e1~Some(e2) => e1 & e2
      case e1~None     => e1
    }
  def disjP: Parser[BExpr] =
    equivP ~ opt("|"~>disjP) ^^ {
      case e1~Some(e2) => e1 | e2
      case e1~None     => e1
    }
  def equivP: Parser[BExpr] =
    compP ~ opt("<->"~>equivP) ^^ {
      case e1~Some(e2) => e1 | e2
      case e1~None     => e1
    } |
    "("~>equivP<~")"
  def compP: Parser[BExpr] =
    ilit ~ bcontP ^^ { case e ~ co => co(e) } |
    blit
  def bcontP: Parser[IExpr=>BExpr] =
    "<=" ~> ilit ^^ (e2 => (e1: IExpr) => e1 <= e2) |
    ">=" ~> ilit ^^ (e2 => (e1: IExpr) => e1 >= e2) |
    "<"  ~> ilit ^^ (e2 => (e1: IExpr) => e1 < e2)  |
    ">"  ~> ilit ^^ (e2 => (e1: IExpr) => e1 > e2)  |
    "==" ~> ilit ^^ (e2 => (e1: IExpr) => e1 === e2)

  def blit: Parser[BExpr] =
//    booleanVal |
    "!" ~> bexpr ^^ Not          |
//    identifier<~(":"~"B") ^^ Var |
    identifierOrBool<~opt(":"~"B") ^? ({
      case be: BExpr => be
    },
      ie => s"Integer not expected: $ie")       |
    "(" ~> bexpr <~ ")"


  // integer expressions
  def iexpr: Parser[IExpr] =
    ilit ~ ibop ~ iexpr ^^ {case l ~ op ~ r => op(l,r)} |
      ilit
  def ilit: Parser[IExpr] =
    intVal                                      |
    identifierOrBool ~ opt(":"~"I") ^? ({
      case (ie:IExpr)~_ => ie
    },{
      case be~_ => s"Boolean not expected: $be"
    }) |
//  identifier~":"~"I" ^^ {case s~_~_=>Var(s) } |
//    identifier ^^ Var                           |
    "(" ~> iexpr <~ ")"
  def intVal: Parser[IVal] =
    """[0-9]+""".r ^^ { s:String => IVal(s.toInt) }
  def ibop: Parser[(IExpr,IExpr)=>IExpr] =
    "+"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 + e2 } |
    "-"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 - e2 } |
    "*"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 * e2 } |
    "/"  ^^ {_ => (e1:IExpr,e2:IExpr) => e1 / e2 }

  ///////////////
  // Connector //
  ///////////////

  def annotate: Parser[List[Annotation]] =
    "["~>annotations<~"]"

  def annotations: Parser[List[Annotation]] =
    annotation~opt(","~>annotations) ^^ {
      case ann~Some(rest) => ann :: rest
      case ann~None => ann :: Nil
    }

  def annotation:Parser[Annotation] =
    identifierCap~opt(":"~>expr) ^^ {
      case s~e => Annotation(s,e)
    }
}
