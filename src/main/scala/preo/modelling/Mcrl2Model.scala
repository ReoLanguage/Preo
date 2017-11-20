package preo.modelling

import preo.ast.{CId, CPar, CPrim, CSeq, CSymmetry, CTrace, CoreConnector}

class Mcrl2Model(act: Seq[Mcrl2Action], proc: List[Mcrl2Process], init: Mcrl2Process){
  override def toString: String = {
    val acts = Mcrl2Process.toString(act);
    val procs = ""
    for(p <- proc){
      val processedP = p.toString;
      procs + s"$processedP;\n"
    }
    val initProc = init.toString;
    s"""
       |act
       |  $acts
       |proc
       |  $procs
       |init
       |  $initProc;
     """.stripMargin
  }
}


/**
  * So far every mcrl2process puts the name at the end of the connector (ex: Fifo1 = (a.b).Fifo1)
  */
class Mcrl2Process(name: String, ops: Mcrl2Operator){
  override def toString: String = s"$name = (${ops.toString}).$name"
}

object Mcrl2Process{
  def toString(act: Seq[Mcrl2Action]): String = act match{
    case x :: y:: rest => x.toString + ", " + toString(y :: rest)
    case x:: Nil => x.toString + ";"
    case Nil => ""
  }
}


abstract class Mcrl2Operator{
  abstract def left_vars: Seq[Mcrl2Action];
  abstract def right_vars: Seq[Mcrl2Action];
  abstract def total_vars: Seq[Mcrl2Action];
}

//atomic
case class Mcrl2Action(name: String) extends Mcrl2Operator{
  override def toString: String = name

  override def left_vars: Seq[Mcrl2Action] = Nil

  override def right_vars: Seq[Mcrl2Action] = Nil

  override def total_vars: Seq[Mcrl2Action] = Seq(this)
}

//operations
case class seq(before: Mcrl2Operator, after: Mcrl2Operator) extends Mcrl2Operator{
  override def toString: String = s"${before.toString} . ${after.toString}"

  override def left_vars: Seq[Mcrl2Action] = before.total_vars

  override def right_vars: Seq[Mcrl2Action] = after.total_vars

  override def total_vars: Seq[Mcrl2Action] = before.total_vars ++ after.total_vars
}

case class choice(left: Mcrl2Operator, right: Mcrl2Operator) extends Mcrl2Operator{
  override def toString: String = s"${left.toString} + ${right.toString}"

  override def left_vars: Seq[Mcrl2Action] = left.left_vars ++ right.left_vars

  override def right_vars: Seq[Mcrl2Action] = left.right_vars ++ right.right_vars

  override def total_vars: Seq[Mcrl2Action] = left.total_vars ++ right.total_vars
}

case class par(left: Mcrl2Operator, right:Mcrl2Operator) extends Mcrl2Operator{
  override def toString: String = s"${left.toString} || ${right.toString}"

  override def left_vars: Seq[Mcrl2Action] = left.total_vars

  override def right_vars: Seq[Mcrl2Action] = right.total_vars

  override def total_vars: Seq[Mcrl2Action] = left.total_vars ++ right.total_vars
}

case class multiAction(left: Mcrl2Action, right: Mcrl2Action) extends Mcrl2Operator{
  override def toString: String = s"${left.toString} | ${right.toString}"

  override def left_vars: Seq[Mcrl2Action] = left.total_vars

  override def right_vars: Seq[Mcrl2Action] = right.total_vars

  override def total_vars: Seq[Mcrl2Action] = left.total_vars ++ right.total_vars
}

case class comm(vars: List[Tuple3[Mcrl2Action, Mcrl2Action, Mcrl2Action]], in: Mcrl2Operator) extends Mcrl2Operator{
  override def toString: String = s"""comm({${toString(vars)}}, ${in.toString})"""

  def toString(vars: List[Tuple3[Mcrl2Action, Mcrl2Action, Mcrl2Action]]): String = vars match{
    case tupple :: y :: rest => s"""${tupple._1.toString}|${tupple._2.toString} -> ${tupple._2.toString}, """ + toString(y:: rest)
    case tupple :: Nil => s"""${tupple._1.toString}|${tupple._2.toString} -> ${tupple._2.toString}"""
    case Nil => ""
  }

  override def left_vars: Seq[Mcrl2Action] = in.left_vars

  override def right_vars: Seq[Mcrl2Action] = in.right_vars

  //this is not like this, fix that todo
  override def total_vars: Seq[Mcrl2Action] = in.total_vars
}

case class allow(vars: Seq[Mcrl2Action], in: Mcrl2Operator) extends Mcrl2Operator{
  override def toString: String = s"""allow({${Mcrl2Process.toString(vars)}}, ${in.toString})"""

  override def left_vars: Seq[Mcrl2Action] = in.left_vars

  override def right_vars: Seq[Mcrl2Action] = in.right_vars

  override def total_vars: Seq[Mcrl2Action] = in.total_vars
}

case class block(vars: Seq[Mcrl2Action], in: Mcrl2Operator) extends Mcrl2Operator{
  override def toString: String = s"""block({${Mcrl2Process.toString(vars)}, ${in.toString})"""

  override def left_vars: Seq[Mcrl2Action] = in.left_vars

  override def right_vars: Seq[Mcrl2Action] = in.right_vars

  override def total_vars: Seq[Mcrl2Action] = in.total_vars
}

//todo: adicionar um método para converter simples conetores em Mcrl2 Models.
//todo: adicionar outros métodos necessários, ex: if's e elses
//todo: adicionar isto ao site

object Mcrl2Model{
  def apply(con: CoreConnector): Mcrl2Model = {
    var actions = Seq[Mcrl2Action]()
    var procs = List[Mcrl2Process()
    (actions, procs) = conToProcess(con, actions, procs, 0, 0)
    //temos que começar pela definição das variáveis
    //adicionar os processos
    //concatenar os processos para o init 
  }

  private def conToProcess(con: CoreConnector, actions: Seq[Mcrl2Action], processes: List[Mcrl2Process], countproc: Int, countvars:Int): List[Mcrl2Process] = con match{
    case CSeq(c1, c2) =>

    case CPar(c1, c2) =>
    //case CId(i) => todo: como criar a conversão do id
    case CSymmetry(i, j) =>
    case CTrace(i, c) =>
    case x@CPrim(name, i, j, extra) => List(convertPrimitive(x, countproc, countvars))
  }


  private def convertPrimitive(prim: CPrim, countvars: Int, countprocs: Int): Mcrl2Process = prim match{
    case CPrim("fifo", _, _, _) => {
      val procName = "Fifo"+countprocs.toString
      val firstAction = Mcrl2Action("X"+countvars.toString)
      val secondAction = Mcrl2Action("X"+(countvars+1).toString)
      new Mcrl2Process(procName, seq(firstAction, secondAction))
    }
    case CPrim("fifofull", a, b, _) => {
      val procName = "FifoFull" + countprocs.toString
      val firstAction = Mcrl2Action("X" + countvars.toString)
      val secondAction = Mcrl2Action("X" + (countvars + 1).toString)
      new Mcrl2Process(procName, seq(secondAction, firstAction))
    }
    case CPrim("lossy",a,b, _ ) => {
      val procName = "Lossy" + countprocs.toString
      val firstAction = Mcrl2Action("X" + countvars.toString)
      val secondAction = Mcrl2Action("X" + (countvars + 1).toString)
      new Mcrl2Process(procName, choice(firstAction, seq(secondAction, firstAction)))
    }
    case CPrim("merger",a,b,_) => {
      val procName = "Merger" + countprocs.toString
      val firstAction = Mcrl2Action("X" + countvars.toString)
      val secondAction = Mcrl2Action("X" + (countvars + 1).toString)
      val thirdAction = Mcrl2Action("X" + (countvars + 2).toString)
      new Mcrl2Process(procName, choice(par(firstAction, thirdAction), par(secondAction, thirdAction)))
    }
    case CPrim("dupl",a,b, _) => {
      val procName = "Dupl" + countprocs.toString
      val firstAction = Mcrl2Action("X" + countvars.toString)
      val secondAction = Mcrl2Action("X" + (countvars + 1).toString)
      val thirdAction = Mcrl2Action("X" + (countvars + 2).toString)
      new Mcrl2Process(procName, par(firstAction, par(secondAction, thirdAction)))
    }
    case CPrim("drain",a,b, _) => {
      val procName = "Drain" + countprocs.toString
      val firstAction = Mcrl2Action("X" + countvars.toString)
      val secondAction = Mcrl2Action("X" + (countvars + 1).toString)
      new Mcrl2Process(procName, par(firstAction, secondAction))
    }
    case CPrim(name, a, b, _) => {
      val procName = name + countprocs.toString
      val firstAction = Mcrl2Action("X" + countvars.toString)
      val secondAction = Mcrl2Action("X" + (countvars + 1).toString)
      new Mcrl2Process(procName, par(firstAction, secondAction))
    }
  }
}