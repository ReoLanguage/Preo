package preo.frontend.mcrl2

import preo.ast._
import preo.common.Utils
import preo.frontend.{Eval, Substitution}




abstract class Family {
  type ProcSpec = (List[Action], List[ProcessName], List[Process], List[ProcessName], List[Action])

  /**@
    * Generates a list of ProcSpec. Each element of the list is an instance of an exponentiation.
    * \n.fifo**n (n \in 0..5) -----> [Fifo, Fifo, Fifo, Fifo, Fifo]
    * @param counter Counter class for unique process identifier
    * @param values List of values for the connector variables
    * @param make_starter If true, we need to generate an EntryNode instance
    * @return
    */
  def processes(counter: Counter, values : Map[Var, List[Int]], make_starter: Boolean): List[ProcSpec]

  /**@
    * return the vars for the manager multiaction
    * example:
    *   fifo**n
    *   controller({n -> 5}) = {a1,a2, a3, a4, a5}
    *   controller({n -> 2}) = {a1, a2}
    *
    * @param vars map of variable to integer
    */
  def choices(vars: Map[Var, Int]): Set[Int]

  /**@
    * Joins two different instances of the data type ProcSpec
    * @param p1 first instance
    * @param p2 second instance
    * @return
    */
  protected def join(p1: ProcSpec, p2: ProcSpec): ProcSpec = {
    val (inActions1, inProcs1, procs1, outProcs1, outActions1) = p1
    val (inActions2, inProcs2, procs2, outProcs2, outActions2) = p2
    (inActions1++inActions2, inProcs1++inProcs2, procs1++procs2, outProcs1++outProcs2, outActions1++outActions2)
  }


  /**@
    * returns the different families of the connector
    * \n.fifo**n (n --> [0..5]) = [{a1}, {a2}, {a3}, {a4}, {a5}]
    * @return
    */
  def families: Map[Int, Set[Action]]


  /**@
    * Replicates the current family into a new equal class
    * @return new family
    */
  def replicate: Family

  def size: Int
}



case class PrimFamily(prim: Prim) extends Family{
  private var controllerAction: Set[Action] = Set()

  override def processes(counter: Counter, range: Map[Var, List[Int]], make_starter: Boolean): List[ProcSpec] = {
    val channel = Util.primToChannel(prim, counter.getChannelCount)
    counter.incrementChannelCount(1)

    if(make_starter){
      val starterNode = Util.makeStarterNode(counter, channel.getName)
      controllerAction = starterNode.getActions
      (channel.before, channel.before.map(_ => starterNode.getName), List(channel, starterNode), channel.after.map(_ => starterNode.getName), channel.after) :: Nil
    }
    else {
      (channel.before, channel.before.map(_ => channel.getName), List(channel), channel.after.map(_ => channel.getName), channel.after) :: Nil
    }
  }

  override def choices(vars: Map[Var, Int]): Set[Int] = Set(1)

  override def families: Map[Int, Set[Action]] = if(controllerAction.nonEmpty) Map(1 -> controllerAction) else Map()

  override def replicate: Family = PrimFamily(prim)

  override def size = 1
}


case class SeqFamily(f1: Family, f2: Family) extends Family{


  override def processes(counter: Counter, range: Map[Var, List[Int]], starter_node: Boolean): List[ProcSpec] = {
      val result1 = f1.processes(counter, range, starter_node)
      val result2 = f2.processes(counter, range, false)
      var final_procs = Nil
      for((in1, namesIn1, procs1, namesOut1, out1) <- result1){
          for((in2, namesIn2, procs2, namesOut2, out2) <- result2){
            val inits: Map[ProcessName, Process] = Util.makeInits(namesOut1, out1, namesIn2, in2, counter)
            val replaced1 = namesIn1.map(name => {
              Util.getRealName(inits, name)
            })
            val replaced2 = namesOut2.map(name => {
              Util.getRealName(inits, name)
            })
            final_procs ++= List((in1, replaced1, procs1 ++ procs2 ++ inits.values.toSet.toList, replaced2, out2)) //inits1.values == inits2.values
          }
      }
      final_procs
  }

  override def size: Int = f1.size

  override def choices(vars: Map[Var, Int]): Set[Int] =  f1.choices(vars)

  override def replicate: Family = SeqFamily(f1.replicate, f2.replicate)

  override def families: Map[Int, Set[Action]] = f1.families


}


case class ParFamily(f1: Family, f2: Family) extends Family{

  override def processes(counter: Counter, range: Map[Var, List[Int]], make_starter: Boolean): List[ProcSpec] = {
    val result1 = f1.processes(counter, range, make_starter)
    val result2 = f2.processes(counter, range, make_starter)
    var final_procs:List[ProcSpec] = Nil
    for(r1 <- result1){
      for(r2 <- result2){
        final_procs ++= List(join(r1, r2))
      }
    }
    final_procs
  }

  override def size: Int =
    if(f1.size == 1 && f2.size == 1) 1
    else f1.size + f2.size

  override def choices(vars: Map[Var, Int]): Set[Int] = {
    if(f1.size == 1 && f2.size == 1) f1.choices(vars)
    else f1.choices(vars) ++ f2.choices(vars).map(i => i + f1.size)
  }

  override def replicate: Family = ParFamily(f1.replicate, f2.replicate)

  override def families: Map[Int,Set[Action]] = {
    val res1 = f1.families
    val res2 = f2.families
    if(res1.size == 1 && res2.size == 1) Map(1 -> (res1(1) ++ res2(1)))
    else res1 ++ res2.map(r => r._1 + res1.size -> r._2)
  }
}

//todo: update this
//case class IdFamily(i: IExpr) extends Family{
//
//  private var controllerActions: Set[Action] = Set()
//
//
//  override def processes(counter: Counter, range: Map[Var, Range], make_starter: Boolean): List[ProcSpec] = {
//    val vars = Utils.freeVars(i)
//    val maximum = Util.maximumValue(vars, i, range)
//    val minimum = Util.minimumValue(vars, i, range)
//
//
//    val channels = Util.makeSyncs(maximum, counter)
//    if(make_starter){
//      var result:ProcSpec = (Nil, Nil, Nil, Nil, Nil)
//      for(channel <- channels){
//        val starter_node = Util.makeStarterNode(counter, channel.getName)
//        controllerActions ++= starter_node.getActions
//        val channel_result = (channel.before, channel.before.map(_ => starter_node.getName), List(channel, starter_node),
//          channel.after.map(_ => starter_node.getName), channel.after)
//        result = join(result, channel_result)
//      }
//      result
//    }
//    else {
//      val ins: List[Action] = channels.flatMap(f => f.before)
//      val outs: List[Action] = channels.flatMap(f => f.after)
//      val namesIn: List[ProcessName] = channels.flatMap(f => f.before.map(_ => f.getName))
//      val namesOut: List[ProcessName] = channels.flatMap(f => f.after.map(_ => f.getName))
//      (ins, namesIn, channels, namesOut, outs)
//    }
//  }
//
//
//  override def choices(vars: Map[Var, Int]): Set[Action] = controllerActions
//
//  override def replicate: Family = IdFamily(i)
//}

case class AbsFamily(v: Var, et: ExprType, f: Family) extends Family{


  override def processes(counter: Counter, range: Map[Var, List[Int]], make_starter: Boolean): List[ProcSpec] = {
    if(range.get(v).isEmpty) f.processes(counter, range.updated(v, if(et == IntType) (1 to 5).toList else (-1 to 0).toList), true)
    else f.processes(counter, range, true)
  }

  override def size:Int = f.size


  override def choices(vars: Map[Var, Int]): Set[Int] = f.choices(vars)

  override def families: Map[Int,Set[Action]] = f.families

  override def replicate: Family = AbsFamily(v, et, f.replicate)
}



case class ExpFamily(a: IExpr, f: Family) extends Family{

  private var replFamilies: List[Family] = List(f)

  override def processes(counter: Counter,  range: Map[Var, List[Int]], make_starter: Boolean): List[ProcSpec] = {
    val vars = Utils.freeVars(a)
    val maximum = Util.maximumValue(vars, a, range)

    replFamilies ++= (2 to maximum).map(_ => f.replicate)

    replFamilies.flatMap(f_1 =>{
      val res = f_1.processes(counter, range, vars.nonEmpty)
      counter.incrementSyncCount1(1)
      counter.resetSyncCount2
      res
    })
  }

  override def size: Int = replFamilies.map(f => f.size).sum

  override def families: Map[Int, Set[Action]] = {
    var n = 1
    replFamilies.map(f => {
      val res = f.families.map(x => n -> x._2)
      n+=1
      res
    }).foldRight(Map(): Map[Int, Set[Action]])((x, y) => y ++ x)
  }


  override def choices(vars: Map[Var, Int]): Set[Int] = {
    val free_vars = Utils.freeVars(a)
    var res = a
    for(v <- free_vars){
      res = preo.frontend.Substitution(v, IVal(vars(v)))(res)
    }
    val exp_value:Int = Eval(res) match{
      case IVal(n) => n
    }
    var sum = 0
    replFamilies.take(exp_value).foldRight(Set(): Set[Int])((fam, res) =>{
      val x = res ++ fam.choices(vars).map(n => n+sum)
      sum = sum + fam.size
      x
    })
  }

  override def replicate: Family = ExpFamily(a, f.replicate)
}


//case class SymmetryFamily(i: IExpr, j: IExpr) extends Family{}



//case class ExpXFamily() extends Family







//case class TraceFamily() extends Family
//case class ChoiceFamily() extends Family
//case class AppFamily() extends Family
//case class RestrFamily extends Family



