package preo.frontend

import preo.DSL
import preo.ast._
import preo.common.{GenerationException, TypeCheckException}

case class TreoLite(args:List[TVar],conns:List[TConn])
case class TVar(name:String,isIn:Boolean) {def isOut: Boolean = !isIn}
case class TConn(cc:CoreConnector,args:List[TVar])

// before type inference
case class TreoLiteAST(args:List[TVar],conns:List[TConnAST])
case class TConnAST(name:Either[String,Connector],args:List[String])

object TreoLite {
  /**
    * add In/Out fields to a connector, by trying to infer its type.
    * @param c Treo connector to be extended with types to its arguments
    * @param inferPrim function that infers the type of te primitive name
    * @return updated Treo connector with typed arguments
    */
  def inferTypes(c:TConnAST,inferPrim:String=>Connector): TConn = {
    val preoConn: Connector = c.name.fold(inferPrim,(x:Connector)=>x) // inferPrim(c.name)
    // TODO: changing here
    preo.DSL.unsafeTypeOf(preoConn) match {
      case (Type(args, Port(IVal(i)),Port(IVal(j)),BVal(true),_),BVal(true)) if args.vars.isEmpty =>
        if (c.args.size != i+j)
//          fail(s"${c.name} has ${c.args.size} arguments, but found ${i+j}.")
          throw new TypeCheckException(s"${c.name} has ${c.args.size} arguments, but found ${i+j}.")
        else
          TConn(Eval.unsafeReduce(preoConn), // must work with given type
                c.args.zipWithIndex.map(pair => TVar(pair._1,pair._2 < i)))
      case _ =>
//        fail(s"Not a concrete primitive: ${Show(preoConn)}")
        throw new TypeCheckException(s"Not a concrete primitive: ${Show(preoConn)}")
    }
  }

  def inferTypes(c:TreoLiteAST,inferPrim:String=>Connector): TreoLite =
    TreoLite(c.args,c.conns.map(inferTypes(_,inferPrim)))

  /**
    * Converts a [[TreoLite]] into a [[CoreConnector]].
    * @param t TreoLite to be converted
    * @param split dupl or xor connector - how to transform a merge into a splitting connector
    * @return Resulting CoreConnector
    */
  def treo2preo(t:TreoLite,split:String): CoreConnector = {
    // aux function
    def mkPar(c1:CoreConnector, c2:CoreConnector) = c1*c2
    def mkSeq(c1:CoreConnector, c2:CoreConnector) = c1&c2
    def id(i:Int) = CId(CoreInterface(i))
    // 1. build core in parallel
    val core = t.conns
                .map(_.cc)
                .reduceOption(mkPar)
                .getOrElse(id(0))
    // 2. collect inputs and outputs
    val (insV,outsV) = t.conns.flatMap(_.args).partition(_.isIn)
    val (ins,outs) = (insV.map(_.name),outsV.map(_.name))
    // 3. collect shared ports
    val shared = ins.toSet intersect outs.toSet
    // 3b. check if boundaries are not mixed
    val boundaryShared  = shared intersect t.args.map(_.name).toSet
    if (boundaryShared.nonEmpty)
      throw new GenerationException(s"Treo: boundary nodes cannot be mixed nodes: ${boundaryShared.mkString(",")}")
    // 4. determine desired order (first args, then unshared, then shared (sorted))
    val insFinal  = sort(ins, t.args.filter(_.isIn ).map(_.name),shared)
    val outsFinal = sort(outs,t.args.filter(_.isOut).map(_.name),shared)
    // 5. build path recursively
    val outsPath: List[List[CoreConnector]] = buildPath(outs,outsFinal)
    val insPath: List[List[CoreConnector]] = reverse(buildPath(ins,insFinal),split)
    // ins is same, but reversed
    // 6. create pre and post connectors
    val insConn  = insPath
      .map(_.reduceOption(mkPar)
            .getOrElse(id(0)))
      .reduceOption(mkSeq).getOrElse(id(ins. size))
    val outsConn = outsPath
      .map(_.reduceOption(mkPar)
            .getOrElse(id(0)))
      .reduceOption(mkSeq).getOrElse(id(outs.size))
    // combine result
    CTrace(CoreInterface(shared.size),insConn & core & outsConn)
  }

  /**
    * Creates the desired order of a list of arguments
    * @param args arguments found inside the definition
    * @param top arguments that must be on top (they are in the outer interface)
    * @param shared ports that are shared (and must loop)
    * @return reorder of the ports to the desired order
    */
  private def sort(args: List[String], top: List[String], shared: Set[String]): List[String] = {
    val l2 = args.filterNot(top contains _)
    val (l3,_) = l2.partition(shared)
    // top maintains the order, l4 does not care about order (now deleted), l3 must be sorted.
    val res = top ++ l3.sorted
//    assert(res.size == res.toSet.size,s"unexpected replicas: ${res.mkString(",")} - ignored ${l4.mkString(",")}.") // todo: Delete
    res.distinct
  }

  /**
    * Generates a matrix of connectors to be applied, including mergers, dupls, ids, noSrc, and noSnk.
    * @param from starting point
    * @param to ending point
    * @param acc transformations made so far
    * @return list of transformations until the "to" is reached
    */
  private def buildPath(from: List[String], to: List[String])
      : List[List[CoreConnector]] = {
    // println(s"BP - ${from.mkString(".")} -> ${to.mkString(".")}")
    // cover loose ends (unique "from" and unique "to")
    val from2 = for (p <- from) yield (p,to   contains p)
    val to2   = for (p <- to  ) yield (p,from contains p)
    val from3 = from2.filter(_._2).map(_._1) // 'from' with shared names with 'to'
    val to3   = to2  .filter(_._2).map(_._1) // 'to' with shared names with 'from'
    val plugFrom = if (from3.size==from.size) Nil
                   else List(from2.map(x => if (x._2) cID else cNoSrc))
    val plugTo   = if (to3.size==to.size) Nil
                   else List(to2.map(x =>   if (x._2) cID else cNoSnk))
    val inner = buildPath2(from3,to3,Nil)
    // check if there are repeated not-needed ends
    val fromToPlug = from2.filterNot(_._2).map(_._1)
    if (fromToPlug != fromToPlug.distinct)
      throw new GenerationException(s"Unused mixed port(s) ${fromToPlug.diff(fromToPlug.distinct).distinct.mkString(",")}")
    //
//    println(s"BP1 - ${plugFrom.map(_.map((x:CoreConnector) => Show.short(x.toConnector))).mkString(",")} / "+
//                  s"${plugTo.map(_.map((x:CoreConnector) => Show.short(x.toConnector))).mkString(",")}")
    plugFrom ++ inner ++ plugTo
  }

  private val cID     = Eval.unsafeReduce(DSL.id)
  private val cNoSnk  = Eval.unsafeReduce(DSL.noSnk)
  private val cNoSrc  = Eval.unsafeReduce(DSL.noSrc)
  private val cSwap   = Eval.unsafeReduce(DSL.swap)
  private val cMerger = Eval.unsafeReduce(DSL.merger)

  private def buildPath2(from: List[String], to: List[String], acc: List[List[CoreConnector]])
      : List[List[CoreConnector]] = {
//    println(s"BP2 - from ${from.mkString(",")} to ${to.mkString(",")}") // -- ${acc.map(_.map(_) mkString()}")
    if (from == to) acc.reverse
    else { // from and to cannot be empty
      var last = from.head
      var run: List[CoreConnector] = Nil
      var from2: List[String] = Nil
      var addLast = true
      for (next <- from.tail) {
        if (addLast) {
          if (next==last){
//            println(s"    + merging ${next}")
            run ::= cMerger
            from2 ::= last // skip one element
            addLast = false
          }
          else if (isBefore(last, next, to)){
//            println(s"    + leaving ${last}")
            run ::= cID
            from2 ::= last
          }
          else {
//            println(s"    + swapping $last with $next")
            run ::= cSwap
            from2 = last::next::from2 // swapping elements
            addLast = false
          }
        }
        else addLast = true
        last = next
      }
      if (addLast) {
//        println(s"    + leaving last ${last}")
        run ::= cID
        from2 ::= last
      }
      buildPath2(from2.reverse,to,run.reverse::acc)
    }
  }

  private def isBefore(a:String,b:String,ref:List[String]): Boolean = {
    val fst = ref.find(x => x==a || x==b)
    fst match {
      case Some(x) => x==a
      case _ => throw new RuntimeException(s"did not find $a nor $b in [${ref.mkString(",")}]")
    }
  }

  private def reverse(list: List[List[CoreConnector]],dupl: String): List[List[CoreConnector]] = {
    list.reverse.map(_.map {
      case CPrim("merger", i, j, extra) => CPrim(dupl    , j, i, extra)
      case CPrim(`dupl`  , i, j, extra) => CPrim("merger", j, i, extra)
      case CPrim("noSrc" , i, j, extra) => CPrim("noSnk" , j, i, extra)
      case CPrim("noSnk" , i, j, extra) => CPrim("noSrc" , j, i, extra)
      case x => x
    })
  }

  /////////////////

  def expand(conn: Connector,infer: String=>Connector, split: String): Connector = conn match {
    case Prim(name, i, j, trs) => trs.toList match {
      case List(tr:TreoLiteAST) =>
        val treo = inferTypes(tr,infer)
        treo2preo(treo,split).toConnector
      case _ => conn
    }
    case Seq(c1, c2) => Seq(expand(c1,infer,split),expand(c2,infer,split))
    case Par(c1, c2) => Par(expand(c1,infer,split),expand(c2,infer,split))
    case Trace(i, c) => Trace(i,expand(c,infer,split))

    case SubConnector(name, c1, anns) => SubConnector(name,expand(c1,infer,split),anns)

    case Exp(a,c) => Exp(a,expand(c,infer,split))
    case ExpX(x,a,c) => ExpX(x,a,expand(c,infer,split))
    case Choice(b,c1,c2) => Choice(b,expand(c1,infer,split),expand(c2,infer,split))
    case Abs(x,et,c) => Abs(x,et,expand(c,infer,split))
    case App(c,a) => App(expand(c,infer,split),a)

    case Restr(c,phi) => Restr(expand(c,infer,split),phi)

    case _ => conn
  }



}

