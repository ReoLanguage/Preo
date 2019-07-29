package preo.frontend

import preo.DSL
import preo.ast._
import preo.common.{GenerationException, TypeCheckException}

// TreoLite maps [IO arguments] to [CoreConnectors with IO arguments]
case class TreoLite(args:List[TVar],conns:List[TConn])
case class TVar(name:String,isIn:Boolean) {def isOut: Boolean = !isIn}
case class TConn(cc:CoreConnector,args:List[TVar])

// before type inference
// TreoLiteAST maps [IO Arguments] to [[names or connectors] with port names]
case class TreoLiteAST(args:List[TVar],conns:List[TConnAST])
case class TConnAST(name:Either[String,Connector],args:List[String]) {
  def getName:String = name match {
    case Left(value) => value
    case Right(value) => Show(value)
  }
}

// New notion of TreoLite - Work in Progress!
case class TreoLiteConn(args:List[TVar], conns:List[(String,List[TVar])])

object TreoLite {
  /**
    * add In/Out fields to a connector, by trying to infer its type.
    * @param c Treo connector to be extended with types to its arguments
    * @param inferPrim function that infers the type of te primitive name
    * @return updated Treo connector with typed arguments
    */
  def inferTypes(c:TConnAST, inferPrim:String=>Connector, scope:TreoASTs, split:String): TConn = {
    def inferPrim2(s:String): Connector = {
      //println(s"inferring type of $s in scope ${scope}")
      val res = scope.get(s) match {
        case Some((in, out)) => Prim(s, Port(IVal(in)), Port(IVal(out)))
        case _ => inferPrim(s)
      }
      //println(s" got $res")
      res
    }
    //println("getting preoConn")
    val preoConn1: Connector = c.name.fold(inferPrim2,(x:Connector)=>x) // either(inferPr,id) c.name
    //println(s"got1: ${Show(preoConn1)}")
    val (preoConn,_) = expandTreoAST(preoConn1,scope,inferPrim,split)
    //println(s"got2: ${Show(preoConn)}")
    // TODO: changing here
    preo.DSL.unsafeTypeOf(preoConn) match {
      case (Type(args, Port(IVal(i)),Port(IVal(j)),BVal(true),_),BVal(true)) if args.vars.isEmpty =>
        if (c.args.size != i+j)
//          fail(s"${c.name} has ${c.args.size} arguments, but found ${i+j}.")
          throw new TypeCheckException(s"${c.getName} has ${c.args.size} arguments, but expected ${i+j}.")
        else
          TConn(Eval.unsafeReduce(preoConn), // must work with given type
                c.args.zipWithIndex.map(pair => TVar(pair._1,pair._2 < i)))
      case _ =>
//        fail(s"Not a concrete primitive: ${Show(preoConn)}")
        throw new TypeCheckException(s"Not a concrete primitive: ${Show(preoConn)}")
    }
  }

  private def inferTypes(c:TreoLiteAST, inferPrim:String=>Connector,
                         scope:TreoASTs, split:String): TreoLite =
    TreoLite(c.args,c.conns.map(inferTypes(_,inferPrim,scope,split)))

  /**
    * Unfolds a TreoLiteConn (also a CoreConnector) into a CoreConnector without TreoLiteConn
    * @param c CoreConnector with possible treoLiteConns
    * @param split Split operator - dupl or xor usually
    * @return CoreConnector with no TreoLiteConns
    */
  def unfoldTreoLiteConn(c:CoreConnector, split: String): CoreConnector = c match {
    case CTreo(treo) =>
      def insOuts(as:List[TVar]) = as.partition(_.isIn)
      val treolite = TreoLite(treo.args,treo.conns.map(c => {
        val (in,out) = insOuts(c._2)
        TConn(CPrim(c._1, CoreInterface(in.size), CoreInterface(out.size)), c._2)
      }))
      unfoldTreoLiteConn(treo2preo(treolite, split),split)

    case CSeq(c1, c2) => CSeq(unfoldTreoLiteConn(c1,split),unfoldTreoLiteConn(c2,split))
    case CPar(c1, c2) => CPar(unfoldTreoLiteConn(c1,split),unfoldTreoLiteConn(c2,split))
    case CId(i) => c
    case CSymmetry(i, j) => c
    case CTrace(i, c2) => CTrace(i,unfoldTreoLiteConn(c2,split))
    case CPrim(name, i, j, extra) => c
    case CSubConnector(name, c2, ann) => CSubConnector(name,unfoldTreoLiteConn(c2,split),ann)
  }




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

  def expandTreoAST(conn:Connector,scope:TreoASTs, infer: String=>Connector,
                    split: String): (Connector,TreoASTs) = {
    //println(s"expanding ${Show(conn)}")
    conn match {
      case Prim(name, i, j, trs) => trs.toList match {
        case List(tr:TreoLiteAST) =>
          //println(s"found treoAST $name")
          val (ins,outs) = tr.args.partition(_.isIn)
          val toScope = name -> (ins.size,outs.size)
          //println(s" - new scope = ${scope+toScope}")
          val treo = inferTypes(tr, infer, scope, split) // get TreoLite
          //println(s" - new treo: ${treo}")
          expandTreoAST(treo2preo(treo, split).toConnector,
            scope+toScope, infer, split)

        case _ =>
          //println(s"found another prim: ${Show(conn)}")
          (conn,scope)
      }

      case Seq(c1, c2) =>
        val nc1 = expandTreoAST(c1,scope,infer,split)
        val nc2 = expandTreoAST(c2,nc1._2,infer,split)
        (Seq(nc1._1,nc2._1),nc2._2)
      case Par(c1, c2) =>
        val nc1 = expandTreoAST(c1,scope,infer,split)
        val nc2 = expandTreoAST(c2,nc1._2,infer,split)
        (Par(nc1._1,nc2._1),nc2._2)
      case Choice(b, c1, c2) =>
        val nc1 = expandTreoAST(c1,scope,infer,split)
        val nc2 = expandTreoAST(c2,nc1._2,infer,split)
        (Choice(b,nc1._1,nc2._1),nc2._2)
      case Exp(a, c) =>
        val nc = expandTreoAST(c,scope,infer,split)
        (Exp(a,nc._1),nc._2)
      case ExpX(x, a, c) =>
        val nc = expandTreoAST(c,scope,infer,split)
        (ExpX(x,a,nc._1),nc._2)
      case Abs(x, et, c) =>
        val nc = expandTreoAST(c,scope,infer,split)
        (Abs(x,et,nc._1),nc._2)
      case App(c, a) =>
        val nc = expandTreoAST(c,scope,infer,split)
        (App(nc._1,a),nc._2)
      case Restr(c, phi) =>
        val nc = expandTreoAST(c,scope,infer,split)
        (Restr(nc._1,phi),nc._2)
      case Trace(i, c) =>
        val nc = expandTreoAST(c,scope,infer,split)
        (Trace(i,nc._1),nc._2)

      case SubConnector(name, c1, annotations) =>
        val nc = expandTreoAST(c1,scope,infer,split)
        (SubConnector(name,nc._1,annotations),nc._2)
      case _ => (conn,scope)
    }
  }




//  def collectTreoAST(conn:Connector,past:List[String]): TreoASTsOld =
//    conn match {
//      case Prim(name, i, j, trs) => trs.toList match {
//        case List(tr:TreoLiteAST) =>
//          val (ins,outs) = tr.args.partition(_.isIn)
//          Map((name::past) -> (ins.size,outs.size))
//        case _ => Map()
//      }
//
//      case SubConnector(name, c1, annotations) =>
//        collectTreoAST(c1,name::past)
//
//      case Id(i) => Map()
//      case Symmetry(i, j) => Map()
//      case Seq(c1, c2) => collectTreoAST(c1,past)++collectTreoAST(c2,past)
//      case Par(c1, c2) => collectTreoAST(c1,past)++collectTreoAST(c2,past)
//      case Trace(i, c) => collectTreoAST(c,past)
//      case Exp(a, c) => collectTreoAST(c,past)
//      case ExpX(x, a, c) => collectTreoAST(c,past)
//      case Choice(b, c1, c2) => collectTreoAST(c1,past)++collectTreoAST(c2,past)
//      case Abs(x, et, c) => collectTreoAST(c,past)
//      case App(c, a) => collectTreoAST(c,past)
//      case Restr(c, phi) => collectTreoAST(c,past)
//    }


  type TreoASTs = Map[String,(Int,Int)]
//  type TreoASTsOld = Map[List[String],(Int,Int)]

  def treoASTToTreo(conn: Connector, infer: String=>Connector, split: String): Connector = {
    //treoASTToTreo(conn,collectTreoAST(conn,Nil),infer,split,Nil)
    expandTreoAST(conn,Map(),infer,split)._1
  }


//  def treoASTToTreo(conn: Connector, treos:TreoASTsOld,
//                    infer: String=>Connector, split: String, past:List[String]): Connector =
//    conn match {
//      case Prim(name, i, j, trs) => trs.toList match {
//        case List(tr: TreoLiteAST) =>
//          val treo = inferTypes(tr, treos, infer, past)
//          treo2preo(treo, split).toConnector
//        case _ => conn
//      }
//      case Seq(c1, c2) => Seq(treoASTToTreo(c1, infer, split), treoASTToTreo(c2, infer, split))
//      case Par(c1, c2) => Par(treoASTToTreo(c1, infer, split), treoASTToTreo(c2, infer, split))
//      case Trace(i, c) => Trace(i, treoASTToTreo(c, infer, split))
//
//      case SubConnector(name, c1, anns) => SubConnector(name, treoASTToTreo(c1, infer, split), anns)
//
//      case Exp(a, c) => Exp(a, treoASTToTreo(c, infer, split))
//      case ExpX(x, a, c) => ExpX(x, a, treoASTToTreo(c, infer, split))
//      case Choice(b, c1, c2) => Choice(b, treoASTToTreo(c1, infer, split), treoASTToTreo(c2, infer, split))
//      case Abs(x, et, c) => Abs(x, et, treoASTToTreo(c, infer, split))
//      case App(c, a) => App(treoASTToTreo(c, infer, split), a)
//
//      case Restr(c, phi) => Restr(treoASTToTreo(c, infer, split), phi)
//
//      case _ => conn
//    }



}

