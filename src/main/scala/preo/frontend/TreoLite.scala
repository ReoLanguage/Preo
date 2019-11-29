package preo.frontend

import preo.DSL
import preo.ast._
import preo.common.{GenerationException, TypeCheckException}

// TreoLite maps [IO arguments] to [CoreConnectors with IO arguments]
//case class TreoLite(args:List[TVar],conns:List[TConn])
case class TVar(name:String,isIn:Boolean) {
  def isOut: Boolean = !isIn
  override def toString: String = name+(if(isIn)"?" else "!")
}
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

// New notion of TreoLite
case class TreoLiteConn(args:List[TVar], conns:List[TreoLite.TConnUse]) {
  def map(f:Connector=>Connector) =
    TreoLiteConn(args, conns.map(kv =>(kv._1.map(c=>f(c)),kv._2)))
}
case class TreoLiteCConn(args:List[TVar], conns:List[TreoLite.TCConnUse])

object TreoLite {

  type TConnDef = Either[String,Connector]
  type TConnUse = (TConnDef,List[TVar])

  type TCConnDef = Either[String,CoreConnector]
  type TCConnUse = (TCConnDef,List[TVar])

  type TreoASTs = Map[String,List[TVar]]
  //  type TreoASTsOld = Map[List[String],(Int,Int)]


  ////////////////////////////////////////////////
  /// 1st part: replacing TreoLiteAST        /////
  //        by Connectors with TreoLiteConn  /////
  ////////////////////////////////////////////////


  /**
    * Finds special primitives with a [[TreoLiteAST]] extra argument, and
    * 1. transform them into a TreoLiteConn
    * 2. update all of its usages in scope by a TreoLiteConn
    * @param conn input connector with TreoLiteAST occurrencs
    * @param infer helper to convert known primitives (in the prelude)
    * @return new connector with no TreoLiteAST references
    */
  def treoASTToTreo(conn: Connector, infer: String=>Connector): Connector = {
//    println("Connector, before applying treoAST2Treo:\n"+conn)
    val res = expandTreoAST(conn,Map(),infer)._1
//    println("Connector, after applying treoAST2Treo:\n"+res)
    res
  }

  private def expandTreoAST(conn:Connector,scope:TreoASTs, infer: String=>Connector): (Connector,TreoASTs) = {
    // println(s"expanding ${Show(conn)}: ${conn.getClass}")
    val res = conn match {
      case Prim(name, i, j, trs) => trs.toList match {
        case List(tr:TreoLiteAST) =>
//          println(s"found treoAST $name")
          val toScope = name -> tr.args
//          println(s" - new scope = ${scope+toScope}")
          val treo = inferTArgs(tr, infer, scope) // get TreoLiteConn
//          println(s" - new treo: ${treo}")
          expandTreoAST(Treo(treo),scope+toScope,infer)
        //          (Treo(treo),scope+toScope)

        case _ =>
          //println(s"found another prim: ${Show(conn)}")
          (conn,scope)
      }

      case Treo(treo:TreoLiteConn) =>
        val t2 = treo.map(expandTreoAST(_,scope,infer)._1)
        (Treo(t2),scope)

      case Seq(c1, c2) =>
        val nc1 = expandTreoAST(c1,scope,infer)
        val nc2 = expandTreoAST(c2,nc1._2,infer)
        (Seq(nc1._1,nc2._1),nc2._2)
      case Par(c1, c2) =>
        val nc1 = expandTreoAST(c1,scope,infer)
        val nc2 = expandTreoAST(c2,nc1._2,infer)
        (Par(nc1._1,nc2._1),nc2._2)
      case Choice(b, c1, c2) =>
        val nc1 = expandTreoAST(c1,scope,infer)
        val nc2 = expandTreoAST(c2,nc1._2,infer)
        (Choice(b,nc1._1,nc2._1),nc2._2)
      case Exp(a, c) =>
        val nc = expandTreoAST(c,scope,infer)
        (Exp(a,nc._1),nc._2)
      case ExpX(x, a, c) =>
        val nc = expandTreoAST(c,scope,infer)
        (ExpX(x,a,nc._1),nc._2)
      case Abs(x, et, c) =>
        val nc = expandTreoAST(c,scope,infer)
        (Abs(x,et,nc._1),nc._2)
      case App(c, a) =>
        val nc = expandTreoAST(c,scope,infer)
        (App(nc._1,a),nc._2)
      case Restr(c, phi) =>
        val nc = expandTreoAST(c,scope,infer)
        (Restr(nc._1,phi),nc._2)
      case Trace(i, c) =>
        val nc = expandTreoAST(c,scope,infer)
        (Trace(i,nc._1),nc._2)

      case SubConnector(name, c1, annotations) =>
        val nc = expandTreoAST(c1,scope,infer)
        (SubConnector(name,nc._1,annotations),nc._2)
      case _ => (conn,scope)
    }
    //println(s"expanded - ${Show(res._1)} / ${res._2}")
    res
  }


  /**
    * add In/Out fields to a connector, by trying to infer its type.
    * @param c Treo connector to be extended with types to its arguments
    * @param inferPrim function that infers the type of te primitive name
    * @return updated Treo connector with typed arguments
    */
  private def inferTArgs(c:TreoLiteAST, inferPrim:String=>Connector,
                         scope:TreoASTs): TreoLiteConn =
    TreoLiteConn(c.args,c.conns.map(inferTArgs(_,inferPrim,scope)))


  private def inferTArgs(c:TConnAST, inferPrim:String=>Connector,
                 scope:TreoASTs): TConnUse = {

    def updNames(vars: List[TVar]): List[TVar] =
      if (vars.size == c.args.size)
        c.args.zip(vars).map(kv => TVar(kv._1, kv._2.isIn))
      else throw new TypeCheckException(
        s"${c.getName} has ${c.args.size} arguments, but expected ${c.args.size}.")

    def getFromTask(conn: Connector): Option[List[TaskPort]] = {
      def toTaskPort(l: List[_]): List[TaskPort] = l match {
        case (a: TaskPort) :: rest => a :: (toTaskPort(rest))
        case _::rest => toTaskPort(rest)
        case _ => Nil
      }

      conn match {
        case SubConnector(_, Prim(_, _, _, extra), _) =>
          for (e <- extra) e match {
            case hd :: tl => hd match {
              // needed, otherwise type would be removed by erasure
              case hd: TaskPort =>
                return Some(hd :: toTaskPort(tl))
              case _ =>
            }
            case _ =>
          }
          None
        case _ => None
      }
    }

    def inferFromConn(conn: Connector): List[TVar] = {
      def toTVar(p:TaskPort) = TVar(p.name.getOrElse(""),p.isInput)

      getFromTask(conn) match {
        // conn is a task - it has the port information inside "extra"
        case Some(ports) =>
          ports.map(toTVar)
        // conn is some primitive: assume leftmost are input, rightmost are output.
        case _ =>
          preo.DSL.unsafeTypeOf(expandTreoAST(conn, scope, inferPrim)._1) match {
            case m@(Type(args, Port(IVal(i)), Port(IVal(j)), BVal(true), _), BVal(true))
              if args.vars.isEmpty =>
              /* to track all port names from treo to the Connector ------> */
              //            conn match {
              //              case SubConnector(n,c1,ann) if ann.exists(e=> e.name == "task") =>
              //              println("SubConnector type"+m._1)
              //              mkPortNames(conn,c.args)
              //              case _ => updNames(c.args.zipWithIndex.map(pair => TVar(pair._1, pair._2 < i)))
              //            }
              /* -----> to track all port names from treo to the Connector */
              updNames(c.args.zipWithIndex.map(pair => TVar(pair._1, pair._2 < i)))
            case _ =>
              throw new TypeCheckException(s"Only concrete primitives can be in a Treo block. " +
                s"But non concrete primitive found: ${Show(conn)}")
          }
      }
    }

    c.name match {
      case Left(name) => scope.get(name) match {
        case Some(list) => c.name -> updNames(list)
        case _ =>
          val c2 = inferPrim(name)
          Right(c2) -> inferFromConn(c2)
      }
      case Right(conn) => c.name -> inferFromConn(conn)
    }
  }

//  /**
//    * Given a connector that is actually a SubConnector with a task,
//    * and given a list of arguments names to that connector/task
//    * Creates a list of TVar, this is necesary because the order is not necesarally inputs then outputs
//    * they can be mixed.
//    * @param c
//    * @param names
//    * @return
//    */
//  private def mkPortNames(c:Connector,names:List[String]):List[TVar] = {
//    c match {
//      case SubConnector(name,c1,ann)=> mkPortNames(c1,names)
//      case Prim(name,i,j,extra) =>
//        val (ins,outs) = getInterfacePorts(c)
//        val res = names.take(ins).map(n=>TVar(n,true))++names.drop(ins).take(outs).map(n=> TVar(n,false))
//        res
//      case Par(c1,c2) =>
//        val (i1,o1) = getInterfacePorts(c1)
//        mkPortNames(c1,names.take(i1+o1))++mkPortNames(c2,names.drop(i1+o1))
//      case Seq(c1,c2) =>
//        val (i1,o1) = getInterfacePorts(c1)
//        mkPortNames(c1,names.take(i1))++mkPortNames(c2,names.drop(i1))
//      case Trace(i,c2) => mkPortNames(c2,names)
//      case conn => List()
//    }
//  }

//  /**
//    * Given a connector of any type, it returns the number of inputs and ouputs
//    * @param c
//    * @return
//    */
//  private def getInterfacePorts(c:Connector):(Int,Int) = c match {
//    case SubConnector(name,c1,ann) => getInterfacePorts(c1)
//    case Prim(n,i,j,e)=> preo.DSL.unsafeTypeOf(c) match {
//      case (Type(args, Port(IVal(i)), Port(IVal(j)), BVal(true), _),BVal(true))
//        if args.vars.isEmpty => (i,j)
//      case _=> throw new TypeCheckException(s"Only concrete primitives can be in a Treo block. " +
//        s"But non concrete primitive found: ${Show(c)}")
//    }
//    case Par(c1,c2) =>
//      val (i1,o1) = getInterfacePorts(c1)
//      val (i2,o2) = getInterfacePorts(c2)
//      (i1+i2,o1+o2)
//    case Seq(c1,c2)=>
//      val (i1,o1) = getInterfacePorts(c1)
//      val (i2,o2) = getInterfacePorts(c2)
//      (i1,o2)
//    case Id(i) => preo.DSL.unsafeTypeOf(c) match {
//      case (Type(args, Port(IVal(i)), Port(IVal(j)), BVal(true), _),BVal(true))
//        if args.vars.isEmpty => (i,j)
//      case _=> throw new TypeCheckException(s"Only concrete primitives can be in a Treo block. " +
//        s"But non concrete primitive found: ${Show(c)}")
//    }
//    case Symmetry(i,j) => preo.DSL.unsafeTypeOf(c) match {
//      case (Type(args, Port(IVal(i1)), Port(IVal(j1)), BVal(true), _),BVal(true))
//        if args.vars.isEmpty => (i1,j1)
//      case _=> throw new TypeCheckException(s"Only concrete primitives can be in a Treo block. " +
//        s"But non concrete primitive found: ${Show(c)}")
//    }
//    case Trace(i,c2) => getInterfacePorts(c2)
//  }

  /////////////////////////////////////////////
  /// 2nd part: replacing TreoLiteCConn   /////
  //        by plain Core Connectors      /////
  /////////////////////////////////////////////


  /**
    * Unfolds a TreoLiteConn (also a CoreConnector) into a CoreConnector without TreoLiteConn
    * @param c CoreConnector with possible treoLiteConns
    * @param split Split operator - dupl or xor usually
    * @return CoreConnector with no TreoLiteConns
    */
  def treo2preo(c:CoreConnector, split: String): CoreConnector = c match {
    case CTreo(treo) =>
//      println("connectore before removing treoliteconn:\n"+CTreo(treo))
      val res = treo2preo(treo2preo(treo, split),split)
//      println("connectore after removing treoliteconn:\n"+res)
      res
    case CSeq(c1, c2) => CSeq(treo2preo(c1,split),treo2preo(c2,split))
    case CPar(c1, c2) => CPar(treo2preo(c1,split),treo2preo(c2,split))
    case CId(i) => c
    case CSymmetry(i, j) => c
    case CTrace(i, c2) => CTrace(i,treo2preo(c2,split))
    case CPrim(name, i, j, extra) => c
    case CSubConnector(name, c2, ann) => CSubConnector(name,treo2preo(c2,split),ann)
  }


//  /**
//    * Given a core connector it returns the number of inputs and outputs
//    * @param c
//    * @return
//    */
//  def getInterfacePorts(c:CoreConnector):(Int,Int) = c match {
//    case CSubConnector(name,c1,ann) => getInterfacePorts(c1)
//    case CPrim(n,i,j,e)=> (i.ports,j.ports)
//    case CPar(c1,c2) =>
//      val (i1,o1) = getInterfacePorts(c1)
//      val (i2,o2) = getInterfacePorts(c2)
//      (i1+i2,o1+o2)
//    case CSeq(c1,c2)=>
//      val (i1,o1) = getInterfacePorts(c1)
//      val (i2,o2) = getInterfacePorts(c2)
//      (i1,o2)
//    case CId(i) => (i.ports,0) //?
//    case CSymmetry(i,j) => (i.ports,j.ports)
//    case CTrace(i,c2) =>
//      val (i2,o2) = getInterfacePorts(c2)
//      (i.ports,o2) // ??
//  }
//
//  /**
//    * Given a core connector and a list of inputs and outputs tvars, it
//    * returns the same core connector where each prim stores the name information
//    * of its corresponding parameters.
//    * To be used if it is desired to keep track of all ports names
//    * @param c
//    * @param names
//    * @return
//    */
//  def mkPortNames(c:CoreConnector,names:(List[TVar],List[TVar])):CoreConnector = {
////    println("ins:"+names._1)
////    println("outs:"+names._2)
//    c match {
//      case CSubConnector(name,c,ann)=>CSubConnector(name,mkPortNames(c,names),ann)
//      case CPrim(name,i,j,extra) =>
////        println("bfore naming:"+c)
//        val res = CPrim(name,i,j,extra++Set(names._1.take(i.ports)++names._2.take(j.ports)))
////        println("after naming:"+res)
//        res
//      case CPar(c1,c2) =>
//        val (i1,o1) = getInterfacePorts(c1)
//        CPar(mkPortNames(c1,(names._1.take(i1),names._2.take(o1))),mkPortNames(c2,(names._1.drop(i1),names._2.drop(o1))))
//      case CSeq(c1,c2) =>
//        val (i1,o1) = getInterfacePorts(c1)
//        CSeq(mkPortNames(c1,(names._1.take(i1),names._2)),mkPortNames(c2,(names._1.drop(i1),names._2)))
//      case CTrace(i,c2) => CTrace(i,mkPortNames(c2,names))
//      case conn => conn
//    }
//  }

  /**
    * Converts a [[TreoLiteCConn]] into a pure [[CoreConnector]].
    * @param t TreoLite to be converted
    * @param split dupl or xor connector - how to transform a merge into a splitting connector
    * @return Resulting CoreConnector
    */
  def treo2preo(t:TreoLiteCConn,split:String): CoreConnector = {
    // aux function
    def mkPar(c1:CoreConnector, c2:CoreConnector) = c1*c2
    def mkSeq(c1:CoreConnector, c2:CoreConnector) = c1&c2
    def id(i:Int) = CId(CoreInterface(i))
    //
    val args = t.args
    def getA(x:TCConnUse) = x._1
    def getV(x:TCConnUse) = x._2
    def getCC(x:TCConnUse) = x._1 match {
      case Left(name) =>
        val (ins,outs) = x._2.partition(_.isIn)
//        mkPortNames(CPrim(name,CoreInterface(ins.size),CoreInterface(outs.size)),(ins,outs))
        CPrim(name,CoreInterface(ins.size),CoreInterface(outs.size))
      case Right(conn) => conn //mkPortNames(conn,x._2.partition(_.isIn))
    }
    // 1. build core in parallel
    val core = t.conns
                .map(getCC)
                .reduceOption(mkPar)
                .getOrElse(id(0))
    // 2. collect inputs and outputs
    val (insV,outsV) = t.conns.flatMap(getV).partition(_.isIn)
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




}

