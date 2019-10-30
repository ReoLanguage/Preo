package preo.frontend

import preo.DSL.{Tr, event, eventFull, id, swap}
import preo.ast.{Connector, SubConnector}
import preo.DSL._


/**
  * Helper to create virtuoso tasks.
  *
  * A task is a list of input/output ports with modes: W/NW/TO
  * The list specifies the order in which ports are enabled
  *
  * Created by guillecledou on 2019-10-30
  */

object Task {

  
  def apply(ports:List[SubConnector]):Connector = {

    var ins = ports.filter(p => p.name.startsWith("get")).size
    var outs = ports.filter(p => p.name.startsWith("put")).size

    var currentIns = ins
    var currentOuts = 0

    var task:Connector = mkStep(eventFull,ins,0)

    var iterator = ports.iterator

    while (iterator.hasNext) {

      val con = iterator.next()

      if (con.name.startsWith("put")) {
        if (iterator.hasNext)
          task = task & mkStep(con & (event * id),currentIns,currentOuts)
        else
          task = task & mkStep(con,currentIns,currentOuts)
        currentOuts+=1
      } else {
        if (iterator.hasNext)
          task = task & mkStep(con & event,currentIns-1,currentOuts)
        else
          task = task & mkStep(con,currentIns-1,currentOuts)
        currentIns-=1
      }

    }

    // swap next from top to bottom as many times as outs are
    for(i <- 1 to outs) {
      task = task & mkStep(swap,i-1,outs-i)
    }

    val res = Tr(1,task)
    println("TaskConnector:\n"+res)
    res

  }

  // it would be usefull to have a null connector to avoid this, s.t. conn */& null = conn
  private def mkStep(conn:Connector,ins:Int,outs:Int):(Connector) = (ins>0,outs>0) match {
    case (true,true)  => ids(ins) * conn * ids(outs)
    case (true,false) => ids(ins) * conn
    case (false,true) => conn * ids(ins)
    case _ => conn
  }

  /** makes n ids in parallel. assumes n>0 */
  private def ids(n:Int):Connector = (1 to (n-1)).map(_=>id).foldRight[Connector](id)(_*_)

}
