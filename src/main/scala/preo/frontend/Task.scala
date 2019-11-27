package preo.frontend

import preo.DSL
import preo.DSL.{Tr, event, eventFull, id, swap}
import preo.ast._
import preo.DSL._
import preo.common.GenerationException


/**
  * Helper to create virtuoso tasks.
  *
  * A task is a list of input/output ports with modes: W/NW/TO
  * The list specifies the order in which ports are enabled
  *
  * Created by guillecledou on 2019-10-30
  */

object Task {

  /**
    * Given a list of task's ports connectors, organize them so that they execute in sequence
    * Its a wrapper for 'mkSequentialPorts' and 'toSingleConnector'
    *
    * Depending if its a single port or more uses different methods
    *
    * @param ports list of task's ports
    * @return a reo connector that represents a task
    */
  def apply(ports: List[TaskPort], periodicity: Option[Int] = None): Connector = {
//    if (periodicityOk(ports,periodicity))
      ports match {
        case p :: Nil => toSingleConnector(p, periodicity)
        case _ => mkSequentialPorts(ports, periodicity)
      }
//    else throw new GenerationException("Unsupported periodicity for ports: "+ ports.map(_.toString))
  }
  /**
    * Check if the lists of ports is correct for a given periodicity (if any)
    * Ok if: none W and sum of TO <= periodicity
    * @param ports
    * @param periodicity
    * @return
    */
  private def periodicityOk(ports:List[TaskPort],periodicity:Option[Int]):Boolean = periodicity match {
    case Some(p) => !(ports.exists(_.isWait) || ports.map(port => port.timeout).sum > p)
    case None => true
  }

  /**
    * Given a list of task's ports connectors, organize them so that they execute in sequence
    *
    * @param ports
    * @return
    */
  private def mkSequentialPorts(ports:List[TaskPort],periodicity:Option[Int]=None):Connector = {
    // number of inputs to have from the beginning
    var ins = ports.count(p => p.isInput)
    // number of final output ports
    var outs = ports.count(p => !p.isInput)

    // current inputs we need to pass through steps (each step is separated by a ;)
    var currentIns = ins
    // current outputs
    var currentOuts = 0

    // accumulator for final task, initially has an eventFull and as many ids in parallel as ins (and 0 outputs)
    // the event full determines which task port can go first
    // if periodicity is defined it adds periodic synchronizer at the beggining which will force periodicity to reset when the period pass
    var task:Connector = mkStep(if(periodicity.isDefined) periodic(periodicity.get) & eventFull  else eventFull,ins,0)

    // iterator over ports of the task
    var iterator = ports.iterator

    // mk the first port since it doesn't need special connectors before trying to do that port action
    val con = iterator.next()
    if (con.isOutput) { // if it is a put request (put request are of type (1 -> 2): (go -> next,write)
      // mk a step keeping the same number of inputs in parallel and current outs,
      task = task & mkStep(toSeqConnector(con),currentIns,currentOuts)
      // a put increments in 1 the number of outputs
      currentOuts+=1
    } else { // if it is a get request (get request are of type (2 -> 1): (read,go -> next))
      // mk a step consuming one input and keeping the same outputs,
      task = task & mkStep(toSeqConnector(con),currentIns-1,currentOuts)
      // a get decrements in 1 the number of inputs
      currentIns-=1
    }

    // while there are more ports
    while (iterator.hasNext) {
      // get the next port
      val con = iterator.next()
      // if it is a put request (put request are of type (1 -> 2): (go -> next,write)
      if (con.isOutput) {
        if (iterator.hasNext) // if this is not the last port
          // mk a step keeping the same number of inputs in parallel and current outs
          // if it is a  wait it adds a event before go so that the previous port and this one do not synchronize at the same time
          task = task & mkStep( (if(con.isWait) event else id) & toSeqConnector(con),currentIns,currentOuts)
        else // if it is the last port
          // if it is a  wait it adds a event before go so that the previous port and this one do not synchronize at the same time
          // if it is periodic it adds an event after next so that next does not have a guard on the same transition the action fires
          task = task & mkStep( (if(con.isWait) event else id) & toSeqConnector(con) & ((if(periodicity.isDefined) event else id) * id),currentIns,currentOuts)
        // a put increments in 1 the number of outputs
        currentOuts+=1
      } else { // if it is a get request (get request are of type (2 -> 1): (read,go -> next))
        if (iterator.hasNext) // if this is not the last port
          // mk a step consuming one input and keeping the same outputs
          // if it is a wait it adds an event before go so that the previous port and this one do not synchronize at the same time
          task = task & mkStep((id * (if(con.isWait) event else id)) & toSeqConnector(con),currentIns-1,currentOuts)
        else // if it is the last port
        // mk a step consuming one input and keeping the same outputs
        // if it is a wait it adds an event before go so that the previous port and this one do not synchronize at the same time
        // if it is periodic it adds an event after next so that next does not have a guard on the same transition the action fires
          task = task & mkStep((id * (if(con.isWait) event else id)) & toSeqConnector(con) & (if(periodicity.isDefined) event else id),currentIns-1,currentOuts)
        // a get decrements in 1 the number of inputs
        currentIns-=1
      }
    }

    // swap next from top to bottom as many times as outs are
    for(i <- 1 to outs) {
      task = task & mkStep(swap,i-1,outs-i)
    }

    // do a loop from last next to the eventFull input
    Tr(1,task)
  }

  /**
    *
    * Helper to make a step of a task connector, each step is separated by sequential composition ;
    * The step preserve inputs and outputs from previous step,
    * so that they are carried to next steps.
    * The step is build following an order: inputs, new port connector for this step, outputs
    *
    * @param conn connector
    * @param ins number of inputs forward
    * @param outs number of outputs to forward
    * @return a new connector representing the current step
    */
  private def mkStep(conn:Connector,ins:Int,outs:Int):Connector = (ins>0,outs>0) match {
    case (true,true)  => ids(ins) * conn * ids(outs)
    case (true,false) => ids(ins) * conn
    case (false,true) => conn * ids(outs)
    case _ => conn
  }

  /** Helper to make n ids in parallel - assumes n>0 */
  private def ids(n:Int):Connector = (1 until n).map(_=>id).foldRight[Connector](id)(_*_)

  /* - Helpers for building virtuoso tasks - */

  /** single writer port */
  private val writer = (n:Option[String],v:Option[IVal],to:Int,t:String,keep:String,p:Option[Int]) =>
    Prim("writer",Port(IVal(0)),Port(IVal(1)),
      Set("component",keep,if (v.isDefined) "writes:"+v.get.n else "",oname(n,to,t),addPeriod(p)))

  /** single reader port */
  private val reader = (n:Option[String],to:Int,t:String,p:Option[Int]) =>
    Prim("reader",Port(IVal(1)),Port(IVal(0)), Set("component","T",iname(n,to,t),addPeriod(p)))


  /*
   * - Connectors for ports when they need to be sequenced inside a task -
   *
   *   put request in a sequece have type 1 -> 2 (go -> next, out)
   *   get request in a sequece have type 2 -> 1 (go,read -> next)
   */

  /** NW put request */
  private val nwput = (n:Option[String],v:Option[IVal],p:Option[Int]) =>
    (id * writer(None,v,0,"NW","",None) ) & ( dupl * dupl) & (id * drain * putNB(n,0,"NW",p)) & (event * id * dupl) & (dupl * merger * id) & (id * drain * id)

  /** TO put request */
  private val toput = (n:Option[String],to:Int,v:Option[IVal],p:Option[Int]) =>
    (id * writer(None,v,to,"TO","",None) ) & ( dupl * dupl) & (id * drain * putNB(n,to,"TO",p)) & (event * id * dupl) & (dupl * merger * id) & (id * drain * id)

  /** W put request */
  private val wput = (n:Option[String],v:Option[IVal],p:Option[Int]) => (id * writer(n,v,0,"W","T",p)) & ( dupl * dupl) & (id * drain * id)

  /* W get request */
  private val wget = (n:Option[String],p:Option[Int]) => (dupl * dupl) & (reader(n,0,"W",p) * drain * id)

  /* NW get request */
  private val nwget = (n:Option[String],p:Option[Int]) => getNB(n,0,"NW",p)

  /* TO get request */
  private val toget = (n:Option[String],to:Int,p:Option[Int]) => getNB(n,to,"TO",p)

  /** helper to build non-blocking put request (NW/TO) */
  private val putNB = (n:Option[String],to:Int,t:String,p:Option[Int]) => Prim("nbput",Port(IVal(1)),Port(IVal(2)), Set("T","to:"+to,oname(n,to,t),addPeriod(p))) // 1->2 (in->ok*err)

  /** helper to build non-blocking get request (NW/TO) */
  private val getNB = (n:Option[String],to:Int,t:String,p:Option[Int]) =>
    Prim("nbget",Port(IVal(2)),Port(IVal(1)),
      Set("component","T",iname(n,to,t),"to:"+to,addPeriod(p))) //& (id * reader(n,t))

  private def iname(n:Option[String],to:Int,t:String) =
    if(n.isDefined) s"portName:${if (t == "TO") to else t} "+n.get+"?" else ""

  private def oname(n:Option[String],to:Int,t:String) =
    if(n.isDefined) s"portName:${if (t == "TO") to  else t} "+n.get+"!" else ""
  /*
  * - Connectors for ports when they are unique or -
  *
  *   put request in a sequece have type 0 -> 1 ( . -> write )
  *   get request in a sequece have type 1 -> 0 (read -> .)
  *
  */

  /** single NW put request */
  private val singleNWput = (n:Option[String],v:Option[IVal],p:Option[Int]) =>
    writer(n,v,0,"NW","",None) & Prim("nbtimer",1,1,Set("to:"+0,addPeriod(p)))

  /** single W put request */
  private val singleWput = (n:Option[String],v:Option[IVal],p:Option[Int]) => writer(n,v,0,"W","T",p)

  /** single TO put request */
  private val singleTOput = (n:Option[String],v:Option[IVal],to:Int,p:Option[Int]) =>
    writer(n,v,to,"TO","",None) & Prim("nbtimer",1,1,Set("T","to:"+to,addPeriod(p),oname(n,to,"TO")))

  /** single NW get request */
  private def singleNWget(n:Option[String],p:Option[Int]):Connector =
    Prim("nbreader", Port(IVal(1)), Port(IVal(0)), Set(addPeriod(p),"component", "T",  "to:" + 0, iname(n, 0, "NW")))


  /** single W get request */
  private val singleWget = (n:Option[String],p:Option[Int]) => reader(n,0,"W",p)

  /** single TO get request */
  private val singleTOget = (n:Option[String],to:Int,p:Option[Int]) =>
    Prim("nbreader",Port(IVal(1)),Port(IVal(0)),Set(addPeriod(p),"component","T","to:"+to,iname(n,to,"TO")))

  // NEW DEFINITIONS SINGLE PORT
  private def onlyGetTO(n:Option[String],to:Int,p:Option[Int]): Connector =
    Prim("nbget",Port(IVal(1)),Port(IVal(0)),Set(addPeriod(p),"component","T","to:"+to,iname(n,to,"TO")))
  private def onlyGetNW(n:Option[String],p:Option[Int]): Connector =
    Prim("nbget",Port(IVal(1)),Port(IVal(0)),Set(addPeriod(p),"component","T","to:"+0,iname(n,0,"TO")))
  private def onlyGetW(n:Option[String],p:Option[Int]): Connector =
    reader(n,0,"W",p)
  private def onlyPutTO(n:Option[String],v:Option[IVal],to:Int,p:Option[Int]):Connector =
    Prim("nbput",0,1,Set("T","to:"+to,addPeriod(p),oname(n,to,"TO")))
  private def onlyPutNW(n:Option[String],v:Option[IVal],p:Option[Int]):Connector =
    Prim("nbput",0,1,Set("T","to:"+0,addPeriod(p),oname(n,0,"TO")))
  private def onlyPutW(n:Option[String],v:Option[IVal],p:Option[Int]): Connector =
    writer(n,v,0,"W","T",p)


  // NEW DEFINITIONS WHEN SEQUENCE OF PORTS
//  private def seqPutNW(n:Option[String],v:Option[IVal]):Connector =
//    (id * writer(None,v,0,"NW","",None) ) & ( dupl * dupl) & (id * drain * putNB(n,0,"NW")) & (event * id * dupl) & (dupl * merger * id) & (id * drain * id)

  /** TO put request */
//  private val toput = (n:Option[String],to:Int,v:Option[IVal]) =>
//    (id * writer(None,v,to,"TO","",None) ) & ( dupl * dupl) & (id * drain * putNB(n,to,"TO")) & (event * id * dupl) & (dupl * merger * id) & (id * drain * id)
//
//  /** W put request */
//  private val wput = (n:Option[String],v:Option[IVal]) => (id * writer(n,v,0,"W","T",None)) & ( dupl * dupl) & (id * drain * id)
//
//  /* W get request */
//  private val wget = (n:Option[String]) => (dupl * dupl) & (reader(n,0,"W",None) * drain * id)
//
//  /* NW get request */
//  private val nwget = (n:Option[String]) => getNB(n,0,"NW")
//
//  /* TO get request */
//  private val toget = (n:Option[String],to:Int) => getNB(n,to,"TO")
//
//  /** helper to build non-blocking put request (NW/TO) */
//  private val putNB = (n:Option[String],to:Int,t:String) => Prim("putNB",Port(IVal(1)),Port(IVal(2)), Set("T","to:"+to,oname(n,to,t))) // 1->2 (in->ok*err)
//
//  /** helper to build non-blocking get request (NW/TO) */
//  private val getNB = (n:Option[String],to:Int,t:String) =>
//    Prim("getNB",Port(IVal(2)),Port(IVal(1)),
//      Set("component","T",iname(n,to,t),"to:"+to)) //& (id * reader(n,t))

  // periodicity
  private def periodic(p:Int):Connector = Prim("psync",Port(IVal(1)),Port(IVal(1)),Set("T","period:"+p))

  private def addPeriod(p:Option[Int]):String = if(p.isDefined) s"period:${p.get}" else ""

  // maybe event with period
  private def mbPEvent(p:Option[Int]):Connector = if (p.isDefined) Prim("pevent",Port(IVal(1)),Port(IVal(1)),Set("period:"+p.get)) else event

  /**
    * Convert a TaskPort into a Connector that will be connected in a sequence with other ports
    *
    * @param p a task's port
    * @return  a connector
    */
  private def toSeqConnector(p:TaskPort,periodicity:Option[Int]=None):Connector = p match {
    case GetW(name)  => wget(name,periodicity)
    case GetNW(name) => nwget(name,periodicity)
    case GetTO(name,to) => toget(name,to,periodicity)
    case PutW(name,value) => wput(name,value,periodicity)
    case PutNW(name,value) => nwput(name,value,periodicity)
    case PutTO(name,value,to) => toput(name,to,value,periodicity)
  }

  /**
    * Convert a TaskPort into a connector
    * @param p a task's port
    * @return a connector
    */
  private def toSingleConnector(p:TaskPort,periodicity:Option[Int]=None):Connector = p match {
    case GetW(name)  => onlyGetW(name,periodicity)
    case GetNW(name) => onlyGetNW(name,periodicity)
    case GetTO(name,to) => onlyGetTO(name,to,periodicity)
    case PutW(name,value) => onlyPutW(name,value,periodicity)
    case PutNW(name,value) => onlyPutNW(name,value,periodicity)
    case PutTO(name,value,to) => onlyPutTO(name,value,to,periodicity)
  }

}



sealed trait TaskPort {
  val name:Option[String]

  def isInput:Boolean = this match {
    case GetW(_) | GetNW(_) | GetTO(_,_)  => true
    case _ => false
  }

  def isOutput:Boolean = !isInput

  def isWait:Boolean = this match {
    case GetW(_) | PutW(_,_) => true
    case _ => false
  }

  def isTO:Boolean = this match {
    case GetTO(_,_) | PutTO(_,_,_) => true
    case _ => false
  }

  def timeout:Int = this match {
    case GetTO(_,to) => to
    case PutTO(_,_,to) => to
    case _ => 0
  }

  override def toString: String = this match {
    case GetW(name)  => s"W ${name.getOrElse("")}?"
    case GetNW(name) => s"NW ${name.getOrElse("")}?"
    case GetTO(name,to) => s"$to ${name.getOrElse("")}?"
    case PutW(name,value) => s"W ${name.getOrElse("")}!${if(value.isDefined) s"=${value.get.n}" else ""}"
    case PutNW(name,value) => s"NW ${name.getOrElse("")}!${if(value.isDefined) s"=${value.get.n}" else ""}"
    case PutTO(name,value,to) => s"$to ${name.getOrElse("")}!${if(value.isDefined) s"=${value.get.n}" else ""}"
  }
}


case class PutTO(name:Option[String],value:Option[IVal],to:Int)  extends TaskPort
case class PutNW(name:Option[String],value:Option[IVal])         extends TaskPort
case class PutW(name:Option[String],value:Option[IVal])          extends TaskPort

case class GetTO(name:Option[String],to:Int)  extends TaskPort
case class GetNW(name:Option[String])         extends TaskPort
case class GetW(name:Option[String])          extends TaskPort