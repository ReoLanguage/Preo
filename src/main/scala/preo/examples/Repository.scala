package preo.examples

import preo.DSL._
import preo.ast._

/**
  * Created by jose on 01/07/16.
  */
object Repository {

  def filter(p:Any=>Boolean) = Prim("filter",1,1,Set(p))
  def transf[A,B,C](f:(A,B)=>C) = Prim("transf",2,1,Set(f))
  def transf[A,B](f:A=>B) = Prim("transf",1,1,Set(f))
  def writer(l:List[Any]) = Prim("writer",0,1,Set(l))
  def writer(v:Int)    = Prim("writer",0,1,Set(List(v)))
  def writer(v:String) = Prim("writer",0,1,Set(List(v)))
  def reader(n:Int) = Prim("reader",1,0,Set(n))

  /** alternates between 2 inputs */
  val alternator = dupl*dupl & id*drain*fifo & merger
  /** routes non-deterministically to 2 outputs */
  val exrouter = dupl & dupl*id & (lossy*lossy & dupl*dupl & id*swap*id & id*id*merger)*id & id*id*drain

  val fifos = dupl & fifo*fifo

  /** n-ary sequence of a connector. */
  def seq(i:Interface, c:Connector, x:preo.DSL.TypedVar, n:IExpr) =
    Trace(Repl(i,n-1), (c^(x<--n)) & sym(Repl(i,n-1),i) ) | n>0
  /** n-ary sequence of a connector. */
  def seq(i:Interface, c:Connector, n:IExpr) =
    Trace(Repl(i,n-1), (c^n) & sym(Repl(i,n-1),i) ) | n>0

  /** sequence of n fifos. */
  val seqfifo = lam(n,Tr(n - 1, sym(n - 1,1) & (fifo^n)))
  val seqfifo2 = lam(n,seq(1,fifo,n))
  val seqfifo3 = lam(n,Tr(n - 1, (fifo^n) & sym(n - 1,1)))

  /** rearrange 2*n entries: from n+n to 2+2+...+2 (n times) */
  val zip = lam(n,
    Tr( 2 * (n:IExpr) * (n-1), //2*n*(n-1),
      (((id^(n-x)) * (swap^x) * (id^(n-x)))^x<--n) &
        sym(2*(n:IExpr)*(n-1),2*(n:IExpr))
    ))
  /** rearrange 2*n entries: from 2+2+...+2 (n times) to n+n */
  val unzip = lam(n,
    Tr( 2*(n:IExpr)*(n-1),
      (((id^(x+1)) * (swap^(n-x-1)) * (id^(x+1)))^(x,n)) &
        sym(2*(n:IExpr)*(n-1),2*(n:IExpr))
    ))
  /** alternates between the output of a value on each of its n outputs */
  val fifoloop =
    lam(n,Tr(n, sym(n-1,1) & ((fifofull & dupl) * ((fifo & dupl)^(n-1))) & unzip(n) ))
  /** alternate flow between n flows [[http://reo.project.cwi.nl/webreo/generated/sequencer/frameset.htm]] */
  val sequencer = lam(n,
    (((dupl^n) & unzip(n)) * fifoloop(n) ) &
    ((id^n) * (zip(n) & (drain^n))))
  /** sequencer with only inputs (alternates which input is ready) **/
  val sequencerIn = lam(n, ((id^n) *
    Tr(n, sym(n-1,1) & ((fifofull & dupl) * ((fifo & dupl)^(n-1))) & unzip(n) ) ) &
    (zip(n) & (drain^n)))

  /** n-ary duplicator */
  val dupls = lam (n, Tr(n-1,(id * (dupl^(n-1))) & sym(1,(n-1)*2)))
  def duplsGen(d:Connector): Connector =
    lam (n, Tr(n-1,(id * (d^(n-1))) & sym(1,(n-1)*2)))

  /** n-ary variable duplicator */
  val vdupls = lam (n, Tr(n-1,(id * (vdupl^(n-1))) & sym(1,(n-1)*2)))


  /** n-ary merger */
  val mergers = lam (n, Tr(n-1,sym((n-1)*2,1) & (id * (merger^(n-1)))))

  /** n-ary variable merger */
  val vmergers = lam (n, Tr(n-1,sym((n-1)*2,1) & (id * (vmerger^(n-1)))))

  /** n-ary merger and duplicator (node) */
  val node = mergers & dupls
  def nodeGen(d:Connector): Connector =
    mergers & duplsGen(d)

  /** n-ary exrouter */
  // n-ary exrouter
  //   val nexrouter = lam(n, Prim("dupl",1,n+1) &
  //     (((lossy & dupl)^n) & unzip(n))*id &
  //     (id^(n+1))*(dupl^(n-1))*id &
  //     (id^n)*(drain^n) )
  val nexrouter = lam(n, (dupls(n+1)) &
    (((lossy & dupl)^n) & unzip(n))*id &
    (id^n)*(mergers(n))* id &
    (id^n)*drain )

  /** forces 2 or more sinks to synchronise */
  val barrier = (dupl*dupl) & (id*drain*id)
  val barriers = lam(n,
    (n===1)? id + (
      (dupl*((dupl&(id*dupl))^(n-2)) * dupl) & (id*((drain*id)^(n-1)))
    ))

//  val ndupl = lam(n, Trace( NN, ((dupl * (id^x))^(x<--(n-1))) & sym(NN,n) ))
//  val dupl4 = lam(n, Trace( 5 , ((dupl * (id^x))^(x<--3))     & sym(5,4) ))
//  val ndupl = lam(n, Tr( ((n+1)*(n-2))/2, ((dupl * (id^x))^(x<--(n-1))) & sym( ((n+1)*(n-2))/2 ,n) ))

}
