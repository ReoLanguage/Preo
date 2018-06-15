package preo.modelling

abstract class Family[A]{
  def instanciate(n: A): (Set[Action], Set[Mcrl2Def], Set[ProcessName])
}

//what does a family receive
//what are the common methods
//what does it store
//how do we instanciate

class ExpFamily(act: Set[Action]) extends Family[Int]

//Like this?
class OptFamily[A] extends Family[A]

class TraceFamily extends Family[Int]

class SymFamily extends Family[Int]