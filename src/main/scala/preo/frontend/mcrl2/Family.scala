package preo.frontend.mcrl2

import preo.ast.Connector
import preo.frontend.Eval

/**@
  * Family with many models. This class uses these models with mCRL2 to generate lps, lts, and prove properties
  * @param models
  */
class Family(val models: List[Model])


object Family{
  def apply(con: Connector): Family ={
    val instances = Eval.getInstances(con)
    //for every value we create an app with the vars in the instances and turn it into a core connector
    val connectors = instances.map(x => Eval.simpleReduce(x))
    new Family(connectors.map(c => Model(c)))
  }


//  def ap

}



