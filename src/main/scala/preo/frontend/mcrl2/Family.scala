//package preo.frontend.mcrl2
//
//import preo.ast.Connector
//import preo.frontend.Eval
//import sys.process._
//import java.io._
//
//
///**@
//  * Family with many models. This class uses these models with mCRL2 to generate lps, lts, and prove properties
//  * @param models
//  */
//class Family(val models: List[Model]){
//  def generateLPS: Int = {
//    val id = Thread.currentThread().getId
//    val file = new File(s"/tmp/model_$id.mcrl2")
//    file.setExecutable(true)
//    val pw = new PrintWriter(file)
//    pw.write(models.head.toString)
//    pw.close()
//    s"mcrl22lps /tmp/model_$id.mcrl2 /tmp/model_$id.lps" !
//  }
//
//  def generateLTS: Int = {
//    generateLPS
//    val id = Thread.currentThread().getId
//    s"lps2lts /tmp/model_$id.lps /tmp/model_$id.lts" !
//  }
//
//  def show_Graph: Int = {
//    generateLTS
//    val id = Thread.currentThread().getId
//    s"ltsgraph /tmp/model_$id.lts" !
//  }
//
//}
//
//
//object Family{
//  def apply(con: Connector): Family ={
//    val instances = Eval.getInstances(con)
//    //for every value we create an app with the vars in the instances and turn it into a core connector
//    val connectors = instances.map(x => Eval.simpleReduce(x))
//    new Family(connectors.map(c => Model(c)))
//  }
//
//
////  def ap
//
//}
//
//
//
