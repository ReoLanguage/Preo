package preo.modelling

import preo.common.WrongNameException

/**
  * class keeps a name associated to a list of channels.
  * Usefull for familyModel
  * @param name name of the primitive
  * @param items list of channels already created
  */

class ChannelSubs(name: String,var items: List[Mcrl2Channel]) {
  var next: Int = items.length

  def reset(): Unit = this.next = 0

  def hasNext(): Boolean = next < items.length

  def getNext(): Mcrl2Channel = {
    val item = items.drop(this.next).head
    next = next + 1
    item
  }

  def put(item: Mcrl2Channel): Unit = if(item.name == name) {next = items.length+1; items ++= List(item)} else throw new WrongNameException(s"Expected $name, got ${item.name}")
}
