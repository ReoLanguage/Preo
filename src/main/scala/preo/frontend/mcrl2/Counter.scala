package preo.frontend.mcrl2

class Counter {
  private var init_count = 1
  private var channel_count = 1
  private var sync_count1 = 1
  private var sync_count2 = 1


  def getSyncCount1: Int = sync_count1
  def incrementSyncCount1(inc: Int): Unit = sync_count1 += inc


  def getSyncCount2: Int = sync_count2
  def incrementSyncCount2(inc: Int): Unit = sync_count2 += inc
  def resetSyncCount2: Unit = sync_count2 = 1

  def getInitCount: Int = init_count
  def incrementInitCount(inc: Int): Unit = init_count+= inc

  def getChannelCount: Int = channel_count
  def incrementChannelCount(inc: Int): Unit = channel_count += inc

}
