package zgs.sync


private object SyncExe {
  private var num : Int = 0 
  def nextNum : Int = synchronized { val v = num; num += 1; v }
}


final class SyncExe(poolSize : Int, queueCap : Int, name : String) {
  
  //-------------------- API -----------------------------------
  
  def this(poolSize : Int, queueCap : Int) = this(poolSize, queueCap, "SyncExe")
  def this(poolSize : Int, name : String)  = this(poolSize, Int.MaxValue, name)
  def this(poolSize : Int)                 = this(poolSize, Int.MaxValue)
    
  // rises exception if the queue is full
  
  def submit(task : Runnable) : Unit = {
    require(working.get, "submitting to shutted down SyncExe")
    // require(qu.size < queueCap, "SyncExe queue is full") // not needed for own Qu
    qu.put(task)
  }

  def shutdown : Unit = working.set(false) // doesn't prevent to drain the queue
  
  def shutdownNow : Unit = {
    shutdown
    qu.close // closes the queue for both 'take' and 'put'
    cancelAll
  }
  
  //-------------------- internals ------------------------------
  
  private val working = new SyncBool(true)
  private val qu      = new SynQu[Runnable](queueCap)
  
  private val threads = (for(i <- 0 until poolSize) yield {
    val t = newThread(i); t.start; t }).toList
  
  private def newThread(i : Int) : Thread = new Thread {
    // user must catch exception herself if is interested in
    override def run = while (working.get) { try { qu.take.run } catch { case _ => } }
    setName(SyncExe.this.name + "-" + SyncExe.nextNum + "-" + i)
  }
      
  private def cancelAll : Unit = threads.foreach { t => try { t.interrupt } catch { case _ => } }
}
