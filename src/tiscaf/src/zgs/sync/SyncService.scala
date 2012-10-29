package zgs.sync


trait SyncService {

  //------------------------- to override ----------------
  
  protected def doProduce(what : Any) : Any = sys.error("doProduce is not overriden")
  protected def doConsume(what : Any) : Unit = sys.error("doConsume is not overriden")
  
  protected def hookOrder : Int = 0
  protected def name = "SyncService"
  
  protected def onError(e : Throwable) = e.printStackTrace
  
  //-------------------- API ------------------------------
  
  def execute(code : () => Unit) : Unit = qu.put(ExecuteMsg(code))
  def run(toRun : Runnable) : Unit      = qu.put(RunMsg(toRun))
  
  // blocks...
  def produce(what : Any) : Any = {
    val client = new SyncField[Any]
    qu.put(ProduceMsg(client, what))
    client.take // ...here    
  }
  
  def consume(what : Any) : Unit = qu.put(ConsumeMsg(what))
  
  def spam(period: Long, what: Any) : java.util.Timer = {
    val timer = new java.util.Timer(true) // daemon
    timer.scheduleAtFixedRate(
      new java.util.TimerTask { def run { consume(what) } },
      period, period
    )
    timer // to be able to 'cancel'
  }
  
  // not tested yet
  def close : Unit = qu.put(ExitMsg) // 'spam' users will get exception at case timer is not canceled

  //---------------------- internals ----------------------
  
  //-------- msg types
    
  sealed private trait ExeMsg
    private case class  ConsumeMsg(what : Any)                        extends ExeMsg
    private case class  ExecuteMsg(code : () => Unit)                 extends ExeMsg
    private case class  RunMsg(toRun : Runnable)                      extends ExeMsg
    private case class  ProduceMsg(whom : SyncField[Any], what : Any) extends ExeMsg
    private case object ExitMsg                                       extends ExeMsg

  //-------- queue related
  
  private val qu  = new SynQu[ExeMsg]
  private val closed = new SyncBool(false)
  
  //------------ the thread related
  
  private val theThread = new Thread { 
    setDaemon(true) // !
    setName(name + "-" + getName)
    
    private def tryCatch(code : => Unit) : Unit = { try { code } catch  { case _ => onError _ } }

    @scala.annotation.tailrec
    override def run : Unit = qu.take match {
      case ConsumeMsg(what)       => tryCatch(doConsume(what)); run
      case ExecuteMsg(code)       => tryCatch(code); run
      case RunMsg(toRun)          => tryCatch(toRun.run); run
      case ProduceMsg(whom, what) => tryCatch(whom.put(doProduce(what))); run
      case ExitMsg                => qu.close // exit recursion
    }

  }
  theThread.start
  
  //---------------------
  
  private def shutdown : Unit = { qu.put(ExitMsg); theThread.join() }
  
  zgs.utl.ShutHook.register(hookOrder, shutdown)

}
