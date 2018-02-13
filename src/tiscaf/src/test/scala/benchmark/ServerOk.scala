package benchmark

import tiscaf._

object ServerOk extends HServer {

  def main(args: Array[String]): Unit = {
    try { new HStop("localhost", 8911) stop } catch { case _: Exception => }
    Thread.sleep(600)
    start
  }

  protected def ports = Set(8910)
  protected lazy val apps = List(theApp)

  override protected def tcpNoDelay = true // true for benchmarking only!!
  override def interruptTimeoutMillis = 100

  override def error(msg: String, t: Throwable): Unit = filterSomeErrors(t)

  private def filterSomeErrors(err: Throwable): Unit = err.getMessage match {
    case "Broken pipe"              =>
    case "Connection reset by peer" =>
    case _                          => err match {
      case _: java.nio.channels.ClosedSelectorException =>
      case e                                            => e.printStackTrace
    }
  }

  object theApp extends HApp {
    override def keepAlive = true
    override def chunked   = false
    override def buffered  = false
    override def gzip      = false
    override def tracking  = HTracking.NotAllowed

    def resolve(req: HReqData) = Some(Let)
  }

  object Let extends HSimpleLet {
    private val bytes = "Ok".getBytes("UTF-8")
    def act(tk: HTalk) {
      tk.setContentLength(bytes.size)
        .setContentType("text/plain; charset=UTF-8")
        .setContentLength(bytes.length) // if not buffered
        .write(bytes)
        ()
    }
  }

}
