package zgs.sync

object Sync {
  def spawn(code: => Unit) : Unit =
    new Thread(new Runnable { def run : Unit = { code } }) start
}
