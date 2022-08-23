package info.kwarc.mmt.api.utils.time

object Time {
  def measure[A](exec : => A) : (Duration,A) = {
    val t0 = System.nanoTime()
    val result = exec
    val t1 = System.nanoTime()
    (Duration(t1 - t0),result)
  }
}

case class Duration(num : Long) {
  private var inum = num
  private var s = s"${(inum - ((inum / 1000000) * 1000000))}ns"
  if (inum > 1000000) {
    inum /= 1000000
    s = s"${(inum - ((inum / 1000) * 1000))}ms " + s
    if (inum > 1000) {
      inum /= 1000
      s = s"${(inum - ((inum / 60) * 60))}s " + s
      if (inum > 60) {
        inum /= 60
        s = s"${(inum - ((inum / 60) * 60))}min " + s
        if (inum > 60) {
          inum /= 60
          s = s"${inum}h " + s
        }
      }
    }
  }
  override def toString: String = s

  def +(that : Duration) = Duration(this.num + that.num)
}