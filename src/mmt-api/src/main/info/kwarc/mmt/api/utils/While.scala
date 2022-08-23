package info.kwarc.mmt.api.utils

/** while loops with break and continue */
object While {
  private object Break extends Throwable
  def break: Unit = {throw Break}

  private object Continue extends Throwable
  def continue: Unit = {throw Continue}

  @scala.annotation.tailrec
  def apply(b: => Boolean)(body: => Unit): Unit = {
    if (b) {
      try {body}
      catch {
        case Break => return
        case Continue =>
      }
      apply(b)(body)
    }
  }

  /** like while (a.isEmpty) {body} but eventually returns the value of a */
  def undefined[A](a: => Option[A])(body: => Unit): A = {
    var x: Option[A] = a
    while (x.isEmpty) {
      body
      x = a
    }
    x.get
  }
}
