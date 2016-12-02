package info.kwarc.mmt.api.utils

/** while loops with break and continue */
object While {
  private object Break extends Throwable
  def break {throw Break}
  
  private object Continue extends Throwable
  def continue {throw Continue}
  
  @scala.annotation.tailrec
  def apply(b: => Boolean)(body: => Unit) {
    if (b) {
      try {body}
      catch {
        case Break => return
        case Continue =>
      }
      apply(b)(body)
    }
  }
}