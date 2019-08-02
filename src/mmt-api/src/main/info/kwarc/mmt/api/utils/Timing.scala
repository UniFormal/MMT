package info.kwarc.mmt.api.utils

/** allows timing of operations */
class Timer(key: String) {
  private var count: Long = 0
  def apply[A](code: => A) = {
    val bef = java.lang.System.currentTimeMillis()
    val a = code
    val aft = java.lang.System.currentTimeMillis()
    val d = aft - bef
    count += d
    a
  }
  override def toString = key + ": " + count
}
