package info.kwarc.mmt.api.utils

/** Tests whether something is in a list */
case class Anyof[A](l : A*) {
  def ::(a : A) : Boolean = l.exists(_ == a)
  def !::(a : A) : Boolean = ! l.exists(_ == a)
}
