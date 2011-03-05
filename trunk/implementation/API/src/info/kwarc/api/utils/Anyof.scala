package jomdoc.utils

case class Anyof[A](l : A*) {
  def ::(a : A) : Boolean = l.exists(_ == a)
  def !::(a : A) : Boolean = ! l.exists(_ == a)
}
