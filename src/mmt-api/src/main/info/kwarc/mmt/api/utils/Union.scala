package info.kwarc.mmt.api.utils

/** disjoint union of two types */
sealed abstract class Union[A,B] {
   def left : Option[A]
   def right : Option[B]
}

case class Left[A,B](value : A) extends Union[A,B] {
   def left = Some(value)
   def right = None
}

case class Right[A,B](value : B) extends Union[A,B] {
   def left = None
   def right = Some(value)
}
