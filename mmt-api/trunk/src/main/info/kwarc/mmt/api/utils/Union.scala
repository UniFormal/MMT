package info.kwarc.mmt.api.utils

class Union[A,B] {
   def left : Option[A] = None
   def right : Option[B] = None
   def <*>[C](f : A => C, g : B => C) : C = this match {
      case <(a) => f(a)
      case >(b) => g(b)
   }
   def <*[C](f : A => C) : Option[C] = this match {
      case <(a) => Some(f(a))
      case >(b) => None 
   }
   def *>[C](g : B => C) : Option[C] = this match {
      case <(a) => None 
      case >(b) => Some(g(b))
   }
}

case class <[A,B](a : A) extends Union[A,B] {
   override def left = Some(a)
}

case class >[A,B](b : B) extends Union[A,B] {
   override def right = Some(b)
}