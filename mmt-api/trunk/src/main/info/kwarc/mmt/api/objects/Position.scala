package info.kwarc.mmt.api.objects

case class Position(indices : List[Int]) {
   def +(i : Int) = indices match {
	   case Nil => Position.None
	   case _ => Position(i :: indices)
   }
   def current = indices.head
   override def toString = indices.mkString("id_","_","")
   def toIDAttr = if (indices != Nil) new scala.xml.PrefixedAttribute("xml", "id", toString, scala.xml.Null)
      else scala.xml.Null
}
object Position {
	val Init = Position(List(0))
	val None = Position(Nil)
}