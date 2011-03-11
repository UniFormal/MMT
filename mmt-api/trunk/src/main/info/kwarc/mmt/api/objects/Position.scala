package info.kwarc.mmt.api.objects

case class Position(p : List[Int]) {
   def +(i : Int) = p match {
	   case Nil => Position.None
	   case _ => Position(i :: p)
   }
   def current = p.head
   override def toString = p.mkString("id_","_","")
   def toIDAttr = if (p != Nil) new scala.xml.PrefixedAttribute("xml", "id", toString, scala.xml.Null)
      else scala.xml.Null
}
object Position {
	val Init = Position(List(0))
	val None = Position(Nil)
}