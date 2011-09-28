package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

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
   def parse(s: String) = {
      val l = s.split("_").toList map {
         s => try {s.toInt} catch {case _ => throw ParseError("illegal position " + s)} 
      }
      Position(l)
   }
	val Init = Position(List(0))
	val None = Position(Nil)
}