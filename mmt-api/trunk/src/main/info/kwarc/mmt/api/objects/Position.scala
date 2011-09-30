package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

case class Position(indices : List[Int]) {
   def +(i : Int) = Position(indices ::: List(i))
   def current = indices.last
   override def toString = indices.mkString("","_","")
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