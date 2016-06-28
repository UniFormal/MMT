package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects.Conversions._

/** A path in the syntax tree of an object */
case class Position(indices : List[Int]) {
   def /(p:Position): Position = Position(indices ::: p.indices)
   def /(l : List[Int]): Position = this / Position(l)
   def /(i : Int): Position = this / Position(i)
   def current = indices.last
   override def toString = indices.mkString("","_","")
}

object Position {
   /** Parse a string representation of a list of integers
    *  @param s the string encoded by Position.toString */
   def parse(s: String) = {
      val l = if (s == "") Nil else s.split("_").toList map {
         s => try {s.toInt} catch {case _ : Throwable => throw ParseError("illegal position " + s)} 
      }
      Position(l)
   }
	val Init = Position(Nil)
	/** same Init / i */
	def apply(i: Int) : Position = Position(List(i))
   /** @return the positions of the subobjects of t */
    def positions(t: Obj) = (0 until t.subobjects.length).map(i => Position(i)).toList
}
