package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

/** A path in the syntax tree of an object */
case class Position(indices : List[Int]) {
   def /(p:Position): Position = Position(indices ::: p.indices)
   def /(i : Int): Position = this / Position(i)
   def current = indices.last
   override def toString = indices.mkString("","_","")
}
object Position {
   /** Parse a string representation of a list of integers
    *  @param s the string encoded by Position.toString */
   def parse(s: String) = {
      val l = s.split("_").toList map {
         s => try {s.toInt} catch {case _ : Throwable => throw ParseError("illegal position " + s)} 
      }
      Position(l)
   }
	val Init = Position(Nil)
	def apply(i: Int) : Position = Position(List(i))
   /** @return the positions of the subobjects of t */
   def positions(t: Obj) = t match {
      case ComplexTerm(f, args, con, scs) =>
         val n = 1+args.length+con.variables.length+scs.length
         Range(0,n).toList.map(i => Position(i))
      case _ => t.components.indices.toList.map(Position(_))
   }

}
