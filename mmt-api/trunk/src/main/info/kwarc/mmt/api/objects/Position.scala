package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

/** A path in the syntax tree of an object */
case class Position(indices : List[Int]) {
   def /(i : Int) = Position(indices ::: List(i))
   def +(i : Int) = this / i //deprecated
   def current = indices.last
   override def toString = indices.mkString("","_","")
   def toIDAttr = if (indices != Nil) new scala.xml.UnprefixedAttribute("position", toString, scala.xml.Null)
      else scala.xml.Null
}
object Position {
   /** Parse a string representation of a list of integers
    *  @param s the string encoded by Position.toString */
   def parse(s: String) = {
      val l = s.split("_").toList map {
         s => try {s.toInt} catch {case _ => throw ParseError("illegal position " + s)} 
      }
      Position(l)
   }
	val Init = Position(Nil)
}


/** A path in an object
 *  @param parent the knowledge item
 *  @param component an object that's part of the parent's declaration. Example: if component is "type", then it refers to the type of the parent
 *  @param pos position in the syntax tree (default is the root, i.e. the component) */
case class OPath(parent: Path, component: String, pos: Position = Position.Init) {
   def /(i: Int) = OPath(parent, component, pos / i)
}