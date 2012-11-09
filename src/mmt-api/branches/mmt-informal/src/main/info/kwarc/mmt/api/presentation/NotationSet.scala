package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import scala.xml.Node

sealed abstract class Defaults(val desc : String)
case object ImportsDefaults extends Defaults("use")
case object IgnoresDefaults extends Defaults("ignore")
object Defaults {
   def parse(s: String) : Defaults = s match {
      case "use" => ImportsDefaults
	  case "ignore" => IgnoresDefaults
      case s => throw ParseError("illegal attribute value ('use' or 'ignore' expected): " + s)
   }
}

/** Stores a set of notations for presentation */
class Style(val parent : DPath, name : LocalPath, val defaults : Defaults,
            val from : Path, val to : Path, report : frontend.Report)
                extends PresentationElement {
   private val notations = new scala.collection.mutable.HashMap[NotationKey,Notation]
   private def log(s : => String) = report("notations", s)
   def path = parent ? name
   /**
    * adds a notation element 
    */
   def add(n : Notation) {notations(n.key) = n}
   def get(key : NotationKey) : Option[Notation] = {notations.get(key)}
   val role = Role_Notationset
   val components = Nil //TODO
   override def toString = "style " + name + notations.values.map("\t" + _.toString).mkString(" {\n","\n","\n}")
   def toNode =
      <style name={name.flat} from={from.toPath} to={to.toPath} defaults={defaults.desc}>
         {notations.iterator.map(_._2.toNode)}
      </style>
}
