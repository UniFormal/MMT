package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import scala.xml.Node

sealed abstract class Defaults(val desc : String)
case object ImportsDefaults extends Defaults("use")
case object IgnoresDefaults extends Defaults("ignore")

/** Stores a set of notations for presentation */
class Style(val parent : DPath, name : LocalPath, val from : Path, val to : Path)
                extends PresentationElement {
   private val notations = new scala.collection.mutable.HashMap[NotationKey,SimpleNotation]
   def path = parent ? name
   /**
    * adds a notation element 
    */
   def add(n : SimpleNotation) {notations(n.key) = n}
   def get(key : NotationKey) : Option[SimpleNotation] = {notations.get(key)}
   val role = Role_Notationset
   val components = Nil //TODO
   override def toString = "style " + name + notations.values.map("\t" + _.toString).mkString(" {\n","\n","\n}")
   def toNode =
      <style name={name.flat} from={from.toPath} to={to.toPath}>
         {notations.iterator.map(_._2.toNode)}
      </style>
}
