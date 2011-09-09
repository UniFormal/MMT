package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import scala.xml.{Node}

class TGroup(val parent: Path, val path: Path, cs : List[Symbol]) extends ContentElement {
   def toNode : Node = <tgroup>{cs.map(_.toNode)}</tgroup>
   def role : Role = Role_TGroup
   /** the components are an abstract definition of the children of a knowledge item */
   def components : List[Content] = cs
   override def toString = cs.mkString("{\n","\n","\n}\n")
}