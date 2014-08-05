package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import scala.xml.{Node}


class TGroup(val parent: Path, val path: MPath, cs : List[Declaration]) extends ContentElement {
   def toNode : Node = <tgroup>{cs.map(_.toNode)}</tgroup>
   def children = cs
   def getComponents = Nil
   def getDeclarations = cs
   override def toString = cs.mkString("{\n","\n","\n}\n")
}
