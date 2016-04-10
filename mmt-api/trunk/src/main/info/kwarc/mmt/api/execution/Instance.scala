package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import modules._
import symbols._
import objects._
import symbols._

class Instance(heap: MPath, of: MPath, id: Int) extends DeclaredStructure(OMMOD(heap), LocalName(id.toString), TermContainer(OMMOD(of)), false) {
   def assign(p: GlobalName, t: Term) {
      val name = p.toLocalName
      getO(name) match {
        case Some(c: Constant) =>
          c.dfC.analyzed = t
        case Some(_) =>
          
        case None =>
          val c = ConstantAssignment(home, name, Nil, Some(t))
          add(c)
      }
   }
}