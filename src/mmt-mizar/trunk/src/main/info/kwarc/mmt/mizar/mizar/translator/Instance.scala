package info.kwarc.mmt.mizar.mizar.translator
import info.kwarc.mmt.api._
import objects._
import symbols._

object Instance {
   def apply(home : Term, name : LocalName, pattern : MPath, arguments: List[Term]) = {
     DeclaredStructure(home, name, OMPMOD(pattern, arguments), false)
   }
}