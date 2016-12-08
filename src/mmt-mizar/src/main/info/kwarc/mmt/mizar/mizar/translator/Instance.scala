package info.kwarc.mmt.mizar.mizar.translator
import info.kwarc.mmt.api._
import objects._
import symbols._
import notations._

object Instance {
   def apply(home : Term, name : LocalName, pattern : GlobalName, arguments: List[Term], notC: NotationContainer = NotationContainer()) = {
     patterns.Instance(home, name, pattern, arguments, notC)
   }
}