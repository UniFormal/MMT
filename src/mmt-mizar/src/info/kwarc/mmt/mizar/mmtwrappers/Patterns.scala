package info.kwarc.mmt.mizar.mmtwrappers

import info.kwarc.mmt.api._
import objects._
import notations._
import symbols._

//Utility objects for constructing MMT Patterns and  MMT Instances (of Patterns) for the Mizar Import
//Main function is to shield the rest of Mizar code from changes to the MMT API.
//Typically only these two objects need to be updated if the structural extensions API of MMT changes
object MizPattern {
  def apply(name : LocalName, params: Context, body : Context) = {
     patterns.Pattern(OMMOD(Mizar.MizarPatternsTh), name, params, body, NotationContainer())
  }
}

object MizInstance {
   def apply(home : Term, name : LocalName, pattern : GlobalName, arguments: List[Term], notCont: NotationContainer = NotationContainer()) = {
     //val argsS = arguments.map(a => TranslationController.controller.presenter.asString(a)).mkString("\n")
     //println("instance " + name + " with arguments " + argsS)
     patterns.Instance(home, name, pattern, arguments, notCont)
   }
}
