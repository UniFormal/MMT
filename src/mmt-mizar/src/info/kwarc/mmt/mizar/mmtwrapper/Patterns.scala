package info.kwarc.mmt.mizar.mmtwrapper

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.patterns.Instance.Type
import info.kwarc.mmt.mizar.mmtwrapper.MizarPrimitiveConcepts._
import objects._
import notations._
import symbols._
import patterns._

//Utility objects for constructing MMT Patterns and  MMT Instances (of Patterns) for the Mizar Import
//Main function is to shield the rest of Mizar code from changes to the MMT API.
//Typically only these two objects need to be updated if the structural extensions API of MMT changes
object MizPattern {
  def apply(name : LocalName, params: Context, body : Context) = {
     patterns.Pattern(OMMOD(MizarPatternsTh), name, params, body, NotationContainer.empty())
  }
}

object MizInstance {
  //TODO: attach the notation to the *main* external declaration
   def apply(home : Term, name : LocalName, pattern : GlobalName, arguments: List[Term], notCont: NotationContainer = NotationContainer.empty()) = {
     //val argsS = arguments.map(a => TranslationController.controller.presenter.asString(a)).mkString("\n")
     //println("instance " + name + " with arguments " + argsS)
     patterns.Instance(home, name, pattern, arguments, notCont)
   }
  def unapply(dd: DerivedDeclaration) : Option[(Term, LocalName, GlobalName, List[Term], NotationContainer)] = dd match {
    case dd : DerivedDeclaration if (dd.feature == "instance") =>
      val home = dd.home
      val name = dd.name
      val tp = dd.tpC
      val Type(patternMPath, args) = tp.get.getOrElse(throw ImplementationError("No type found for derived declaration at: "+home.toMPath ? name.toString))
      val notC = dd.notC
      val pattern = patternMPath.doc ? patternMPath.name.init ? patternMPath.name.last
      Some((home, name, pattern, args, notC))
    case _ => None
  }
}
