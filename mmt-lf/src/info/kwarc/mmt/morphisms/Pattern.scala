package info.kwarc.mmt.morphisms

import info.kwarc.mmt.api._
import symbols._
import objects._
import notations._

import info.kwarc.mmt.lf._

/**
 * a special constant representing a declaration pattern
 * 
 * it expands using LF-abstraction to name : {params} THEORY = [params] {| body |}
 */
class Pattern(home: Term, name : LocalName, val params: Context, val body : Context, notC: NotationContainer) extends
     FinalConstant(home, name, None, TermContainer(Pi(params, TheoryType())),
                                     TermContainer(Lambda(params, ComplexTheory(body))), None, notC) {

}

object Pattern {
   /** convenience constructor */
   def apply(home: Term, name : LocalName, params: Context, body : Context) = {
      new Pattern(home, name, params, body, NotationContainer())
   }
}

/**
 * a special constant representing an instance of a declaration pattern
 * 
 * it expands using LF-application to name : pattern(arguments)
 */
class Instance(home : Term, name : LocalName, val pattern : GlobalName, val arguments: List[Term]) extends
     FinalConstant(home, name, None, TermContainer(ApplySpine(OMS(pattern), arguments:_*)),
                                     TermContainer(None), None, NotationContainer()) {
}