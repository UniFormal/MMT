package info.kwarc.mmt.morphisms

import info.kwarc.mmt.api._
import symbols._
import objects._
import notations._

import info.kwarc.mmt.lf._


/**
 * a special constant representing an instance of a declaration pattern
 * 
 * it expands using LF-application to name : pattern(arguments)
 */
class Instffance(home : Term, name : LocalName, val pattern : GlobalName, val arguments: List[Term]) extends
     FinalConstant(home, name, Nil, TermContainer(ApplySpine(OMS(pattern), arguments:_*)),
                                     TermContainer(None), None, NotationContainer()) {
}