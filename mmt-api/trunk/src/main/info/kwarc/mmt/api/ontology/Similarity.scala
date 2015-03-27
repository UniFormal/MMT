package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import objects._
import symbols._
import frontend._

/** similarity measure; for now integers */
case class Measure(value: Int)

class Similarity(controller: Controller) {
   def objects(a: Obj, b: Obj): Measure = ???
   
   def declarations(a: Declaration, b: Declaration): Measure = {
      val aDecs  = a.getDeclarations
      val aComps = a.getComponents      
      val bDec   = b.getDeclarations
      val bComps = b.getComponents
      ???
   }
}