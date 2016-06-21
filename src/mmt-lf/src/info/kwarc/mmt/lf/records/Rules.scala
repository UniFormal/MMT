package info.kwarc.mmt.lf.records

import info.kwarc.mmt.api._
import uom._
import objects._
import checking._

import info.kwarc.mmt.lf._

object Simplification extends DepthRule(Elim.path, Intro.path) {
   val apply: Rewrite = (bef,inn,aft) =>
      if (bef.isEmpty && aft.length == 1) aft match {
         case List(oml: OML) =>
            val vals = inn.collect {
               case OML(VarDecl(l,_,Some(d),_)) if l == oml.vd.name => d
            }
            vals match {
               case List(d) => GlobalChange(vals.head)
               case _ => NoChange
            }
      } else
         NoChange
}