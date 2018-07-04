package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import symbols._
import documents._
import modules._
import symbols._
import patterns._
import objects._
import frontend._
import opaque._
import notations._
import ontology._

/**
 * The TypedRelationalExtractor adds a few more typing related informations to the .rel files.
 */
class TypedRelationalExtractor extends RelationalExtractor {
   val allUnary = List(IsDatatypeConstructor, IsDataConstructor, IsJudgementConstructor, IsRule, IsHighUniverse)
   val allBinary = Nil
           
   /** apply a continuation function to every relational element of a StructuralElement */
   def apply(e: StructuralElement)(implicit f: RelationalElement => Unit) {
      val path = e.path
      e match {
         case t: DeclaredModule =>
            t.getDeclarations foreach {
					  case c: Constant =>
               	c.tp match {
               	  case Some(x) => x match {
                 	  case FunType(_, tp) => tp match {
                   	  case Univ(1) => 
                   	    if (controller.globalLookup.getConstant(c.path).rl.contains("Judgment")) {
                   	      f(IsJudgementConstructor(c.path))
                   	    }
                   	    else {
                   	      f(IsDatatypeConstructor(c.path))
                        }
                   	  case Univ(n) if n > 1 => println("found high universe at "+c.path.toString()); (IsHighUniverse(c.path))
                   	  case _ => 
                   	    if (controller.globalLookup.getConstant(c.path).rl.contains("Judgment")) {
                   	      f(IsRule(c.path))
                   	    }
                   	    else {
                   	      f(IsDataConstructor(c.path))
                   	    }
                   	  }
                 	  }
               	  case None => f(IsUntypedConstant(c.path))
               	}
            }
         case _ =>
      }
  }
}
