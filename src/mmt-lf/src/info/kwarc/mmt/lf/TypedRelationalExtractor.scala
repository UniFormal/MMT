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
   val allUnary = List(IsUntypedConstant,IsDatatypeConstructor, IsDataConstructor, IsJudgementConstructor, IsRule, IsHighUniverse)
   val allBinary = Nil
           
   /** Discriminate the constant declarations contained in every structural element into the types data constructor, datatype constructor, rule, judgement constructor and high universe. 
    * Convert them into relational elements and apply a continuation function f to each such relational element
    * @note postcondition: Each constant in e, is annotated as exactly one of the types: 
    * data constructor, datatype constructor, rule, judgement constructor or high universe
    * 
    * @param e the structural element to look for constants in
    * @param f the continuation function
    */
   def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = {
      val path = e.path
      e match {
         case t: Module =>
          t.getDeclarations foreach {
              case c: Constant =>
                  val declType: Unary = c.tp match {
                      case Some(x) => x match {
                          case FunType(_, tp) => tp match {
                              case Univ(1) =>
                                  if (controller.globalLookup.getConstant(c.path).rl.contains("Judgment")) {
                                      IsJudgementConstructor
                                  }
                                  else {
                                      IsDatatypeConstructor
                                  }
                              case Univ(n) if n > 1 => IsHighUniverse
                              case _ =>
                                  if (controller.globalLookup.getConstant(c.path).rl.contains("Judgment")) IsRule else IsDataConstructor
                          }
                      }
                      case None => IsUntypedConstant
                  }
                  f(declType(c.path))
              case _ =>
          }
         case _ =>
      }
  }
}
