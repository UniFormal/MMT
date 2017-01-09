package org.omdoc.latin.foundations.mizar

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.documents._

import info.kwarc.mmt.mizar.mmt.objects._

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
     patterns.Instance(home, name, pattern, arguments, notCont)
   }
}

object MizarPatterns extends uom.RealizedTheory(Some(info.kwarc.mmt.sequences.LFS._path)) {
  // the path used to include this
  override lazy val mpath = Mizar.MizarPatternsTh
  
  val patterns : List[DerivedDeclaration] 
               = List(
                   DefPatterns.MizAttrIsCompleteDef,
                   DefPatterns.MizAttrIsPartialDef,
                   DefPatterns.MizAttrMeansCompleteDef,
                   DefPatterns.MizAttrMeansPartialDef,
                   DefPatterns.MizFuncIsCompleteDef,
                   DefPatterns.MizFuncMeansCompleteDef,
                   DefPatterns.MizFuncIsPartialDef,
                   DefPatterns.MizFuncMeansPartialDef,
                   DefPatterns.MizModeIsCompleteDef,
                   DefPatterns.MizModeIsPartialDef,
                   DefPatterns.MizModeMeansCompleteDef,
                   DefPatterns.MizModeMeansPartialDef,
                   DefPatterns.MizPredIsCompleteDef,
                   DefPatterns.MizPredIsPartialDef,
                   DefPatterns.MizPredMeansCompleteDef,
                   DefPatterns.MizPredMeansPartialDef,
                   DefPatterns.MizStructDef(1),
                   DefPatterns.MizStructDef(2),
                   DefPatterns.MizStructDef(3),
                   DefPatterns.MizStructDef(4),
                   DefPatterns.MizStructDef(5),
                   DefPatterns.MizStructDef(6),
                   DefPatterns.MizStructDef(7),
                   DefPatterns.MizStructDef(8),
                   DefPatterns.MizStructDef(9),
                   DefPatterns.MizStructDef(10),
                   DefPatterns.MizSelDef,
                   SchemePatterns.MizSchemeDef,
                   artPatterns.AntonymicNotation,
                   artPatterns.Lemma,
                   artPatterns.SynonymicNotation,
                   RegPatterns.MizExistentialReg,
                   RegPatterns.MizConditionalReg,
                   RegPatterns.MizFunctionalReg
                   )
  // lazily build the body of the theory
  declare {
    add(PlainInclude(Mizar.HiddenTh, path))
    patterns foreach {add(_,None)}
  }
}