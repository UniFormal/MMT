package org.omdoc.latin.foundations.mizar

import info.kwarc.mmt.api._
import objects._
import notations._
import symbols._
import modules._
import documents._

import info.kwarc.mmt.mizar._
import mmtwrappers._
import translator._

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
    patterns foreach {add(_)}
  }
}
