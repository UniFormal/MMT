package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.lf._
import info.kwarc.mmt.api.patterns._
import objects.Conversions._

import info.kwarc.mmt.mizar.mizar.translator._


object artPatterns {
  /*
   * Notations
   */
  
  val SynonymicNotation : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("SynonymicNotation"),
      Context(
          TermVarDecl("nr", None, None),
          TermVarDecl("args", Some(OMA(OMID(mmt.repetition), Mizar.constant("tp") :: OMV("") :: Nil)), None),
          TermVarDecl("ref", Some(Mizar.constant("prop")), None)
          ),
      Context(TermVarDecl("_not",Some(OMV("ref")), None))
  )
  
  val AntonymicNotation : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("AntonymicNotation"),
      Context(
          TermVarDecl("nr", None, None),
          TermVarDecl("args", Some(OMA(OMID(mmt.repetition), Mizar.constant("tp") :: OMV("") :: Nil)), None),
          TermVarDecl("ref", Some(Mizar.constant("prop")), None)
          ),
      Context(TermVarDecl("_not", Some(OMA(Mizar.constant("not"),List(OMV("ref")))), None))
  )
  
  
  val Lemma : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("Lemma"),
      Context(TermVarDecl("prop", Some(Mizar.constant("prop")),None)),
      Context(TermVarDecl("lemma", Some(OMA(Mizar.constant("proof"), List(OMV("prop")))), None)))
  
}

