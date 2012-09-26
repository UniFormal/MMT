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
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lfs._
import info.kwarc.mmt.api.patterns._
import objects.Conversions._

import info.kwarc.mmt.mizar.mizar.translator._


object artPatterns {
  /*
   * Notations
   */
  
  val SynonymicNotation : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("SynonymicNotation"),
      Context(
          VarDecl("nr", None, None),
          VarDecl("args", Some(Rep(Mizar.constant("tp"),OMV("nr"))), None),
          VarDecl("ref", Some(Mizar.constant("prop")), None)
          ),
      Context(VarDecl("notation",Some(OMV("ref")), None))
  )
  
  val AntonymicNotation : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("AntonymicNotation"),
      Context(
          VarDecl("nr", None, None),
          VarDecl("args", Some(Rep(Mizar.constant("tp"),OMV("nr"))), None),
          VarDecl("ref", Some(Mizar.constant("prop")), None)
          ),
      Context(VarDecl("notation", Some(Mizar.apply(Mizar.constant("not"),OMV("ref"))), None))
  )
  
  
  val Lemma : Pattern = new Pattern(OMMOD(Mizar.MizarPatternsTh), LocalName("Lemma"),
      Context(VarDecl("prop", Some(Mizar.constant("prop")),None)),
      Context(VarDecl("lemma", Some(Mizar.apply(Mizar.constant("proof"), OMV("prop"))), None)))
  
}

