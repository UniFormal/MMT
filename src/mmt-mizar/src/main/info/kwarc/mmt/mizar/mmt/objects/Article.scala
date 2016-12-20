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
import objects.Conversions._

import info.kwarc.mmt.mizar.mizar.translator._

import info.kwarc.mmt.morphisms._


object artPatterns {
  /* Notations */
  val SynonymicNotation = MizPattern(LocalName("SynonymicNotation"),
    Context(
      VarDecl("nr", None, None, None),
      VarDecl("args", Some(Rep(Mizar.constant("tp"),OMV("nr"))), None, None),
      VarDecl("ref", Some(Mizar.constant("prop")), None, None)
      ),
    Context(VarDecl("notation",Some(OMV("ref")), None, None))
  )
  
  val AntonymicNotation = MizPattern(LocalName("AntonymicNotation"),
    Context(
      VarDecl("nr", None, None, None),
      VarDecl("args", Some(Rep(Mizar.constant("tp"),OMV("nr"))), None, None),
      VarDecl("ref", Some(Mizar.constant("prop")), None, None)
      ),
    Context(VarDecl("notation", Some(Mizar.apply(Mizar.constant("not"),OMV("ref"))), None, None))
  )
  
  
  val Lemma = MizPattern(LocalName("Lemma"),
    Context(VarDecl("prop", Some(Mizar.constant("prop")),None, None)),
    Context(VarDecl("lemma", Some(Mizar.apply(Mizar.constant("proof"), OMV("prop"))), None, None)))
}

