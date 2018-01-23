package info.kwarc.mmt.mizar.mmtwrappers

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import objects.Conversions._

import info.kwarc.mmt.mizar.objects._
import MizSeq._
import info.kwarc.mmt.mizar.translator._

object artPatterns {
  /* Notations */
  val SynonymicNotation = MizPattern(LocalName("SynonymicNotation"),
    Context(
      VarDecl("nr"),
      VarDecl("args", Rep(Mizar.constant("tp"),OMV("nr"))),
      VarDecl("ref", Mizar.constant("prop"))
      ),
    Context(VarDecl("notation", OMV("ref")))
  )

  val AntonymicNotation = MizPattern(LocalName("AntonymicNotation"),
    Context(
      VarDecl("nr"),
      VarDecl("args", Rep(Mizar.constant("tp"),OMV("nr"))),
      VarDecl("ref", Mizar.constant("prop"))
      ),
    Context(VarDecl("notation", Mizar.apply(Mizar.constant("not"),OMV("ref"))))
  )


  val Lemma = MizPattern(LocalName("Lemma"),
    Context(VarDecl("prop", Mizar.constant("prop"))),
    Context(VarDecl("lemma", Mizar.apply(Mizar.constant("proof"), OMV("prop")))))
}

