package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.frameit.archives.Archives.FrameWorld.MetaKeys
import info.kwarc.mmt.frameit.archives.Foundation.StringLiterals
import info.kwarc.mmt.frameit.communication.{SFact, SOMDoc}

sealed case class Fact(declaration: Declaration, label: String) {
  def simplified: SFact = SFact(SOMDoc.OMDocBridge.encode(declaration), label)
}

object Fact {
  def parseFromDeclaration(decl: Declaration): Fact = {
    decl.metadata.get(MetaKeys.factLabel) match {
      // fall back to declaration name as
      case Nil => Fact(decl, decl.name.toString)

      case MetaDatum(_, StringLiterals(label)) :: Nil => Fact(decl, label)
      case _ => throw InvalidMetaData(s"Fact declaration contained an invalid label annotation or multiple label annotations, declaration path was: ${decl.path}")
    }
  }
}
