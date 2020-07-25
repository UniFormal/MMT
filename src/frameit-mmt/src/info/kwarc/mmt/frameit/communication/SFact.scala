package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.frameit.communication.SimpleOMDoc.SDeclaration

sealed case class SFact(declaration: SDeclaration, label: String)
