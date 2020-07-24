package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.frameit.business.Scroll
import info.kwarc.mmt.frameit.communication.SimpleOMDoc.SDeclaration

case class SScroll(problemTheory: SimpleOMDoc.SURI, solutionTheory: SimpleOMDoc.SURI, label: String, description: String, declarations: List[SDeclaration])

object SScroll {
  def fromScroll(scroll: Scroll): SScroll = SScroll(
    scroll.problemTheory.toString,
    scroll.solutionTheory.toString,
    scroll.label,
    scroll.description,
    scroll.declarations.map(SimpleOMDoc.OMDocBridge.encode)
  )
}

case class SScrollApplication(scroll: SScroll, view: String)
