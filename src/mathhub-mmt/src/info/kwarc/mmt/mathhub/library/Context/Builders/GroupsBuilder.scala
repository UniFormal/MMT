package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.mathhub.library.IGroupRef

trait GroupsBuilder { this: Builder =>
  def getGroups() : List[IGroupRef] = {
    val mh = mathHub
    mh.entries_
      .collect({case ae: mh.MathHubGroupEntry => ae.group }).distinct
      .flatMap(e => getGroupRef(e))
  }
}
