package info.kwarc.mmt.mathhub.library.Context

import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.mathhub.library.Context.Builders._


/** A caching API that can resolve api items */
class MathHubAPIContext(val controller: Controller, val report: Report) extends Logger with Builder {
  def logPrefix: String = "mathhub"
}