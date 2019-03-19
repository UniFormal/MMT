package info.kwarc.mmt.mathhub.library

import info.kwarc.mmt.api.web.{JSONBasedGraphServer, ServerRequest, ServerResponse}
import info.kwarc.mmt.mathhub.Server

trait GraphServer { this: Server =>

  // the the json-based extension
  private lazy val graphExtension = controller.extman.get(classOf[JSONBasedGraphServer]).head

  /**
    * Calls the graph backend, assuming we are on a graph sub-path
    * @return
    */
  protected def applyGraph(request: ServerRequest) : ServerResponse = {
    graphExtension.apply(request.copy(path=request.path.tail))
  }
}