package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import web._

/** :notebook?THEORY returns a Jupyter notebook with a fresh theory including THEORY */
class NotebookGenerator extends ServerExtension("notebook") {
  def apply(request: ServerRequest) = {
     val p = Path.parse(request.query)
     val nb = p.dropComp match {
       case p: DPath =>
         MMTNotebook.empty
       case p: ContentPath =>
         MMTNotebook.inTheory(None, p.module)
     } 
     ServerResponse.JsonResponse(nb.toJSON)
  }
}