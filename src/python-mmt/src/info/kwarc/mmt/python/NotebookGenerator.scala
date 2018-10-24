package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import web._

/** :notebook?THEORY returns a Jupyter notebook with a fresh theory including THEORY */
class NotebookGenerator extends ServerExtension("notebook") {
  def apply(request: ServerRequest) = {
     val p = Path.parse(request.query)
     p match {
       case p: MPath =>
         val nb = MMTNotebook.inTheory(None, p)
         ServerResponse.JsonResponse(nb.toJSON)
     } 
  }
}