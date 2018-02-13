package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import utils._
import Javascript._

case class ContextMenuEntry(label: String, function: Expression)

trait ContextMenuProvider extends Extension {
  def getEntries(path: Path): List[ContextMenuEntry]
}

class ContextMenuAggregator extends ServerExtension("menu") {
  def apply(request: ServerRequest): ServerResponse = {
     val path = Path.parse(request.query)
     val providers = controller.extman.get(classOf[ContextMenuProvider])
     var errors: List[Error] = Nil
     val entries = providers.flatMap {p =>
       try {p.getEntries(path)}
       catch {case e: Exception =>
          errors ::= Error(e)
          Nil
       }
     }
     val errorEntry = if (errors.isEmpty) None else {
       val errorString = errors.map(e => e.toStringLong).mkString("\n\n")
       val errorAction = JSSeq(Log(errorString), Alert(errorString))
       Some(ContextMenuEntry("show " + errors.length + " errors", errorAction))
     }

     val json = JSONObject((entries:::errorEntry.toList).map(e => (JSONString(e.label), JSONString(e.function.toJS))))
    ServerResponse.JsonResponse(json)
  }
}

