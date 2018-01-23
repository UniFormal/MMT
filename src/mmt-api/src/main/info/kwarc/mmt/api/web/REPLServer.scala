package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import documents._
import modules._
import parser._
import utils._

/** stores the state of a content-inputing REPL session */ 
class REPLSession(val path: DPath, val id: String) {
  override def toString = s"session with id $id and URI $path" 
  private val doc = new Document(path, true)
  private var scope: ParentInfo = IsDoc(doc.path)
}

import ServerResponse._

class REPLServer extends ServerExtension("repl") {
  private var sessions: List[REPLSession] = Nil
  
  def apply(request: ServerRequest): ServerResponse = {
    val id = request.headers.get("mmtsession").getOrElse {
      throw LocalError("no mmtsession header")
    }
    val currentSessionOpt = sessions.find(_.id == id)
    val command = request.query
    command match {
      case "start" =>
        if (currentSessionOpt.nonEmpty) {
          throw LocalError("session id already exists")
        }
        val path = DPath(mmt.baseURI) / "jupyter" / id
        val s = new REPLSession(path, id)
        sessions ::= s
        TextResponse("new session started with id " + id)
      case "quit" =>
        currentSessionOpt match {
          case None =>
            TextResponse("session did not exist")
          case Some(s) =>
            sessions = sessions.filterNot(_ == s)
            TextResponse("session terminated with id " + id)
        }
      case _ =>
        val currentSession = currentSessionOpt.getOrElse {
          throw LocalError("unknown session")
        }
        val input = request.body.asString
        TextResponse(s"this is what you wrote: $input (your current session is $currentSession")
    }
  }
}