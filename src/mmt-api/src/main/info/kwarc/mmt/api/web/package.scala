package info.kwarc.mmt.api

import web._
import frontend._

/**
  * [[Server]] maintains the HTTP interface of MMT.
  * The server is owned by the [[frontend.Controller]].
  * 
  * It can be customized by [[ServerExtension]]s.
  * 
  * The [[REPLServer]] maintains a set of independent REPL loops for MMT content.
  */
package object web {
}
