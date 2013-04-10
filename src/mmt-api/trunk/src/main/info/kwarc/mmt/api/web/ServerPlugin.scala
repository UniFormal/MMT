package info.kwarc.mmt.api.web
import info.kwarc.mmt.api.frontend._
import zgs.httpd._
import zgs.httpd.let._

/**
 * A ServerPlugin can be added to MMT's HTTP server
 * 
 * It will be called on URIs of the form http://server:port/:CONTEXT/PATH
 */
trait ServerPlugin extends Extension {
  /**
   * @param cont the CONTEXT from above
   * @return true if this wants to serve the URL
   */
  def isApplicable(cont : String) : Boolean
  /**
   * @param path the PATH from above
   * @return the HTTP response
   */
  def apply(path: List[String]) : Option[HLet] 
}