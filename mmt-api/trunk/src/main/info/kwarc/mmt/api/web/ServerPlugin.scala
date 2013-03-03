package info.kwarc.mmt.api.web
import info.kwarc.mmt.api.frontend._
import zgs.httpd._
import zgs.httpd.let._

trait ServerPlugin extends Extension {
  def isApplicable(uriComp : List[String]) : Boolean
  
  def apply(uriComps : List[String]) : Option[HLet] 
}