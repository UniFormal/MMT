package zgs.httpd

import scala.collection.mutable._

object HTracking extends Enumeration { 
  val NotAllowed, Uri, Cookie = Value
}

trait HApp {
  //----------------------- to implement -------------------------
  def resolve(req : HReqHeaderData) : Option[HLet]
  
  //----------------------- to override ---------------------------
  def tracking : HTracking.Value  = HTracking.NotAllowed
  def sessionTimeoutMinutes : Int = 15
  def maxSessionsCount : Int      = 500
  def keepAlive : Boolean         = false
  def chunked : Boolean           = false
  def buffered : Boolean          = false 
  def gzip : Boolean              = false
  def paramsEncoding : String     = "UTF-8"
}

protected object HApp {
  def sidKey    = "sid"
  def cookieKey = "TISCAF_SESSIONID"
}
