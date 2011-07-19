package zgs.httpd

import scala.collection. { mutable => mute }

final private class HSess(app : HApp, conn : HConn, resp : HResponse)
  extends scala.collection.mutable.Map[Any,Any] {

  // scala.collection.mutable.Map ------------------------------------
  def get(key: Any): Option[Any]     = session._2.get(key)
  def iterator: Iterator[(Any, Any)] = session._2.iterator
  def += (kv: (Any, Any)): this.type = { session._2 += kv; this }
  def -= (key: Any): this.type       = { session._2 -= key; this }
  override def size : Int            = session._2.size
  override def clear : Unit          = session._2.clear
  override def foreach[U](f : Tuple2[Any,Any] => U) : Unit = session._2.foreach[U] { t => f(t) }

  //---------------------- delegating here from HTalk user API ---------

  def sid : String = session._1

  //----------------------- SPI ---------------------
  
  def writeSessionCookie : Unit = {
    def writeIt = resp.setHeader("Set-Cookie", HApp.cookieKey + "=" + session._1 + "; path=/")
    app.tracking match {
      case HTracking.Cookie   => writeIt
      case _                      =>
    }
  }  

  def restamp    = HSess.restamp(app, session._1)
  def isValid    = HSess.isValid(app, session._1)
  def invalidate = HSess.invalidate(app, session._1)

  //-------------------- internals ---------------------------------------------
  
  private lazy val session : (String, scala.collection.mutable.Map[Any,Any]) = extractSid match {
    case None    => HSess.create(app)
    case Some(x) => HSess.bag(app, x) match { // extracted sid may be invalidated already
      case None      => HSess.create(app)
      case Some(bag) => (x, bag)
    }
  }
  
 // uri has higher priority rather cookie
  private def extractSid : Option[String] = app.tracking match {
    case HTracking.Uri      => extractSidFromUri
    case HTracking.Cookie   => extractSidFromCookie
    case _ => None
  }
  
  private def extractSidFromUri : Option[String] = conn.headerData.uriExt.flatMap { x =>
    val parts = x.split("=", 2)
    if (parts.size == 2 && parts(0) == HApp.sidKey && HSess.sidIsValid(parts(1))) Some(parts(1))
    else None
  }
  
  private def extractSidFromCookie : Option[String] = conn.headerData.header("cookie").flatMap { x =>
    val sids = for {
      pair <- x.split(";", 2)
      val parts = pair.split("=", 2)
      if (parts.size == 2) && parts(0).trim == HApp.cookieKey && HSess.sidIsValid(parts(1).trim)
    } 
    yield parts(1).trim
          
    if (sids.size > 0) Some(sids(0)) else None // browser can keep few cookies with the same NAME in
                                                // NAME=VALUE pairs; take first one
  }
  
}


private object HSess {
  
  def create(app : HApp) : (String,mute.Map[Any,Any]) = 
    withApp(app, _.create, { val aS = new HAppSess; bags(app) = aS; aS.create }) 
    
  def bag(app : HApp, sid : String) : Option[mute.Map[Any,Any]] = withApp(app, _.bag(sid), None) 
  def restamp(app : HApp, sid : String) : Unit                  = withApp(app, _.restamp(sid), None)
  def isValid(app : HApp, sid : String) : Boolean               = withApp(app, _.isValid(sid), false)
  def invalidate(app : HApp, sid : String) : Unit               = withApp(app, _.invalidate(sid), {})
  def count(app : HApp) : Int                                   = withApp(app, _.size, 0)
  
  //----------------- internals --------------------
  
  private val alpha = "abcdefghijklmnopqrstuvwxyz"
  private val symbols = alpha + alpha.toUpperCase + "0123456789"
  private val symLength = symbols.length
  private val idLength = 16
  private val random = new java.security.SecureRandom
  
  private def withApp[T](app: HApp, some : HAppSess => T, none : => T) : T = 
    bags.synchronized { bags.get(app) match { case None => none; case Some(aS) => some(aS) }}
  
  private def newSid : String = synchronized {
    val buf = new StringBuilder
    (1 to idLength).foreach(_ => buf.append(symbols.charAt(random.nextInt(symLength))))
    buf.toString
  }
  
  private def sidIsValid(sid : String) : Boolean = 
    if (idLength != sid.length) false
    else sid.toSet subsetOf symbols.toSet
  
  private def sidKey = "sid"
  private def cookieKey = "TISCAF_SESSIONID"
  
  private val bags = new mute.HashMap[HApp,HAppSess] // hand-written synchronization
    
  private val millisInMin : Long = 60000L
  private def cleanExpired = bags.synchronized {
    bags.keysIterator.foreach(app => bags(app).cleanExpired(app.sessionTimeoutMinutes * millisInMin))
  } 
  
  new java.util.Timer(true) // daemon
    .scheduleAtFixedRate(
      new java.util.TimerTask { def run { cleanExpired } },
      15000, 15000 // every 15 seconds 
    )
  
  //--------------------------------------------
  
  private class HAppSess {
    
    type StampBag = (Long,mute.Map[Any,Any])
    
    def create : (String,mute.Map[Any,Any]) = {
      val sidMap = new mute.HashMap[Any,Any] with mute.SynchronizedMap[Any,Any]
      val sid = newSid
      appBags.synchronized {
        appBags(sid) = (now, sidMap)
        (sid, sidMap)
      }
    }
  
    def bag(sid : String) : Option[mute.Map[Any,Any]] = withApp(sid, { bag => Some(bag._2) }, None)
      
    def restamp(sid : String) : Unit    = withApp(sid, { bag => appBags(sid) = (now, bag._2) }, {})
    def isValid(sid : String) : Boolean = withApp(sid, _ => true, false)
    def invalidate(sid : String) : Unit = appBags.synchronized { appBags -= sid }
    def size : Int                      = appBags.synchronized { appBags.size }
    
    def cleanExpired(timeout : Long) : Unit = appBags.synchronized {
      val timeX = now - timeout
      appBags.keysIterator.filter(sid => appBags(sid)._1 < timeX).foreach(sid =>  appBags -= sid)
    }
    
    //--------------
    
    private val appBags : mute.Map[String,(Long,mute.Map[Any,Any])] = new mute.HashMap[String,(Long,mute.Map[Any,Any])]
    private def now = System.currentTimeMillis
    
    private def withApp[T](sid: String, some : StampBag => T, none : => T) : T = 
      appBags.synchronized { appBags.get(sid) match { case None => none; case Some(tb) => some(tb) }}
  }
  
}
