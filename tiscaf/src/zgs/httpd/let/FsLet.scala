package zgs.httpd.let

import java.io.File


private object FsLet {
  val stdIndexes = List("index.html", "index.htm")
}

import zgs.httpd._
import FsLet._

trait FsLet extends HLet {
  
  //----------------- to implement -------------------------
  
  protected def dirRoot : String // will be mounted to uriRoot

  //----------------- to override -------------------------
  
  protected def uriRoot : String         = "" // say, "myKit/theDir"
  protected def indexes : Seq[String]    = stdIndexes 
  protected def allowLs : Boolean        = false
  protected def bufSize : Int            = 4096
  protected def plainAsDefault : Boolean = false
  
  //-------------------- init ------------------------------
  
  private val theDirRoot : String = {
    val tmp = (new File(dirRoot)).getCanonicalPath.replaceAll("\\\\", "/") 
    if (tmp.endsWith("/")) tmp else tmp + "/"
  }
  
  private val theUriRoot = { // remove leading and trailing slashes
    val anUri = uriRoot
    val tmp = if (anUri.startsWith("/")) anUri.substring(1) else uriRoot
    if (tmp.endsWith("/")) tmp.substring(0, tmp.length - 1) else tmp
  }
  
  //------------------ HLet implemented --------------------
  
  def act(tk : HTalk) = if ((tk.req.uriPath).startsWith(theUriRoot)) {
    val uriExt = if (tk.req.uriExt.isDefined) { ";" + tk.req.uriExt.get } else ""
    
    val pathRest =  tk.req.uriPath.substring(theUriRoot.length)
    val path = theDirRoot + { if (pathRest.startsWith("/")) pathRest.substring(1) else pathRest }
    val f = new File(path)
    if ((f.getCanonicalPath.replaceAll("\\\\", "/") + "/").startsWith(theDirRoot) && f.exists) {
      if (f.isDirectory) {
        if (theUriRoot == "") {
          if (allowLs) {
            val dirLet = new DirLet(theDirRoot, theUriRoot, pathRest) {
              override def before = FsLet.this.before
            }
            delegate(dirLet, tk)
          } 
          else notFound(tk)
        } 
        else if (tk.req.uriPath.endsWith("/")) {
          indexes.find { index => 
            val p = path + index
            val i = new File(p)
            i.exists && i.isFile
          } match {
            case None    => 
              if (allowLs) {
                val dirLet = new DirLet(theDirRoot, theUriRoot, pathRest) {
                  override def before = FsLet.this.before
                } 
                delegate(dirLet, tk)
              } 
              else notFound(tk) 
            case Some(x) => 
              val fiLet = new FiLet(path + x, bufSize, plainAsDefault) {
                override def before = FsLet.this.before
              }
              delegate(fiLet, tk)
          }
        }
        else new RedirectLet("/" + theUriRoot + pathRest + "/" + uriExt) act(tk)
      }
      else {
        val fiLet = new FiLet(path, bufSize, plainAsDefault) {
          override def before = FsLet.this.before
        } 
        delegate(fiLet, tk)
      }
    } else notFound(tk)
  }
  else notFound(tk)
  
  private def notFound(tk : HTalk) = new ErrLet(HStatus.NotFound) act(tk)

}
