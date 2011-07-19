package zgs.httpd.let

import zgs.httpd._

trait ResourceLet  extends HLet {

  //----------------- to implement -------------------------

  protected def dirRoot : String // will be mounted to uriRoot

  //----------------- to override -------------------------

  protected def getResource(path : String) : java.io.InputStream =
    this.getClass.getResourceAsStream(path)
  protected def uriRoot : String         = "" // say, "myKit/theDir"
  protected def indexes : Seq[String]    = Nil
  protected def bufSize : Int            = 4096
  protected def plainAsDefault : Boolean = false

  //-------------------- init ------------------------------

  // starts with anf ends with "/"
  private val theDirRoot : String = "/" + dirRoot + { if (dirRoot.endsWith("/")) "" else "/" }

  private val theUriRoot = { // remove leading and trailing "/"
    val anUri = uriRoot
    val tmp = if (anUri.startsWith("/")) anUri.substring(1) else uriRoot
    if (tmp.endsWith("/")) tmp.substring(0, tmp.length - 1) else tmp
  }

  private def resolvePath(tk : HTalk) : String = {
    val uriExt = if (tk.req.uriExt.isDefined) { ";" + tk.req.uriExt.get } else ""
    val pathRest =  tk.req.uriPath.substring(theUriRoot.length)
    val path = theDirRoot + { if (pathRest.startsWith("/")) pathRest.substring(1) else pathRest }
    new java.io.File(path).getCanonicalPath
  }
  
  //------------------ HLet implemented --------------------

  def act(tk : HTalk) = if ((tk.req.uriPath).startsWith(theUriRoot)) {
    val path = resolvePath(tk)
    val io = ("" :: indexes.toList)
      .view
      .map(idx => getResource((path + { if (idx.length == 0) "" else "/" + idx}).replace("//", "/")))
      .find(_ != null)

    if (io.isEmpty) notFound(tk) else {
      val cType = HMime.exts.keysIterator.find(ext => path.toLowerCase.endsWith("." + ext)) match {
        case Some(e) => HMime.exts(e)
        case None    => if (plainAsDefault) "text/plain" else "application/octet-stream"
      }
      tk.setContentType(cType)

      val ar = new Array[Byte](bufSize)
      val is = io.get
      
      @scala.annotation.tailrec
      def step(wasRead : Int) : Unit = if (wasRead > 0) {
        tk.write(ar, 0, wasRead)
        step(is.read(ar))
      }
      step(is.read(ar))
      is.close
      tk.close
    }
  }
  else notFound(tk)

  private def notFound(tk : HTalk) = new ErrLet(HStatus.NotFound, tk.req.uriPath) act(tk)
}
