/*******************************************************************************
 * This file is part of tiscaf.
 * 
 * tiscaf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Foobar is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package tiscaf
package let

import java.io.File

import scala.concurrent._
import ExecutionContext.Implicits.global

/** Simply serve static content from the file system. */
trait FsLet extends HSimpleLet {

  //----------------- to implement -------------------------

  /** The root directory that will be mounted to `uriRoot`. */
  protected def dirRoot: String // will be mounted to uriRoot

  //----------------- to override -------------------------

  /** The root URI. */
  protected def uriRoot: String = "" // say, "myKit/theDir"

  /** Index files if no file if given. By default, none. */
  protected def indexes: Seq[String] = Nil // say, List("index.html", "index.htm")

  /** Is directory listing allowed. By default, `false`. */
  protected def allowLs: Boolean = false

  /** Buffer size. By default `4096`. */
  protected def bufSize: Int = 4096

  /** Indicates whether the default mime type is `text/plain` if none could be
   *  recognized by the server (from the file extension).
   *  If this is `false`, the default mime type will be `application/octet-stream`.
   *  By default, `false`.
   */
  protected def plainAsDefault: Boolean = false

  //-------------------- init ------------------------------

  // force trailing slash
  private val theDirRoot: String = {
    val tmp = (new File(dirRoot)).getCanonicalPath.replace("\\\\", "/").replace("\\","/")
    if (tmp.endsWith("/")) tmp else tmp + "/"
  }
  // remove leading and trailing slashes
  private val theUriRoot = {
    val tmp = if (uriRoot.startsWith("/")) uriRoot.substring(1) else uriRoot
    if (tmp.endsWith("/")) tmp.substring(0, tmp.length - 1) else tmp
  }

  //------------------ HLet implemented --------------------

  def act(tk: HTalk) {
      if ((tk.req.uriPath).startsWith(theUriRoot)) {
      val uriExt = if (tk.req.uriExt.isDefined) { ";" + tk.req.uriExt.get } else ""

      val pathRest = tk.req.uriPath.substring(theUriRoot.length)
      val path = theDirRoot + { if (pathRest.startsWith("/")) pathRest.substring(1) else pathRest }
      val f = new File(path)

      if ((f.getCanonicalPath.replace("\\\\", "/").replace("\\","/") + "/").startsWith(theDirRoot) && f.exists) {
        if (f.isDirectory) {
          // try indexes first - before direcory listing
          if (theUriRoot.isEmpty || tk.req.uriPath.endsWith("/")) indexes.find { index =>
            val indexFile = new File(path + index)
            indexFile.exists && indexFile.isFile
          } match {
            case None    => if (allowLs) new DirLet(theDirRoot, theUriRoot, pathRest).act(tk) else notFound(tk)
            case Some(x) => new FiLet(path + x, bufSize, plainAsDefault).act(tk)
          }
          else new RedirectLet("/" + theUriRoot + pathRest + "/" + uriExt) act (tk)
        } // isDirectory
        else new FiLet(path, bufSize, plainAsDefault).act(tk)
      } else notFound(tk)
    } else notFound(tk)
  }

  private def notFound(tk: HTalk) = new ErrLet(HStatus.NotFound) act (tk)
}
