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

import scala.concurrent._
import ExecutionContext.Implicits.global

/** Serve static resources that are present in the classpath*/
trait ResourceLet extends HSimpleLet {

  //----------------- to implement -------------------------

  /** The root directory that will be mounted to the root URI. */
  protected def dirRoot: String // will be mounted to uriRoot

  //----------------- to override -------------------------

  /** Returns the resource associated to the given path.
   *  Following Java convention: returns null if resource is not found.
   *  We must return null for directories in jars. Official API doesn't provide any
   *  way to differentiate files and directories in jars. But stream.available()
   *  rises an error for directories.
   */
  protected def getResource(path: String): java.io.InputStream = {
    val url = this.getClass.getResource(path)
    if (url == null) null else url.getProtocol match {
      case "jar"  =>
        val is = url.openStream
        try {
        is.available
        is
      } catch {
        case _: Exception =>
          if(is != null)
            is.close
          null
      }
      case "file" => if (new java.io.File(url.toURI) isFile) url.openStream else null
      case _      => null
    }
  }

  /** The root URI. */
  protected def uriRoot: String = "" // say, "myKit/theDir"

  /** Index files if no file if given. By default, none. */
  protected def indexes: Seq[String] = Nil // say, List("index.html", "index.htm")

  /** Buffer size. By default `4096`. */
  protected def bufSize: Int = 4096

  /** Indicates whether the default mime type is `text/plain` if none could be
   *  recognized by the server (from the file extension).
   *  If this is `false`, the default mime type will be `application/octet-stream`.
   *  By default, `false`.
   */
  protected def plainAsDefault: Boolean = false

  //-------------------- init ------------------------------

  // starts and ends with "/"
  private val theDirRoot: String = "/" + dirRoot + { if (dirRoot.endsWith("/") || dirRoot.isEmpty) "" else "/" }

  private val theUriRoot = { // remove leading and trailing "/"
    val anUri = uriRoot
    val tmp = if (anUri.startsWith("/")) anUri.substring(1) else uriRoot
    if (tmp.endsWith("/")) tmp.substring(0, tmp.length - 1) else tmp
  }

  private def resolvePath(tk: HTalk): String = {
    val pathRest = tk.req.uriPath.substring(theUriRoot.length)
    def path = theDirRoot + { if (pathRest.startsWith("/")) pathRest.substring(1) else pathRest }
    java.net.URI.create(path).normalize.getPath
  }

  //------------------ HLet implemented --------------------

  def act(tk: HTalk) {
    if ((tk.req.uriPath).startsWith(theUriRoot)) {
      val path = resolvePath(tk)

      val fullPathAndStream = ("" +: indexes)
        .map(idx => (path + { if (idx.length == 0) "" else "/" + idx }).replace("//", "/"))
        .map { fullPath => (fullPath, getResource(fullPath)) }
        .find(_._2 != null)

      if (fullPathAndStream.isEmpty) notFound(tk) else {
        def cType = HMime.exts.keySet.find(ext => fullPathAndStream.get._1.toLowerCase.endsWith("." + ext)) match {
          case Some(e) => HMime.exts(e)
          case None    => if (plainAsDefault) "text/plain" else "application/octet-stream"
        }
        tk.setContentType(cType)

        val ar = new Array[Byte](bufSize)
        val is = fullPathAndStream.get._2

        @scala.annotation.tailrec
        def step(wasRead: Int): Unit = if (wasRead > 0) {
          tk.write(ar, 0, wasRead)
          step(is.read(ar))
        }
        step(is.read(ar))
        is.close
      }
    } else notFound(tk)
  }

  private def notFound(tk: HTalk) = new ErrLet(HStatus.NotFound, tk.req.uriPath) act (tk)
}
