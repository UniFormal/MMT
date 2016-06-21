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

import java.io.RandomAccessFile

import scala.concurrent._
import ExecutionContext.Implicits.global

// doesn't catch IO exception as far as they will be delegated to HServer.onError()

protected class FiLet(path : String, bufSize : Int, plainAsDefault : Boolean) extends HSimpleLet {

  def act(tk : HTalk) {
    val f = new java.io.File(path)
    if (f.exists && f.isFile) {

      // deal with simplest Range: bytes=123-
      val shift = tk.req.header("Range") match {
        case None => 0L
        case Some(x) =>
          val range = x.toLowerCase.replace("bytes", "").replace("=", "")
          if (range.endsWith("-")) try {
            val tmp = range.substring(0, range.length - 1).trim.toLong
            if (tmp >= 0 && tmp < f.length) tmp else -1L
          } catch { case _: Exception => -1L }
          else -1L
      }

      shift match {
        case x if (x > 0) =>
          tk.setStatus(HStatus.PartialContent)
            .setHeader("Content-Range", "bytes " + shift + "-" + (f.length - 1) + "/" + f.length)
        case x if (x == 0) => // nothing additional
        case x if (x < 0)  => tk.setStatus(HStatus.RequestRangeNotSatisfiable)
      }

      if (shift >= 0) {
        tk.setHeader("Last-Modified", HResponse.stdDateString(f.lastModified))
          .setContentLength(f.length - shift)

        val cType = HMime.exts.keysIterator.find(ext => path.toLowerCase.endsWith("." + ext)) match {
          case Some(e) => HMime.exts(e)
          case None    => if (plainAsDefault) "text/plain" else "application/octet-stream"
        }
        tk.setContentType(cType)

        val ar = new Array[Byte](bufSize)
        val raf = new RandomAccessFile(f, "r")
        if (shift > 0) raf.seek(shift)

        @scala.annotation.tailrec
        def writeBuf : Unit = {
          val wasRead = raf.read(ar)
          if (wasRead > 0) {
            tk.write(ar, 0, wasRead)
            writeBuf
          }
        }
        writeBuf
        raf.close

      } // if (shift >= 0)
    } // if (f.exists && f.isFile)
    else new ErrLet(HStatus.NotFound) act (tk)
  }

}

