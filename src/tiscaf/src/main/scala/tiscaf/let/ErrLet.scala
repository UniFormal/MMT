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

/** Always send the given error as response. */
class ErrLet(status: HStatus.Value, msg: String = "") extends HSimpleLet {

  def act(tk: HTalk) {
    val add = if (msg.length == 0) "" else ", " + msg
    val toWrite = (HStatus.asString(status) + add + "\n").getBytes("ISO-8859-1")

    tk.setStatus(status)
      .setContentType("text/plain")
      .setContentLength(toWrite.length) // if not chunked
      .write(toWrite)

    tk.ses.invalidate
  }
}
