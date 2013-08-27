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

/** Redirect (send a moved permanently code) to a fix URI. */
class RedirectLet(toUri: String) extends HSimpleLet {

  def act(tk: HTalk) {
    tk.setContentLength(0)
      .setContentType("text/html")
      .setHeader("Location", toUri)
      .setStatus(HStatus.MovedPermanently)
  }

}
