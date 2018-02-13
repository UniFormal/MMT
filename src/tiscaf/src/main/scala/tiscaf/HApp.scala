/** *****************************************************************************
 *  This file is part of tiscaf.
 *
 *  tiscaf is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Foobar is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 *  ****************************************************************************
 */
package tiscaf

import scala.collection.{ mutable => mute }

object HTracking extends Enumeration {
  val NotAllowed, Uri, Cookie = Value
}

/** An application is a group of request handlers ([[tiscaf.HLet]]s)
 *  sharing common behaviour.
 */
trait HApp {
  //----------------------- to implement -------------------------

  /** Returns a handler to process the request.
   *  To decide which handler to use, you have full request information
   *  available via `HReqData`.
   *  @see [[tiscaf.HReqData]]
   */
  def resolve(req: HReqData): Option[HLet]

  //----------------------- to override ---------------------------

  /** Tracking method for sessions. By default, sessions are not tracked.
   *  @see [[tiscaf.HTracking]]
   */
  def tracking: HTracking.Value = HTracking.NotAllowed

  /** Session timeout in minutes. By default 15 minutes. */
  def sessionTimeoutMinutes: Int = 15

  /** Maximum sessions count allowed simultaneously on the server.
   *  By default 500.
   */
  def maxSessionsCount: Int = 500

  /** Keep-Alive TCP connections. By default `true`. */
  def keepAlive: Boolean = true

  /** Chunked response. By default `false`. */
  def chunked: Boolean = false

  /** Buffered response. By default `false`. */
  def buffered: Boolean = false

  /** Gzipped response (for compatible mime types). By default `false`. */
  def gzip: Boolean = false

  /** Response encoding. By default `UTF-8`. */
  def encoding: String = "UTF-8"

  /** Session cookie name (if cookie tracking enabled).
   *  By default `TISCAF_SESSIONID`.
   */
  def cookieKey: String = "TISCAF_SESSIONID"

  /** Session ID key name (if URL tracking enabled).
   *  By default `sid`.
   */
  def sidKey: String = "sid"

  /** Called when the given session is invalidated.
   *  Override this to perform specific actions on invalidate.
   *  Does nothing by default.
   */
  def onSessionInvalidate(sid: String, data: Map[Any, Any]): Unit = {}

}
