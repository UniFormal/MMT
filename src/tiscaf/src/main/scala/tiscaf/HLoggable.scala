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

/** The tiscaf logging API, used by tiscaf itself to log internal information.
 *  It is not intended to be used by applications using tiscaf but allows for people
 *  to change the implementation by their favorite logging API.
 *  By default, messages are logged to stdout and stderr.
 *
 *  The method names used do not imply that, let's say, a warning from the server internals must
 *  be a warning in the application logic. It may simply be ignored in production code and be
 *  treated as a debug message (for example, the post data was too big).
 *  The semantics of `info`, `wraning` and `error` is from the Http(s) connection and
 *  request handling point of view, not necessarily from an applicative point of view.
 *
 *  @author Lucas Satabin
 */
trait HLoggable {

  def info(msg: String): Unit

  def warning(msg: String): Unit

  def error(msg: String, t: Throwable): Unit

}
