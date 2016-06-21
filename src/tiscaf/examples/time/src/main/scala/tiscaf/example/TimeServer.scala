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
package example

import java.util.Date

object TimeServer extends HServer with App {

  def apps = Seq(TimeApp, StaticApp)

  def ports = Set(8080)

  // do not start the stop thread
  override protected def startStopListener { }

  start

  println("press enter to stop...")
  Console.readLine

  stop
}

/** The application that serves the pages */
object TimeApp extends HApp {

  def resolve(req: HReqData): Option[HLet] = req.uriPath match {
    case "currenttime" => Some(TimeLet)
    case _             => None
  }

}

/** Serves the current server time */
object TimeLet extends HSimpleLet {

  def act(talk: HTalk) {
    // simply return the current server time
    val time = new Date().getTime.toString
    talk.setContentLength(time.length)
      .write(time)
  }

}

/** Simply servers the resource from the classpath */
object StaticApp extends HApp {

  override def buffered : Boolean  = true // ResourceLet needs buffered or chunked be set

  def resolve(req: HReqData) = Some(StaticLet) // generates 404 if resource not found
}

object StaticLet extends let.ResourceLet {
  protected def dirRoot          = ""
  override protected def uriRoot = ""
  override protected def indexes = List("index.html")
}
