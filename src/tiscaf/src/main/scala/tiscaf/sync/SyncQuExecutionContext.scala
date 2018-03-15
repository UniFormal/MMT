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
package sync

import scala.concurrent.{
  ExecutionContext,
  SyncVar
}

private object SyncQuExecutionContext {
  private var num : Int = 0
  def nextNum : Int = synchronized { val v = num; num += 1; v }
}

/** The simple execution context used  internally by tiscaf
 *  to execute its asynchronous  tasks
 *
 *  @author Lucas Satabin
 */
abstract class SyncQuExecutionContext(poolSize: Int, queueCap: Int, name: String) extends ExecutionContext with HLoggable {

  def execute(task: Runnable): Unit =
    if(working.get)
      qu.put(task)
    else
      error("submitting to shutted down SyncQuExecutionContext " + name, new RuntimeException)

  def reportFailure(t: Throwable): Unit =
    error("An error occurred in ExecutionContext " + name, t)

  def stopAccepting : Unit = {
    working.set(false) // doesn't prevent to drain the queue
  }

  def shutdown : Unit = {
    stopAccepting
    qu.close // closes the queue for both 'take' and 'put'
    cancelAll
  }

  //-------------------- internals ------------------------------

  private val working = new SyncBool(true)
  private val qu      = new SyncQu[Runnable](queueCap)
  private val poolNum = SyncQuExecutionContext.nextNum

  private val threads = for(i <- 0 until poolSize) yield {
    val t = newThread(i); t.start; t }

  private def newThread(i : Int) : Thread = new Thread {
    override def run = while (working.get) {
      try {
        qu.take.run
      } catch {
        case e : java.lang.InterruptedException => // ignore
        case e : Exception =>
          error("Some error occurred while processing thread " + name + "-" + poolNum + "-" + i, e)
      }
    }
    setName(name + "-" + poolNum + "-" + i)
  }

  private def cancelAll : Unit = threads.foreach { t =>
    try {
      t.interrupt
    } catch {
      case _: Exception =>
    }
  }

}

