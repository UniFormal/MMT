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
package tiscaf.sync

object Sync {
  
  def spawn(code: => Unit): Unit =
    new Thread(new Runnable { def run: Unit = { code } }) start
  
  def spawnNamed(name: String)(code: => Unit) : Unit = {
    val t = new Thread(new Runnable { def run: Unit = { code } }) 
    t setName name
    t start
  }
  
  def daemon(code: => Unit): Unit = {
    val t = new Thread(new Runnable { def run: Unit = { code } })
    t setDaemon true
    t start
  }
  
  def daemonNamed(name: String)(code: => Unit) : Unit = {
    val t = new Thread(new Runnable { def run: Unit = { code } })
    t setName name
    t setDaemon true
    t start
  }
  
  def join(code: => Unit): Unit = {
    val t = new Thread(new Runnable { def run: Unit = { code } })
    t.start
    t.join
  }
  
}
