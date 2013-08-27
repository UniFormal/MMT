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

// SyncVar-like, but closable

final class SyncField[T] {
 
  //----------------------------- API -------------------
    
  def put(item : T) : Unit = synchronized {
    require(!closed, "putting to closed SyncField")
    while (value.isDefined) wait()
    value = Some(item)
    notifyAll()
  }
  
  def set(item : T) : Unit = synchronized {
    require(!closed, "setting closed SyncField")
    value = Some(item)
    notifyAll()
  }
    
  def take : T = synchronized {
    require(!closed, "taking closed SyncField")
    while (value.isEmpty) wait()
    val item = value.get
    value = None
    notifyAll()
    item
  }
  
  def eat : Unit = synchronized {
    require(!closed, "eating closed SyncField")
    while (value.isEmpty) wait()
    value = None
    notifyAll()
  }
  
  def get : T = synchronized {
    require(!closed, "getting closed SyncField")
    while (value.isEmpty) wait()
    value.get
  }
  
  def isDefined : Boolean = synchronized { value.isDefined }
  
  def close : Unit = synchronized { 
    if (!closed) {
      closed  = true
      value = None
      notifyAll() // to arise an error for waiters
    } 
  }
  
  //---------------------- internal ---------------------------
  
  private var value : Option[T] = None
  private var closed            = false
}
