package zgs.sync

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
