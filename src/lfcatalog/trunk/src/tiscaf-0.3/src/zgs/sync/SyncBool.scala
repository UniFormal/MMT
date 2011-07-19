package zgs.sync


// Scala hasn't 'volatile' (if neglect annotations)

final class SyncBool(init : Boolean) {
  
  def set(newValue : Boolean) : Unit = synchronized { value = newValue }
  def get : Boolean = synchronized { value }
  
  //----- internals ---------
  
  private var value = init
}
