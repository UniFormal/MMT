package zgs.sync

final class SynQu[T](cap : Int = Int.MaxValue) {

  //----------------------------- API -------------------

  def put(item : T) : Unit = synchronized {
    require(!closed, "putting to closed SynQu")
    require(theSize < cap, "putting to filled in SynQu")
    if (theSize == 0) {
      first = Some(new Node(item))
      last = first
      theSize = 1
    }
    else {
      last.get.next = Some(new Node(item))
      last = last.get.next
      theSize += 1
    }
    notifyAll()
  }

  def take : T = synchronized {
    require(!closed, "taking from closed SynQu")
    while (theSize == 0) wait()
    val result = first.get
    first = result.next
    result.next = None // deref
    theSize -= 1
    if (first.isEmpty) last = None
    notifyAll()
    result.value
  }

  def close : Unit = synchronized {
    closed = true
    // deref all
    while (first.isDefined) {
      val second = first.get.next
      first.get.next = None
      first = second
    }
    last = None
    theSize = 0
    notifyAll() // to rise error for waiters
  }

  def isEmpty : Boolean  = synchronized { first.isEmpty }
  def isClosed : Boolean = synchronized { closed }
  def size : Int         = synchronized { theSize }

  //----------------- internals --------------------------

  private var closed  = false
  private var theSize = 0

  private var first : Option[Node] = None
  private var last : Option[Node]  = None

  private class Node(val value : T)  {
    var next : Option[Node] = None
  }
}

