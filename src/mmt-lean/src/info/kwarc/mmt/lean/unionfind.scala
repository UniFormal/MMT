package info.kwarc.mmt.lean

class UFNode {
  var parent: UFNode = this
  var rank = 0

  def find(): UFNode = {
    if (parent.parent eq parent) return parent
    parent = parent.find()
    parent
  }

  def union(that: UFNode): Unit = {
    val a = this.find()
    val b = that.find()

    if (a eq b) return

    if (a.rank < b.rank) {
      a.parent = b
      if (a.rank == b.rank) b.rank += 1
    } else {
      b.parent = a
      if (a.rank == b.rank) a.rank += 1
    }
  }
}