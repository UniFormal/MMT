package info.kwarc.mmt.api.utils

import scala.collection.mutable._

/** a hash set that uses a complex (possibly infinite) type as hash values */
class ComplexHashSet[H, A](hash: A => H) extends Set[A] {
  private var elements = new HashMap[H,Set[A]]()
  private def bucket(h: H) = elements.getOrElseUpdate(h, Set.empty)

  override def clear(): Unit = elements.clear()

  def addOne(a: A) = {bucket(hash(a)) += a; this}
  def subtractOne(a: A) = {bucket(hash(a)) -= a; this}
  def contains(a: A) = bucket(hash(a)) contains a
  def iterator = elements.valuesIterator.flatten

  override def empty = new ComplexHashSet[H,A](hash)
  override def foreach[U](f: A => U): Unit = {
    elements.valuesIterator foreach {s => s foreach f}
  }
}
