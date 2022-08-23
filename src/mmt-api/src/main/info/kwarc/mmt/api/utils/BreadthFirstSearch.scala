package info.kwarc.mmt.api.utils

import scala.collection.mutable

/**
  * Utils for breadth-first-search (BFS)
  */
@MMT_TODO("only used in diagrams, should probably be moved/deleted")
object BreadthFirstSearch {
  /**
    * Collect elements in a BFS manner from a bounded set.
    *
    * @param all      The set of all elements.
    * @param initial  The initial set to start collecting with.
    * @param explorer A function exploring all possible elements from one element.
    *                 It is called like `explorer(elem, collectionSoFar, remainingElems)`.
    *
    *                 You can use `remainingElems` to optimize your algorithm. E.g. if you needed to
    *                 search through `all` previously, you can just search through `remainingElems`,
    *                 since all other elements are already part of the collection. Thus readding them
    *                 is a no-op.
    *
    *                 Invariant: `remainingElems == all - collectionSoFar && remainingElems.contains(elem)`.
    * @tparam T The type of elements.
    * @return All collected elements.
    */
  def collectBounded[T](all: Set[T], initial: Seq[T], explorer: (T, Set[T], Set[T]) => Set[T]): Set[T] = {
    val queue = mutable.Queue[T](initial: _*)
    val collection = mutable.HashSet[T](initial : _*)
    val remaining = mutable.HashSet[T](all.toSeq: _*)

    while (queue.nonEmpty) {
      val newNodes = explorer(queue.dequeue(), collection.toSet, remaining.toSet).diff(collection.toSet)

      queue.enqueueAll(newNodes)
      collection ++= newNodes
      remaining --= newNodes
    }

    collection.toSet
  }

  /**
    * Collect elements in a BFS manner.
    *
    * @param initial  The initial set to start collecting with.
    * @param explorer A function exploring all possible elements from one element.
    *                 It is called like `explorer(elem, collectionSoFar)`.
    * @tparam T The type of elements.
    * @return All collected elements.
    */
  def collect[T](initial: Seq[T], explorer: (T, Set[T]) => Set[T]): Set[T] = {
    val queue = mutable.Queue[T](initial: _*)
    val collection = mutable.HashSet[T](initial : _*)
    while (queue.nonEmpty) {
      val newNodes = explorer(queue.dequeue(), collection.toSet).diff(collection.toSet)

      queue.enqueueAll(newNodes)
      collection ++= newNodes
    }

    collection.toSet
  }

  /**
    * Collect elements in a BFS manner.
    *
    * @param initial  The initial set to start collecting with.
    * @param explorer A function exploring all possible elements from one element.
    * @tparam T The type of elements.
    * @return All collected elements.
    */
  def collect[T](initial: Seq[T], explorer: T => Set[T]): Set[T] = {
    collect(initial, (t: T, _: Set[T]) => explorer(t))
  }
}