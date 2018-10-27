package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api.ParseError
import info.kwarc.mmt.api.utils._

import scala.collection.mutable
import scala.util.Try

/**
  * A
  */
trait QueryIterator[T, L] {
  /** an element that has been ungotten */
  private var pushback: Option[(T, L)] = None

  /** gets the next element */
  def get: Option[(T, L)] = {
    if (pushback.nonEmpty) {
      val ret = pushback.get
      pushback = None
      Some(ret)
    } else {
      getActual
    }
  }

  /** gets all elements in this iterator */
  def all: List[T] = {
    val cache = mutable.Stack[T]()

    var continue = true
    while(continue){
      getNext match {
        case Some(x) => cache.push(x)
        case None => continue = false
      }
    }

    cache.reverse.toList
  }

  /** gets the next actual result */
  def getNext: Option[T] = get.map(_._1)

  /** take the next n elements */
  def take(n: Int): List[T] = {
    val cache = mutable.Stack[T]()

    var continue = true
    while(continue && cache.size <= n){
      getNext match {
        case Some(x) => cache.push(x)
        case None => continue = false
      }
    }

    cache.reverse.toList
  }

  def drop(n: Int): QueryIterator[T, L] = {
    take(n)
    this
  }

  /** takes a given slice from this query iterator */
  def slice(from: Int, to: Int): List[T] = take(to).drop(from)

  /** ungets an element, i.e. adds it to the pushback */
  def unget(e: (T, L)): Unit = {
    if(pushback.nonEmpty){
      throw new Exception("can not pushback two items")
    }

    pushback = Some(e)
  }

  /** to be implemented by subclass: gets the next element of this iterator */
  protected def getActual: Option[(T, L)]

  /** an order on L */
  def >(left: L, right: L): Boolean
}

/** a lazy query iterator that joins */
class QueryIteratorJoin[T, L](l: QueryIterator[List[T], L], r: QueryIterator[List[T], L]) extends QueryIterator[List[T], L] {
  /** order: just recurse */
  def >(left: L, right: L): Boolean = l.>(left, right) || r.>(left, right)

  def getActual: Option[(List[T], L)] = {
    while(true){
      val lr = l.get.getOrElse(return None)

      var continue = true
      while(continue){
        // it's not in None
        val rr = r.get.getOrElse(return None)

        // horray we have the element
        if(rr._2 == lr._2){
          return Some(lr._1 ::: rr._1, lr._2)
        }

        // we have a bigger element than rr
        // so we have no chance that any element will ever be around
        else if (>(rr._2, lr._2)) {
          r.unget(rr)
          continue = false
        }
      }
    }
    throw new Exception("you can't reach me")
  }
}

abstract class LeafIterator[T, L] extends QueryIterator[List[T], L] {
  def getLeaf: Option[(T, L)]
  def getActual: Option[(List[T], L)] = getLeaf.map({case (a, b) => (List(a), b)})
}

class JSONURLIterator[T](url: String, extra: T, label_key: String, data_key: String="data", next_key: String = "next") extends LeafIterator[(JSONObject, T), String] {
  def >(left: String, right: String): Boolean = left > right

  private var current: String = ""
  private var next: String = url

  private var cache: List[JSONObject] = Nil

  def getJSON: Option[JSONObject] = cache match {
    case h :: t =>
      cache = t
      Some(h)
    case Nil =>

      // if we have no new results to fetch
      // we have no more elements
      if (next == current) {
        None

      // if we have a next url, fetch it
      } else {
        val res = JSONURLIterator.get_json(next).getOrElse(return None).asInstanceOf[JSONObject]

        // get the next next
        current = next
        next = res.getAsString(next_key)

        // build the cache
        cache = res.getAs(classOf[JSONArray], data_key).values.map(_.asInstanceOf[JSONObject]).toList

        // and return our json
        getJSON
      }
  }
  def getLeaf: Option[((JSONObject, T), String)] = getJSON.map { jo: JSONObject =>
    ((jo, extra), jo.getAsString(label_key))
  }
}

object JSONURLIterator {
  private def get_json(url: String) : Option[JSON] = {
    // println("getting json from: " + url) // for debug
    val attempt = Try(io.Source.fromURL(url))
    // println("done getting json from: " + url)
    if (attempt.isFailure) None else {
      val json = Try(JSON.parse(attempt.get.toBuffer.mkString))
      // println("done parsing json from: " + url)
      if (json.isFailure) throw ParseError(url.toString).setCausedBy(json.failed.get)
      json.toOption
    }
  }
}

/*
FOR SIMPLE TESTIN
object Test {
  class Multiples(n: Int) extends QueryIterator[List[(Int, Int)], Int] {
    def >(left: Int, right: Int): Boolean = left > right

    var int: Int = 0
    def getActual: Option[(List[(Int, Int)], Int)] = {
      //println(s"get $int ($this): $int")
      int += 1
      Some(List((n, n*int)), n*int)
    }
  }

  def main(args: Array[String]): Unit = {
    val both = new QueryIteratorJoin(new Multiples(4), new Multiples(8))
    val both2 = new QueryIteratorJoin(new Multiples(2), both)

    val res = both2.slice(2, 10)
    print(res)
  }
}*/