package info.kwarc.mmt.odk.LMFDB

import info.kwarc.mmt.api.ParseError
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web.WebQuery

import scala.collection.mutable
import scala.util.Try

/**
  * An object that can lazily resolve queries
  * @tparam T Type of Data the query returns
  * @tparam L Labels of individual query results
  */
trait QueryIterator[T, L] {
  /** an element that has been ungotten */
  private var pushback: Option[(T, L)] = None

  /**
    * Gets the next element and its label from this QueryIterator, or None
    * if there is no such element
    * @return
    */
  def get: Option[(T, L)] = {
    if (pushback.nonEmpty) {
      val ret = pushback.get
      pushback = None
      Some(ret)
    } else {
      getActual
    }
  }

  /**
    * Gets the next data item (without label) from the result
    * @return
    */
  def getNext: Option[T] = get.map(_._1)

  /** gets all elements from this iterator */
  def all: List[T] = {
    val cache = mutable.ListBuffer[T]()

    var continue = true
    while(continue){
      getNext match {
        case Some(x) => cache += x
        case None => continue = false
      }
    }

    cache.toList
  }

  /** take the next n elements */
  def take(n: Int): List[T] = {
    val cache = mutable.ListBuffer[T]()

    var continue = true
    while(continue && cache.size <= n){
      getNext match {
        case Some(x) => cache += x
        case None => continue = false
      }
    }

    cache.toList
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

  /**
    * function to receive the next element along with it's label from the underlying implementation.
    *
    * Assumption: getActual returns the results in order of labels
    * @return
    */
  protected def getActual: Option[(T, L)]

  /** an order on the labels, returns if left > right */
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
    throw new Exception("Invalid State")
  }
}

/** a class simplifying nesting using QueryIteratorJoin */
abstract class LeafIterator[T, L] extends QueryIterator[List[T], L] {
  def getLeaf: Option[(T, L)]
  def getActual: Option[(List[T], L)] = getLeaf.map({case (a, b) => (List(a), b)})
}

/**
  * A query iterator retrieving results from a pagination JSON API
  * @param url Initial url to retrieve results from
  * @param extra extra data to be passed along with this JSONURLIterator
  * @param label_key Key of (string) label used to identifiy curves
  * @param data_key Key to find data items under
  * @param next_key Key to find next url under
  * @tparam T Type of extra data
  */
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
        val res = JSONFromURL(next).getOrElse(return None).asInstanceOf[JSONObject]

        // get the next next
        current = next
        next = res.getAsString(next_key)

        // build the cache
        cache = res.getAs(classOf[JSONArray], data_key).values.map(_.asInstanceOf[JSONObject]).toList

        // and return our json
        getJSON
      }
  }
  def getLeaf: Option[((JSONObject, T), String)] = getJSON.map { (jo: JSONObject) =>
    ((jo, extra), jo.getAsString(label_key))
  }
}

/** Query Iterators for LMFDB */
trait LMFDBQueryIterators { this: LMFDBSystem =>
  type LQI = QueryIterator[List[(JSONObject, DB)], String]

  def makeQueryIterator(queries: List[(JSONObject, DB)]): LQI = {
    val queryIterators = queries.map({case (d, j) => lmfdbIterator(d, j)})
    queryIterators.reduce[LQI]({case (l, r) => new QueryIteratorJoin(l, r)})
  }

  private def lmfdbIterator(query: JSONObject, db: DB): LQI = {
    val schema = controller.get(db.schemaPath) match {
      case dt: Theory => dt
      case _ => error("Schema-Theory missing from controller")
    }

    val key = getKey(schema)
    val theQuery = query.map :+ (JSONString("_sort"), JSONString(key))

    val queryParams = theQuery.map(kv => {
      val key = kv._1.value
      val value = if(key.startsWith("_")){
        kv._2.asInstanceOf[JSONString].value
      } else {
        s"py${kv._2.toString}"
      }
      (key, value)
    })

    val url = db.queryurl(s"&${WebQuery.encode(queryParams)}")

    new JSONURLIterator(url.toString, db, key)
  }
}


/*
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