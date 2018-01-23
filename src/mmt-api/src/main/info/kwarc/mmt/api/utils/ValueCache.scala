package info.kwarc.mmt.api.utils

/**
 * An auxiliary trait for introducing structure sharing of values
 * the most recent values are cached,
 * if a new value is in the cache, it is discarded and replaced with the cached one
 *
 * @tparam A the type values to cache
 * @param size the number of values to cache
 * @param enable set to false to switch off caching (mainly useful as a base line for optimization)
 */
class ValueCache[A<:AnyRef](size: Int, enable: Boolean = true) {
   /**
    * the cache
    *
    * Scala doesn't let us make this an Array[A]
    * so we have to cast when retrieving elements, but the problem is minor since it is a private field
    */
   private val cache = new Array[AnyRef](size)
   private var next = 0
   @inline private def dec(i:Int) = if (i==0) size-1 else i-1
   @inline private def inc(i:Int) = if (i==size-1) 0 else i+1
   def get(a: A): A = {
      if (!enable) return a
      var i = next
      do {
         i = dec(i)
         val c = cache(i)
         if (c == a) return c.asInstanceOf[A]
      } while (i != next)
      cache(next) = a
      next = inc(next)
      a
   }
}

/**
 * An auxiliary trait for introducing tabling a function and introducing structure sharing of result values
 * @tparam A the input type
 * @tparam B the output type
 * @param f the function to table
 * @param size the number of A-B-pairs to cache
 */
class ResultCache[A,B](f: A => B, size: Int) {
   /**
    * the cache
    *
    * Scala doesn't let us make this an Array[A]
    * so we have to cast when retrieving elements, but the problem is minor since it is a private field
    */
   private val cache = new Array[(A,B)](size)
   private var next = 0
   @inline private def dec(i:Int) = if (i==0) size-1 else i-1
   @inline private def inc(i:Int) = if (i==size-1) 0 else i+1
   def apply(a: A): B = {
      var i = next
      do {
         i = dec(i)
         val c = cache(i)
         if (c != null && c._1 == a) return c._2
      } while (i != next)
      val b = f(a)
      cache(next) = (a,b)
      next = inc(next)
      b
   }
}
