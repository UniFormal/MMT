package info.kwarc.mmt.api.utils

/** adds an equality method that is guarded by equality of cached hashcodes
 *  
 *  This can be mixed into immutable inductive data structures whose values are often tested for equality.
 *  Failing equality tests do not traverse the value anymore the first time.
 *  
 *  @tparam A the type into which this is mixed in (only needed to constrain the type of hasheq)
 */
trait HashEquality[A] {
   /** the hash code */
   lazy val hash: Int = hashCode
   /** this hasheq that is the same as this == that, but fails immediately if false */
   def hasheq(that: HashEquality[A]) : Boolean = this.hash == that.hash && this == that
   /** this hasheq that is the same as this == that, but fails immediately if false */
   def hashneq(that: HashEquality[A]) : Boolean = this.hash != that.hash || this != that
}