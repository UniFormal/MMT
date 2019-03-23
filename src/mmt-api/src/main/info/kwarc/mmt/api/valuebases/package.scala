package info.kwarc.mmt.api

import valuebases._
import objects._

/**
  * This package maintains databases of cocnrete mathematical objects.
  * 
  * Concrete objects are special MMT [[Term]]s that can be represented as concrete database objects (e.g., JSON).
  * The connection between the two is mediated by [[Codec]]s and [[CodecOerator]].
  * The codec-based translation is implemented in the [[Coder]].  
  */
package object valuebases { 
}