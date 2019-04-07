package info.kwarc.mmt.api

import notations._

/**
  * This package maintains the common data structures for parsing and presentation.
  *   
  * [[TextNotation]] is the main notation class. It is similar to MMT objects, in particular it is stateless. 
  *
  * [[NotationContainer]] statefully maintains the notations assigned to a declaration.
  * These are owned by [[StructuralElement]]s to carry notations, akin to how they carry type/definition.
  */
package object notations {
}
