package info.kwarc.mmt.api

import checking._

/**
  * The algorithm for checking MMT content.
  * See [[api]] for an overview of the algorithms.
  * 
  * The main interfaces are
  * - [[Checker]]: the main interface for checkers (combining a structure and an object checker)
  * - [[StructureChecker]]: checking structural elements
  * - [[ObjectChecker]]: checking objects
  *
  * The main implementations are
  * - [[MMTStructureChecker]] for structural elements
  * - [[RuleBasedChecker]] for objects
  * 
  * The latter creates a [[Solver]] for each judgment, which perform type reconstruction.
  * 
  * Structure checking is not extensible except through [[DerivedElement]]s.
  * Object checking is extensible through [[Rule]]s.  
  */
package object checking {
}
