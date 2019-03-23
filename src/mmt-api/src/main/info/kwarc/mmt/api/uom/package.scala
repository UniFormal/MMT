package info.kwarc.mmt.api

import uom._

/**
  * =Simplification=
  *
  * The algorithm for immutably computing with MMT content, i.e., simplification (strings to MMT data structures).
  * See [[api]] for an overview of the algorithms.
  * 
  * The main interfaces are
  * - [[Simplifier]]: the main interface for parser (combining a structure and an object simplifier)
  * - [[StructureSimplifier]]: simplifying structural elements
  * - [[ObjectSimplifier]]: simplifying objects
  *
  * The main implementations are
  * - [[ElaborationBasedSimplifier]] for structural elements
  * - [[RuleBasedSimplifier]] for objects
  * 
  * Structure simplification is extensible using derived elements.
  * Object simplification is extensible using rules.
  * 
  * =Literals and semantic objects=
  * 
  * This package also contains the classes for using Scala objects as MMT literals.
  * 
  * [[SemanticType]] defines types as sets of Scala objects.
  * [[SemanticValue]] defines a distinguished element of such a type.
  * [[SemanitcOperator]] defines functions on such types as Scala functions.
  * 
  * Literals and operations on them are injected into the MMT language
  * by declaring [[RealizedValue]], [[RealizedType]] and [[RealizedOperator]] rules,
  * which tie a syntactic type/operator (i.e., an MMT term) to a semantic counterpart.
  * 
  * [[StandardLiterals]] defines semantic types for the most important types.
  * 
  * [[RealizedTheory]] represents an MMT theory programmed in Scala, usually as a Scala class.
  *
  * =Scala companion objects for MMT theories=
  * 
  * [[TheoryScala]] and [[ConstantScala]] are auxiliary classes that are useful when implementing MMT rules or other logic-specific algorithms.
  */
package object uom {
}
