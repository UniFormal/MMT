package info.kwarc.mmt.api

import uom._

/**
 * after parsing and checking, simplification is the thrid phase of the source processing pipeline
 *  
 * the main interfaces are
 * * [[StructureSimplifier]]: simplification of structure, e.g.,, elaboration
 * * [[ObjectSimplifier]]: simplification of objects, e.g, computation, normalization, rewriting
 * 
 * Each has one primary implementation, which is also in this package
 * * [[ElaborationBasedSimplifier]]: for structure
 * * [[RuleBasedSimplifier]]: for objects
 * Both can be customized, by structural and features rules respectively.
 * 
 */
package object uom {
}
