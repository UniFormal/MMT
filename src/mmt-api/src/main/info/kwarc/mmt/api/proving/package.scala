package info.kwarc.mmt.api

import proving._

/**
  * The algorithm for proving theorems about MMT content.
  * This is very premature and experimental.
  * 
  * See [[api]] for an overview of the algorithms.
  * 
  * The main interfaces are
  * - [[Prover]]: object level proving
  *
  * Structure level proving does not exist yet.
  *
  * The main implementations are
  * - [[RuleBasedProver]] for object-level proving
  * 
  * The latter creates a [[Searcher]] for each proving task, which applies search rules to find MMT objects. 
  * 
  */
package object proving {
}
