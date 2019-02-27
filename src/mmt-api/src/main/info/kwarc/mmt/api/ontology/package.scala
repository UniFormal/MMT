package info.kwarc.mmt.api

import ontology._

/**
  * This package contains a relational ontology and a query engine for it.
  * 
  * The main classes are:
  * - [[RelationalElement]] defines the concepts and relation of the ontology (TBox)
  * - [[RelationalManager]] extracts the ABox from MMT content
  * - [[RelStore]] maintains the model of the ontology (ABox)
  * - [[RelationGraphExporter]] allows exporting the ABox as a graph.
  * - [[Query]] defines a query language for the ontology
  * - [[Evaluator]] implements the query language for a given ABox.
  * - [[Search]] maintains classes for faceted search, in particular the facet for [[MathWebSearch]].
  */
package object ontology {
}
