package info.kwarc.mmt.api

import parser._

/**
  * The algorithm for parsing MMT content (strings to MMT data structures).
  * See [[api]] for an overview of the algorithms.
  * 
  * The main interfaces are
  * - [[Parser]]: the main interface for parser (combining a structure and an object parser)
  * - [[StructureParser]]: parsing structural elements
  * - [[ObjectParser]]: parsing objects
  *
  * The main implementations are
  * - [[KeywordBasedParser]] for structural elements in .mmt files
  * - [[NotationBasedParser]] for objects
  * 
  * The latter creates a [[Scanner]] for each string, which applies [[notations]] to parse user-defined mixifx syntax.
  *
  * Structure parsing is extensible using [[ParserExtension]]s.
  * Object parsing is extensible using notations or [[LexerExtension]]s. 
  */
package object parser {
}
