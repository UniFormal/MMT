package info.kwarc.mmt.pvs

/**
 * The classes in this package model the grammar of the PVS XML export.
 *
 * They have a dual purpose
 *  * provide a Scala model of the PVS data structures
 *  * automatically induce a parser for the XML files (see [[syntax.makeParser]]).
 *
 * See [[pvs_file]] and [[Object]] for the best entry points.
 *
 * The reference for the XML is https://github.com/samowre/PVS/blob/master/lib/pvs.rnc
 */
package object syntax {
   /**
    * @return a parser that takes PVS XML and returns classes in this package
    *
    * The parser uses [[info.kwarc.mmt.api.utils.XMLToScala]].
    * That means the parser does not have to be changed if the classes in this package are changed.
    * Instead, the parser is generated automatically using Scala reflection, i.e., the classes in this package define the grammar of the parser.
    */
   def makeParser = new info.kwarc.mmt.api.utils.XMLToScala("info.kwarc.mmt.pvs.syntax")
}
