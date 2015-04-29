package info.kwarc.mmt.pvs

/**
 * The classes in this package model the grammar of the PVS XML export.
 * 
 * They have a dual purpose
 *  * provide a Scala model of the PVS data structures
 *  * induce a parser for the XML files via [[info.kwarc.mmt.api.utils.XMLToScala]].
 * 
 * See [[pvs_file]] and [[Object]] for the best entry points.
 * 
 * The reference for the XML is https://github.com/samowre/PVS/blob/master/lib/pvs.rnc
 */
package object syntax {
   /**
    * @return a parser that takes PVS XML and returns classes in this package
    * 
    * The parser does not have to be changed if the classes in this package are changed.
    */
   def makeParser = new info.kwarc.mmt.api.utils.XMLToScala("info.kwarc.mmt.pvs.syntax")
}