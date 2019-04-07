package info.kwarc.mmt.api

import presentation._

/**
  * The algorithm for presenting MMT content (data structures to  user-facing formats).
  * See [[api]] for an overview of the algorithms.
  * 
  * The main interfaces are
  * - [[Presenter]]: the main interface for parser (combining a structure and an object parser)
  * - [[StructurePresenter]]: presenting structural elements
  * - [[ObjectPresenter]]: presenting objects
  *
  * The main implementations are (in each case for structural elements and objects)
  * - for OMDoc XML: [[OMDocPresenter]] resp. [[OpenMathPresenter]]
  * - for plain strings (using the toString methods): [[TextPresenter]] resp. [[ObjectTextPresenter]]
  * - for nice human-oriented strings: [[MMTStructurePresenter]] resp. [[NotationBasedParser]]
  * - for HTML: [[HTMLPresenter]] resp. [[MathMLPresenter]]
  */
package object presentation {
}
