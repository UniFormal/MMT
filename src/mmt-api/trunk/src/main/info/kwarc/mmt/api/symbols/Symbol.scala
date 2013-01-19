package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._

/**
 * A Symbol represents an MMT symbol.<p>
 */
abstract class Symbol extends Declaration

/**
 * An Assignment represents an MMT assignment.<p>
 */
abstract class Assignment extends Declaration {
   //val target : Term //TODO: should be here and optional
}
