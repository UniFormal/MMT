package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._

/**
 * A SFSymbol represents an flexiformal MMT symbol.<p>
 */
trait SFSymbol extends Declaration
  
/**
 * A SFAssignment represents an flexiformal MMT assignment.<p>
 */
trait SFAssignment extends Declaration


/**
 * A Symbol represents an MMT symbol.<p>
 */
abstract class Symbol extends Declaration with SFSymbol {
   /** every MMT symbol takes a list of parameters
    * empty by default, may be overridden when constructing instances
    */
   val parameters = Context()
}

/**
 * An Assignment represents an MMT assignment.<p>
 */
abstract class Assignment extends Declaration with SFAssignment {
   //val target : Term //TODO: should be here and optional
}
