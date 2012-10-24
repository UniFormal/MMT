package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._

/**
 * A Symbol represents an MMT symbol.<p>
 * 
 * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent theory
 */
abstract class Symbol extends Declaration {
   val home : Term
}

/**
 * An Assignment represents an MMT assignment.<p>
 * 
 * @param home the {@link info.kwarc.mmt.api.objects.Term} representing the parent link
 */
abstract class Assignment extends Declaration {
   val home : Term
   //val target : Obj
}
