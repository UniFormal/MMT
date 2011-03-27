package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._

/**
 * An Import represents an MMT inclusion.<p>
 *
 * @param to the module into which it is included
 * @param of the included module object
 */
abstract class Import extends Declaration {
   val of : ModuleObj
   def toNode = <include>{of.toOBJNode}</include>
   def components = List(of)
   val path = parent
   val role = info.kwarc.mmt.api.Role_Include
}

/**
 * A TheoImport represents an MMT inclusion into a theory.<p>
 *
 * @param par the codomain of the inclusion
 * @param thy the included theory
 */
case class TheoImport(parent : MPath, of : TheoryObj) extends Import
/**
 * A PlainImport represents an MMT inclusion between theories.<p>
 *
 * @param from the domain of the inclusion
 * @param to the codomain of the inclusion
 */
object PlainImport {
	def apply(from : MPath, to : MPath) = TheoImport(to, OMT(from))
	def unapply(t: TheoImport) : Option[(MPath,MPath)] = t match {
		case TheoImport(to, OMT(from)) => Some((from, to))
		case _ => None
	}
}


/**
 * A LinkImport represents an MMT inclusion into a link.<p>
 *
 * @param to the link
 * @param of the included morphism
 */
case class LinkImport(parent : MPath, of : Morph) extends Import