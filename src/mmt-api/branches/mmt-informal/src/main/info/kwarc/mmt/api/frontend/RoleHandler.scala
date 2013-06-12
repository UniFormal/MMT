package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._

/**
 * A RoleHandler is an Extension that is called on every Constant with a certain role
 */
abstract class RoleHandler extends Extension {
   def isApplicable(role: String): Boolean
   def apply(c: symbols.Constant)
}