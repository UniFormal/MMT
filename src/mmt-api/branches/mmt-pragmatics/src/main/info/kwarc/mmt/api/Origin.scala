package info.kwarc.mmt.api

/** describes the origin of a generated knowledge item */
abstract class Origin
/** obtained by taking the closure under inclusions
 *  An Include yields further Include element due to transitivity.
 *  A DefLinkAssignment yields further DefLinkAssignment elements by adding those for the theories included into the domain.
 */
case object IncludeClosure extends Origin
case object MetaInclude extends Origin
/** obtained by elaborating an instance of a pattern */
case class InstanceElaboration(instance: GlobalName) extends Origin
/** obtained by replacing document parts with references */
case object DocumentSkeleton extends Origin

