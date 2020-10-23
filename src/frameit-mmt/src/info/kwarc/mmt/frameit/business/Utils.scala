package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.{Path, StructuralElement}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup

object Utils {
  /**
    * mixture of [[Controller.getAs]] and [[Controller.getO]]
    * @todo integrate into official [[Controller]] API?
    */
  def getAsO[E <: StructuralElement](cls : Class[E], path: Path)(implicit lookup: Lookup): Option[E] = lookup.getO(path) match {
    case Some(e : E@unchecked) if cls.isInstance(e) => Some(e)
    case _ => None
  }
}
