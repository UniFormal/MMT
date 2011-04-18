package info.kwarc.mmt.uom

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

class Implementation(of : GlobalName, f : ((Term*) => Term) ) {
  def apply(args : Term*) : Term = { f(args : _*)}
}
