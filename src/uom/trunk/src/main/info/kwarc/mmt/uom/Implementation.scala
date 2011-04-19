package info.kwarc.mmt.uom

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

class Implementation(constantName : GlobalName, function : ((Term*) => Term) ) {
  def name = constantName
  def f = function
  def apply(args : Term*) : Term = { f(args : _*)}
}
