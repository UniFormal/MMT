package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.objects.{OMLIT, OMS, Term}
import info.kwarc.mmt.api.uom.{RealizedOperator, RealizedType, StandardDouble}
import info.kwarc.mmt.api.{DPath, utils}
import info.kwarc.mmt.lf.Apply

/**
  * Created by raupi on 23.03.16.
  */
object PlanarGeometry {
  val _base = DPath(utils.URI("http", "cds.omdoc.org") / "FrameIT")
  val path = _base ? "planar_geometry"
  val holpath = _base ? "HOL"
  val tm = holpath ? "tm"
}

import info.kwarc.mmt.frameit.PlanarGeometry._

object RealLiterals extends RealizedType(Apply(OMS(tm),OMS(path ? "reals")),StandardDouble)

object Tangens extends RealizedOperator(path ? "tan") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.tan(u))
    case _ => throw new Exception("Put a helpful error message here")
  }
}
