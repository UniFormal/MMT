package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.objects.{OMLIT, OMS, Term}
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api._
import info.kwarc.mmt.lf._

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

import SynOpType._
import SemOpType._
import SemanticOperator._

//TODO these are [[SemanticOperator]]s now that are independent of specific MMT URIs; the connection between MMT URIs and semantic operators is made in .mmt syntax now
// the corresponding .mmt files have to be adapted

object DoubleFunctions {
   
   val R = StandardDouble // realizes Apply(OMS(tm),OMS(path ? "reals")
   
   object Negative extends Unary(R,R, {case R(x) => -x})
   object Addition extends Binary(R,R,R, {case (R(x),R(y)) => x+y})
   object Multiplication extends Binary(R,R,R, {case (R(x),R(y)) => x*y})
   object Division extends Binary(R,R,R, {case (R(x),R(y)) => x/y})
   object Modulo extends  Binary(R,R,R, {case (R(x),R(y)) => x % y})
   object Tangent extends Unary(R,R, {case R(x) => scala.math.tan(scala.math.toRadians(x))})
   object ArcTangent extends Unary(R,R, {case R(x) => scala.math.atan(scala.math.toRadians(x))})
   object Sine extends Unary(R,R, {case R(x) => scala.math.sin(scala.math.toRadians(x))})
   object ArcSine extends Unary(R,R, {case R(x) => scala.math.asin(scala.math.toRadians(x))})
   object Cosine extends Unary(R,R, {case R(x) => scala.math.cos(scala.math.toRadians(x))})
   object ArcCosine extends Unary(R,R, {case R(x) => scala.math.acos(scala.math.toRadians(x))})
   object SquareRoot extends Unary(R,R, {case R(x) => scala.math.sqrt(x)})
}