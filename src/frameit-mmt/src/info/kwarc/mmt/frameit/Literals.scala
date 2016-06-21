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

object Negative extends RealizedOperator(path ? "negative") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(-u)
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object Addition extends RealizedOperator(path ? "add") {
  val argTypes = List(RealLiterals, RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double), RealLiterals(v : Double)) => RealLiterals.apply(u + v)
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object Multiplication extends RealizedOperator(path ? "mul") {
  val argTypes = List(RealLiterals, RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double), RealLiterals(v : Double)) => RealLiterals.apply(u * v)
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object Division extends RealizedOperator(path ? "div") {
  val argTypes = List(RealLiterals, RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double), RealLiterals(v : Double)) => RealLiterals.apply(u / v)
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object Modulo extends RealizedOperator(path ? "mod") {
  val argTypes = List(RealLiterals, RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double), RealLiterals(v : Double)) => RealLiterals.apply(u % v)
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object Tangent extends RealizedOperator(path ? "tan") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.tan(scala.math.toRadians(u)))
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object ArcTangent extends RealizedOperator(path ? "atan") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.atan(scala.math.toRadians(u)))
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object Sine extends RealizedOperator(path ? "sin") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.sin(scala.math.toRadians(u)))
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object ArcSine extends RealizedOperator(path ? "asin") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.asin(scala.math.toRadians(u)))
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object Cosine extends RealizedOperator(path ? "cos") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.cos(scala.math.toRadians(u)))
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object ArcCosine extends RealizedOperator(path ? "acos") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.acos(scala.math.toRadians(u)))
    case _ => throw new Exception("Put a helpful error message here")
  }
}

object SquareRoot extends RealizedOperator(path ? "sqrt") {
  val argTypes = List(RealLiterals)
  val retType = RealLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(RealLiterals(u : Double)) => RealLiterals.apply(scala.math.sqrt(u))
    case _ => throw new Exception("Put a helpful error message here")
  }
}
