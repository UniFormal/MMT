package info.kwarc.mmt.frameit.archives.Foundation

import info.kwarc.mmt.api._
import checking._
import objects.{OMS, Stack, Term}
import uom._

import info.kwarc.mmt.lf.{Apply, ApplySpine, Typed}


case class NormedDimension(length : Int, time : Int, mass : Int, amount : Int, temperature : Int, current : Int, lum : Int) {
  def +(that : NormedDimension) = NormedDimension(length + that.length,
    time + that.time,
    mass + that.mass,
    amount + that.amount,
    temperature + that.temperature,
    current + that.current,
    lum + that.lum)

  def minus = NormedDimension(-length, -time, -mass, -amount, -temperature, -current, -lum)

  def toTerm = ApplySpine(NormedDim.term,
    IntegerLiterals.of(BigInt(length)),
    IntegerLiterals.of(BigInt(time)),
    IntegerLiterals.of(BigInt(mass)),
    IntegerLiterals.of(BigInt(amount)),
    IntegerLiterals.of(BigInt(temperature)),
    IntegerLiterals.of(BigInt(current)),
    IntegerLiterals.of(BigInt(lum)))
}

case class NormedUnit(meter : Int, second : Int, kg : Int, mole : Int, kelvin : Int, ampere : Int, candela : Int) {
  def +(that : NormedUnit) = NormedUnit(meter + that.meter,
    second + that.second,
    kg + that.kg,
    mole + that.mole,
    kelvin + that.kelvin,
    ampere + that.ampere,
    candela + that.candela)

  def minus = NormedUnit(-meter, -second, -kg, -mole, -kelvin, -ampere, -candela)

  def toTerm : Term = ApplySpine(NormedDim.term,
    IntegerLiterals.of(BigInt(meter)),
    IntegerLiterals.of(BigInt(second)),
    IntegerLiterals.of(BigInt(kg)),
    IntegerLiterals.of(BigInt(mole)),
    IntegerLiterals.of(BigInt(kelvin)),
    IntegerLiterals.of(BigInt(ampere)),
    IntegerLiterals.of(BigInt(candela)))
  def dimension = NormedDimension(meter,second,kg,mole,kelvin,ampere,candela)
}

object NormedDim {
  val path = Units.normeddim
  val term = OMS(path)

  def unapply(t : Term) : Option[NormedDimension] = t match {
    case ApplySpine(`term`,List(
      IntegerLiterals(length : BigInt),
      IntegerLiterals(time : BigInt),
      IntegerLiterals(mass : BigInt),
      IntegerLiterals(amount : BigInt),
      IntegerLiterals(temperature : BigInt),
      IntegerLiterals(current : BigInt),
      IntegerLiterals(lum : BigInt))) => Some(NormedDimension(length.toInt,time.toInt,mass.toInt,amount.toInt,temperature.toInt,current.toInt,lum.toInt))
    case _ => None
  }

  val length      = NormedDimension(1,0,0,0,0,0,0)
  val time        = NormedDimension(0,1,0,0,0,0,0)
  val mass        = NormedDimension(0,0,1,0,0,0,0)
  val amount      = NormedDimension(0,0,0,1,0,0,0)
  val temperature = NormedDimension(0,0,0,0,1,0,0)
  val current     = NormedDimension(0,0,0,0,0,1,0)
  val lum         = NormedDimension(0,0,0,0,0,0,1)
  val none        = NormedDimension(0,0,0,0,0,0,0)
}
object NormedU {
  val path = Units.normedunit
  val term = OMS(path)

  def unapply(t : Term) : Option[NormedUnit] = t match {
    case ApplySpine(`term`,List(
    IntegerLiterals(meter : BigInt),
    IntegerLiterals(second : BigInt),
    IntegerLiterals(kg : BigInt),
    IntegerLiterals(mole : BigInt),
    IntegerLiterals(kelvin : BigInt),
    IntegerLiterals(ampere : BigInt),
    IntegerLiterals(candela : BigInt))) => Some(NormedUnit(meter.toInt,second.toInt,kg.toInt,mole.toInt,kelvin.toInt,ampere.toInt,candela.toInt))
    case _ => None
  }

  val meter       = NormedUnit(1,0,0,0,0,0,0)
  val second      = NormedUnit(0,1,0,0,0,0,0)
  val kg          = NormedUnit(0,0,1,0,0,0,0)
  val mole        = NormedUnit(0,0,0,1,0,0,0)
  val kelvin      = NormedUnit(0,0,0,0,1,0,0)
  val ampere      = NormedUnit(0,0,0,0,0,1,0)
  val candela     = NormedUnit(0,0,0,0,0,0,1)
  val none        = NormedUnit(0,0,0,0,0,0,0)
}

object Normalize {
  def units(t : Term)(implicit solver : CheckingCallback,stack : Stack, history : History) : Option[NormedUnit] = solver.safeSimplifyUntil(t)(Units.UnitTimes.unapply)._1 match {
    case Units.UnitTimes(ls) => Some(ls.map(units).collect{
      case Some(s) => s
      case None => return None
    }.foldLeft(NormedU.none)((a,b) => a + b))
    case Units.UnitDiv(d1,d2) => units(d1).map(_ + units(d2).getOrElse(return None).minus)
    case OMS(Units.meter) => Some(NormedU.meter)
    case OMS(Units.second) => Some(NormedU.second)
    case OMS(Units.kg) => Some(NormedU.kg)
    case OMS(Units.mole) => Some(NormedU.mole)
    case OMS(Units.kelvin) => Some(NormedU.kelvin)
    case OMS(Units.ampere) => Some(NormedU.ampere)
    case OMS(Units.candela) => Some(NormedU.candela)
    case _ => None
  }
  def dims(t : Term)(implicit solver : CheckingCallback,stack : Stack, history : History) : Option[NormedDimension] = solver.safeSimplifyUntil(t)(Units.UnitTimes.unapply)._1 match {
    case Units.DimTimes(ls) => Some(ls.map(dims).collect{
      case Some(s) => s
      case None => return None
    }.foldLeft(NormedDim.none)((a,b) => a + b))
    case Units.DimDiv(d1,d2) => dims(d1).map(_ + dims(d2).getOrElse(return None).minus)
    case OMS(Units.length) => Some(NormedDim.length)
    case OMS(Units.time) => Some(NormedDim.time)
    case OMS(Units.mass) => Some(NormedDim.mass)
    case OMS(Units.amount) => Some(NormedDim.amount)
    case OMS(Units.temperature) => Some(NormedDim.temperature)
    case OMS(Units.current) => Some(NormedDim.current)
    case OMS(Units.lum) => Some(NormedDim.lum)
    case OMS(Units.dimnone) => Some(NormedDim.none)
    // case _ => Some(NormedDim.none)
    case _ => None
  }
}

object NoneIsReal extends ComputationRule(Units.qe) {
  override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case Apply(OMS(Units.qe),Units.dimnone) => Simplify(OMS(Math.real))
    case Apply(_,Units.dimnone) => RecurseOnly(List(1))
    case Apply(OMS(Units.qe),_) => RecurseOnly(List(2))
    // case Apply(_,_) => Recurse
    case _ => Simplifiability.NoRecurse
  }
}

object DimEq extends TypeBasedEqualityRule(Nil,Units.dimensions ? "dimension") {
  def applicableToTerm(solver:Solver,tm: Term) = true
  
  override def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val (t1,t2) = (Normalize.dims(tm1)(solver,stack,history),Normalize.dims(tm2)(solver,stack,history))
    (t1,t2) match {
      case (Some(nd1),Some(nd2)) => Some(nd1 == nd2)
      case _ =>
        history += "Can't normalize dimensions"
        None
    }
  }
}

object UnitEq extends TypeBasedEqualityRule(Nil,Units.units ? "unit") {
  def applicableToTerm(solver:Solver,tm: Term) = true

  override def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val (t1,t2) = (Normalize.units(tm1)(solver,stack,history),Normalize.units(tm2)(solver,stack,history))
    (t1,t2) match {
      case (Some(nd1),Some(nd2)) => Some(nd1 == nd2)
      case _ =>
        history += "Can't normalize units"
        None
    }
  }
}



/*
object MultSimp extends ComputationRule(Units.DimTimes.path) {
  override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] =
    Normalize(tm)(check,stack,history).map(_.toTerm)
}
object DivSimp extends ComputationRule(Units.DimDiv.path) {
  override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] =
    Normalize(tm)(check,stack,history).map(_.toTerm)
}
*/
