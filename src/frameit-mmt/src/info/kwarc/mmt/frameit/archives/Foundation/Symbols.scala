package info.kwarc.mmt.frameit.archives.Foundation

import info.kwarc.mmt.api.{DPath, GlobalName}
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{Apply, ApplySpine}

object MitM {
  val path = DPath(URI("http","mathhub.info") / "MitM" / "Foundation")
  val under = List(Apply.path)
}

object Math {
  val path = MitM.path ? "Math"

  val logic = MitM.path ? "Logic"
  val natliterals = MitM.path ? "NatLiterals"
  val intliterals = MitM.path ? "IntLiterals"
  val realliterals = MitM.path ? "RealLiterals"
  val strings = MitM.path ? "Strings"
  val lists = MitM.path ? "Lists"
  val vectors = MitM.path ? "Vectors"
  val matrices = MitM.path ? "Matrices"

  val posplus = natliterals ? "plus_pos_lit"
  val natplus = natliterals ? "plus_nat_lit"
  val intplus = intliterals ? "plus_int_lit"
  val realplus = natliterals ? "plus_real_lit"

  val postimes = natliterals ? "times_pos_lit"
  val nattimes = natliterals ? "times_nat_lit"
  val inttimes = intliterals ? "times_int_lit"
  val realtimes = natliterals ? "times_real_lit"

  //val tms = typesystem ? "tm"
  val bool = logic ? "prop"
  val tt = logic ? "true"
  val ff = logic ? "false"

  val real = realliterals ? "real_lit"
  val realleq = realliterals ? "leq_real_lit"
  val intleq = intliterals ? "leq_int_lit"
  val natleq = natliterals ? "leq_nat_lit"
  val int = intliterals ? "int_lit"
  val nat = natliterals ? "nat_lit"
  val pos = natliterals ? "pos_lit"
  val succ = natliterals ? "succ_nat_lit"
  val realminus = realliterals ? "minus_real_lit"
  val intminus = intliterals ? "minus_int_lit"
  val sqrt = realliterals ? "sqrt"

  val string = strings ? "string"
  val list = lists ? "list"
  val nil = lists ? "nil"
  val cons = lists ? "cons"
  val vector = vectors ? "vector"
  val zerovec = vectors ? "zerovec"
  val vectorprepend = vectors ? "vector_prepend"
  val matrix = matrices ? "matrix"
  val matrixconst = matrices ? "matrix_const"
}

object Units {
  val dpath = MitM.path / "Units"

  val dimensions = dpath ? "Dimensions"
  val units = dpath ? "Units"

  val qe = units ? "QE"

  val sibase = dpath ? "SIUnits"

  val dimnone = dimensions ? "DimNone"
  val unitnone = sibase ? "Scalar"

  val normeddim = dimensions ? "NormedDimension"
  val normedunit = sibase ? "NormedUnit"

  class Times(path : GlobalName) {
    //val path = dimensions ? "DimTimes"
    val term = OMS(path)

    def apply(ls : Term*) : Term = if (ls.isEmpty) OMS(dimnone) else
      if (ls.length ==1) ls.head
      else ApplySpine(term,ls.head,apply(ls.tail:_*))

    def unapply(t : Term) : Option[List[Term]] = t match {
      case ApplySpine(`term`,List(t1,t2)) => Some(t1 :: unapply(t2).getOrElse(List(t2)))
      case _ => None
    }
  }

  class Div(path : GlobalName) {
    //val path = dimensions ? "DimDiv"
    val term = OMS(path)
    def apply(t1 : Term, t2 : Term) = ApplySpine(term,t1,t2)
    def unapply(t : Term) = t match {
      case ApplySpine(`term`,List(t1,t2)) => Some((t1,t2))
      case _ => None
    }
  }

  object DimTimes extends Times(dimensions ? "DimTimes")
  object DimDiv extends Div(dimensions ? "DimDiv")
  object UnitTimes extends Times(dpath ? "Units" ? "UnitTimes")
  object UnitDiv extends Div(dpath ? "Units" ? "UnitDiv")

  val length = dimensions ? "Length"
  val time = dimensions ? "Time"
  val mass = dimensions ? "Mass"
  val amount = dimensions ? "Amount"
  val temperature = dimensions ? "Temperature"
  val current = dimensions ? "Current"
  val lum = dimensions ? "LuminousIntensity"


  val meter = sibase ? "Meter"
  val second = sibase ? "Second"
  val kg = sibase ? "Kilogram"
  val mole = sibase ? "Mol"
  val kelvin = sibase ? "Kelvin"
  val ampere = sibase ? "Ampere"
  val candela = sibase ? "Candela"


  def isSI(t : GlobalName) = t match {
    case `meter` | `second` | `kg` | `mole` | `kelvin` | `ampere` | `candela` | `unitnone` => true
    case _ => false
  }

}

