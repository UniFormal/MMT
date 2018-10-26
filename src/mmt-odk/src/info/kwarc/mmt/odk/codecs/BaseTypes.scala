package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api._
import objects._
import uom._
import utils._
import valuebases._
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.odk.LFX.{Append, LFList, ListNil}
import info.kwarc.mmt.odk._
import info.kwarc.mmt.sequences.{NatRules, Sequences}

import scala.collection.mutable
import scala.util.matching.Regex.Match

trait BigIntAsJSON {
  def encodeRep(i: BigInt): JSON = {
    if (i.isValidInt)
      JSONInt(i.toInt)
    else
      JSONString(i.toString)
  }
  def decodeRep(j: JSON): BigInt = j match {
    case JSONInt(i) => i
    case JSONString(s) => BigInt(s)
    case _ => throw CodecNotApplicable
  }
}


object TMInt extends LiteralsCodec[BigInt,JSON](Codecs.standardInt, IntegerLiterals) with BigIntAsJSON
object TMNat extends LiteralsCodec[BigInt,JSON](Codecs.standardNat, NatLiterals) with BigIntAsJSON {
  override def encode(t: Term): JSON = t match {
    case IntegerLiterals(i) if i >= 0 => super.encode(NatLiterals(i))
    case _ => super.encode(t)
  }
}
object TMPos extends LiteralsCodec[BigInt,JSON](Codecs.standardPos, PosLiterals) with BigIntAsJSON

object TMString extends EmbedStringToJSON(new LiteralsAsStringsCodec(Codecs.standardString, StringLiterals))

object BoolAsString extends EmbedStringToJSON(new LiteralsAsStringsCodec(Codecs.boolAsString, MitM.BoolLit))

object BoolAsInt extends LiteralsCodec[java.lang.Boolean,JSON](Codecs.boolAsInt, MitM.BoolLit) {
  def encodeRep(b: java.lang.Boolean) = if (b) JSONInt(1) else JSONInt(0)
  def decodeRep(j: JSON) = j match {
    case JSONInt(x) if x.toInt == 1 => true
    case JSONInt(x) if x.toInt == 0 => false
    case _ => throw CodecNotApplicable
  }
}

object StandardBool extends LiteralsCodec[java.lang.Boolean,JSON](Codecs.standardBool, MitM.BoolLit) {
  def encodeRep(b: java.lang.Boolean) = JSONBoolean(b)
  def decodeRep(j: JSON) = j match {
    case JSONBoolean(b) => b
    case _ => throw CodecNotApplicable
  }
}

object TMList extends ListCodec[JSON](Codecs.standardList, LFList.path, ListNil.path, Append.path) {
  def aggregate(cs: List[JSON]): JSON = JSONArray(cs:_*)
  def separate(j: JSON): List[JSON] = j match {
    case JSONArray(js@_*) => js.toList
    case _ => throw CodecNotApplicable
  }
}

object StandardVector extends CodecOperator[JSON](Codecs.standardVector, MitM.vector) {self =>
  val typeParameterPositions : List[Int] = List(1)

  def aggregate(cs: List[JSON]): JSON = JSONArray(cs:_*)
  def separate(j: JSON): List[JSON] = j match {
    case JSONArray(js@_*) => js.toList
    case _ => throw CodecNotApplicable
  }

  def destruct(tm: Term): List[Term] = tm match {
    case Apply(OMS(MitM.zerovec), _) => Nil
    case ApplySpine(OMS(MitM.vectorprepend), List(_, _, hd, tl)) => hd :: destruct(tl)
  }
  def construct(elemTp: Term, tms: List[Term]): Term = {
    tms.foldLeft[Term](Apply(OMS(MitM.zerovec),elemTp)) {
      case (sofar, next) =>
        ApplySpine(OMS(MitM.vectorprepend), elemTp, NatLiterals.of(BigInt(destruct(sofar).length)), next, sofar)
    }
  }

  def apply(cs: Codec[JSON]*) = {
    val codec = cs.head
    new Codec[JSON](id(codec.exp), tp(codec.tp)) {
      def encode(t: Term) = self.aggregate(self.destruct(t) map codec.encode)
      def decode(c: JSON) = self.construct(codec.tp, self.separate(c) map codec.decode)
    }
  }

}


object StandardMatrix extends CodecOperator[JSON](Codecs.standardMatrix, MitM.matrix) {self =>

  val typeParameterPositions : List[Int] = List(1)

  def aggregate(cs: List[List[JSON]]): JSON = JSONArray(cs.map(l => JSONArray(l:_*)):_*)
  def separate(j: JSON): List[List[JSON]] = j match {
    case JSONArray(js@_*) =>
      js.map({
        case JSONArray(in@_*) => in.toList
        case _ => throw CodecNotApplicable
      }).toList
    case _ => throw CodecNotApplicable
  }

  def destruct(tm: Term): List[List[Term]] = StandardVector.destruct(tm).map(StandardVector.destruct)
  def construct(elemTp: Term, tms: List[List[Term]]): Term = {
    val n = tms.length
    val m = if (n > 1) {
      if (tms.tail.forall(_.length == tms.head.length)) tms.head.length else throw CodecNotApplicable
    } else tms.length
    StandardVector.construct(
      ApplySpine(OMS(MitM.vector), elemTp, NatLiterals.of(m)),
      tms.map(StandardVector.construct(elemTp, _)))
  }

  /*
  def apply(cs: Codec[JSON]*) = {
    val codec = cs.head
    new Codec[JSON](id(codec.exp), tp(codec.tp)) {
      def encode(t: Term) : JSON = self.aggregate(self.destruct(t).map(_.map(codec.encode)))
      def decode(c: JSON) : Term = self.construct(codec.tp, self.separate(c).map(_.map(codec.decode)))
    }
  }*/
  def apply(cs : Codec[JSON]*) = StandardVector(StandardVector(cs.head))
}


object StandardPolynomial extends Codec[JSON](OMS(Codecs.rationalPolynomial), OMS(MitM.polynomials)) { self =>
  val typeParameterPositions : List[Int] = Nil


  def encode(t: Term): JSON = {
    // destruct the polynomial
    val (varName, ints) = destructPolynomial(t)

    // and return the polynomial
    val polyStr = makePoly(
      ints.zipWithIndex.map(pi => (pi._1, List((varName, pi._1))))
    )
    JSONString(polyStr)
  }

  def decode(c: JSON): Term = c match {
    // extract the factors and variable names
    // and then make an actual polynomial term
    case JSONString(s) =>
      val factorMap = extractPolyFactors(parsePoly(s))
      if(factorMap.keys.nonEmpty){
        val varname = factorMap.keys.head
        constructPolynomial(varname, factorMap(varname))
      } else {
        constructPolynomial("x", List(0))
      }
    // case JSONInt(i) => decode(JSONString(i.toString)) // to interpret integers as polynomials
    case _ => throw new Exception(s"not a polynomial: Expected a JSONString, but got a ${c.getClass}: ${c.toCompactString}")
  }

  // Constructs a polynomial out of a list of rational numbers
  private def constructPolynomial(varName: String, ls : List[BigDecimal]) : Term = ApplySpine(
    OMS(MitM.polycons),
    NatLiterals(ls.length),
    OMS(MitM.rationalRing),
    StringLiterals.apply(varName),
    LFList(ls.map(_.toBigInt).map(IntegerLiterals.of))
  )

  // turns a polynomial into a list of rational numbers
  private def destructPolynomial(t : Term): (String, List[BigDecimal]) = t match {
    case ApplySpine(OMS(MitM.polycons), _ :: OMLIT(vname: String, StringLiterals) :: LFList(ls) :: Nil) =>
      (vname, ls.map({ case IntegerLiterals(bi: BigInt) => BigDecimal(bi)}))
    case _ => throw new Exception("not a polynomial")
  }

  /**
    * parses a polynomial into a list of added terms
    * @param s
    * @return
    */
  private def parsePoly(s : String) : List[(BigDecimal, List[(String, BigDecimal)])] = {
    // split by plusses and minus
    val splitRegex = "(?<!\\^)([+-])".r

    splitRegex.split(
      splitRegex.replaceAllIn(
        """\s+""".r.replaceAllIn(s, ""), // replace all the spaces
        "$1$1")               // duplicate +-s
    ) // split
      .map(parsePolyPart).toList // and parse parts
  }

  /**
    * Splits a single part of a polynomial into an integer constant and a list of (variable, power)
    * @param s
    * @return
    */
  private def parsePolyPart(s: String): (BigDecimal, List[(String, BigDecimal)]) = {

    // extract the factor in front of the term
    val factorRE = """^([+-]?\d+(?:\.\d+)?)""".r
    val theFactor: BigDecimal = s match {
      case factorRE(fs: String) => BigDecimal(fs)
      case _ => 1
    }

    // find all the individual parts
    var partRE = """\*?([aA-zZ])(?:\^([+-]?\d+(?:\.\d+)?))?""".r
    val theParts = partRE.findAllMatchIn(factorRE.replaceAllIn(s, "")).map(m =>
      (m.group(1), BigDecimal(Option(m.group(2)).getOrElse("1")))
    )

    // return the extracted factor and list
    (theFactor, theParts.toList)
  }

  /** turns a polynomial into a string */
  private def makePoly(parts: List[(BigDecimal, List[(String, BigDecimal)])]): String = {
    parts.map(makePolyPart).filter(_.nonEmpty).mkString("+")
      .replaceAll("+-", "-").replaceAll("++", "+") // replace all duplicate signs created by .mkString
  }

  /** turns a parsed polynomial part into a string */
  private def makePolyPart(part: (BigDecimal, List[(String, BigDecimal)])): String = {
    val (factor, lst) = part
    if(factor.toIntExact == 0){
      return ""
    }
    factor.toString + "*" + lst.map(f => s"${f._1}^${f._2}").mkString("")
  }

  /** given a parsed set of factors, returns a map of poly factors */
  private def extractPolyFactors(parts: List[(BigDecimal, List[(String, BigDecimal)])]): Map[String, List[BigDecimal]] = {
    val map = mutable.Map[String, mutable.ListBuffer[BigDecimal]]() // the map of a single polynomial

    val powers = parts.flatMap(fl => fl._2.map(_._2))
    val maxIndex = if(powers.isEmpty) 0 else powers.max.toIntExact

    def setPower(variable: String, power: Int, value: BigDecimal): Unit = {
      if(!map.contains(variable)){
        map(variable) = mutable.ListBuffer.fill(maxIndex + 1)(BigDecimal(0))
      }
      map(variable)(power) = value
    }

    // create appropriate buffers for each variable
    parts.foreach(part => {
      if(part._2.isEmpty){
        setPower("x", 0, part._1)
      }
      part._2.foreach(sd => {
        setPower(sd._1, sd._2.toIntExact, part._1)
      })
    })

    // and turn it into something non-mutable
    map.toList.map(kv => (kv._1, kv._2.toList)).toMap
  }
}