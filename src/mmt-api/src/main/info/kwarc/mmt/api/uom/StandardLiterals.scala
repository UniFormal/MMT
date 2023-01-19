package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import parser._

import SemanticOperator._

object Product {
  val matcher = new utils.StringMatcher2("(",",",")")

  def tensor(left: Unary, right: Unary) = {
    val f = new Product(left.from, right.from)
    val t = new Product(left.to, right.to)
    t.pairOps(f.toLeft compose left, f.toRight compose right)
  }
}

case class Product(val left: SemanticType, val right: SemanticType) extends SemanticType {
  def asString = "(" + left.asString + "*" + right.asString + ")"
  override def valid(u: Any) = u match {
    case (l,r) => left.valid(l) && right.valid(r)
    case _ => false
  }
  override def normalform(u: Any) = u match {
    case (l,r) => (left.normalform(l), right.normalform(r))
  }
  def fromString(s: String) = {
    val (s1,s2) = Product.matcher.unapply(s).get
    (left.fromString(s1), right.fromString(s2))
  }
  def toString(u: Any) = u match {
    case (l,r) => Product.matcher(left.toString(l), right.toString(r))
  }
  override def enumerate(m: Int): Option[Iterator[Any]] = {
    val lE = left.enumerate(m).getOrElse {return None}
    val rE = right.enumerate(m).getOrElse {return None}
    val i = new Iterator[Any] {
      private var lSeen: List[Any] = Nil
      private var rSeen: List[Any] = Nil
      /** a buffer of precomputed values */
      private var nextFromLeft = true
      private var precomputed: List[Any] = Nil
      def hasNext = precomputed.nonEmpty || lE.hasNext || rE.hasNext
      def next() = {
        if (precomputed.nonEmpty) {
          val h = precomputed.head
          precomputed = precomputed.tail
          h
        } else {
          if (nextFromLeft && lE.hasNext) {
            val l = lE.next()
            lSeen ::= l
            precomputed = rSeen map {r => (l,r)}
          } else {
            val r = rE.next()
            rSeen ::= r
            precomputed = lSeen map {l => (l,r)}
          }
          nextFromLeft = ! nextFromLeft
          next()
        }
      }
    }
    Some(i)
  }

  val toLeft = Unary(this, left){case (x,y) => x}
  val toRight = Unary(this, right){case (x,y) => y}

  def pairOps(f1: Unary, f2: Unary): Unary = {
    if (f1.from == f2.from && f1.to == left && f2.to == right) {
      Unary(f1.from, this) {x => (f1.map(x), f2.map(x))}
    } else
      throw ImplementationError("ill-typed")
  }

  override def embed(into: SemanticType) = into match {
    case p: Product => (left.embed(p.left), right.embed(p.right)) match {
      case (Some(eL), Some(eR)) => Some(Product.tensor(eL,eR))
      case _ => None
    }
  }
  override def subtype(that: SemanticType) = that match {
    case p: Product => (p.left subtype left) && (p.right subtype right)
    case _ => false
  }
}

class RProduct[U,V](l: RSemanticType[U], r: RSemanticType[V]) extends Product(l, r) with RSemanticType[(U,V)] {
  val cls = classOf[(U,V)]
}

object ListType {
  val matcher = new utils.StringMatcher2Sep("[",",","]")
}
case class ListType(val over: SemanticType) extends SemanticType {
  def asString = "List[" + over.asString + "]"

  override def valid(u: Any) = u match {
    case us: List[_] => us.forall(over.valid)
    case _ => false
  }

  override def normalform(u: Any) = u match {
    case us: List[_] => us map over.normalform
  }

  def fromString(s: String) = {
    val us = ListType.matcher.unapply(s).get
    us map over.fromString
  }

  def toString(u: Any) = u match {
    case us: List[_] => ListType.matcher(us map over.toString)
  }

  override def enumerate(m: Int): Option[Iterator[Any]] = {
    /**
      * value m determines the maximum size of the generated list
      */
    val el = over.enumerate(1).getOrElse {return None}
    val i = new Iterator[Any] {
      private var curlen = 0
      val rand = scala.util.Random
      private var c = 0
      private var precomputed: List[Any] = Nil

      def hasNext = precomputed.nonEmpty || el.hasNext

      def next() = {
        if (precomputed.nonEmpty) {
          val h = precomputed.head
          precomputed = precomputed.tail
          h
        } else {
          var nli: List[Any] = Nil
          while (c <= curlen) {
            if(el.hasNext){
              nli ::= el.next()
              c += 1
            }
          }
          c = 0
          if(m == 0){
            curlen = rand.nextInt() % 100
          } else{
            curlen = (curlen + 1) % m
          }
          nli
        }
      }
    }
    Some(i)
  }
}
class RList[U](o: RSemanticType[U]) extends ListType(o) with RSemanticType[List[U]] {
  val cls = classOf[List[U]]
}

object TupleType {
  val matcher = new utils.StringMatcher2Sep("(",",",")")
}
case class TupleType(val over: SemanticType, val dim: Int) extends SemanticType {
  def asString = "(" + over.asString + "^" + dim + ")"
  override def valid(u: Any) = u match {
    case us: List[_] if us.length == dim => us.forall(over.valid)
    case _ => false
  }
  override def normalform(u: Any) = u match {
    case us: List[_] => us map over.normalform
  }
  def fromString(s: String) = {
    val us = TupleType.matcher.unapply(s).get
    us map over.fromString
  }
  def toString(u: Any) = u match {
    case us: List[_] => TupleType.matcher(us map over.toString)
  }
  override def enumerate(m: Int): Option[Iterator[Any]] = {
    val el = over.enumerate(m).getOrElse {return None}
    val i = new Iterator[Any] {
      private var c = 0
      private var precomputed: List[Any] = Nil

      def hasNext = precomputed.nonEmpty || el.hasNext

      def next() = {
        if (precomputed.nonEmpty) {
          val h = precomputed.head
          precomputed = precomputed.tail
          h
        } else {
          var nt: List[Any] = Nil
          while (c < dim) {
            if(el.hasNext){
              nt ::= el.next()
              c += 1
            }
          }
          c = 0
          nt
        }
      }
    }
    Some(i)
  }
}
class RTuple[U](o: RSemanticType[U], d: Int) extends TupleType(o,d) with RSemanticType[List[U]] {
  val cls = classOf[List[U]]
}

abstract class Subtype(val of: SemanticType) extends SemanticType {
  def asString = "(a subtype of " + of.asString + ")"
  def by(u: Any): Boolean
  override def valid(u: Any) = of.valid(u) && by(u)
  override def normalform(u: Any) = of.normalform(u)
  def fromString(s: String) = of.fromString(s)
  def toString(u: Any) = of.toString(u)
  override def lex = of.lex
  /** for a finite subtype of an infinite type, hasNext will eventually run forever */
  override def enumerate(m: Int) = of.enumerate(m).map(i => i filter by)

  /** inclusion into the supertype */
  def incl = Unary(this, of){x => x}

  override def embed(into: SemanticType) = super.embed(into) orElse {of.embed(into) map {e => incl compose e}}
  override def subtype(that: SemanticType) =
    (of subtype that) || super.subtype(that)
}
abstract class RSubtype[U](of: RSemanticType[U]) extends Subtype(of) with RSemanticType[U] {
  val cls = of.cls
}

abstract class Quotient(val of: SemanticType) extends SemanticType {
  def asString = "(a quotient of " + of.asString + ")"
  def by(u: Any): Any
  override def valid(u: Any) = of.valid(u)
  override def normalform(u: Any) = by(of.normalform(u))
  def fromString(s: String) = by(of.fromString(s))
  def toString(u: Any) = of.toString(by(u))
  override def lex = of.lex
  /** for a finite quotient of an infinite type, hasNext will eventually run forever */
  override def enumerate(m: Int) = of.enumerate(m).map {i =>
    var seen: List[Any] = Nil
    i filter {u =>
      val uN = normalform(u)
      val take = ! (seen contains uN)
      if (take) seen ::= uN
      take
    }
  }

  /** the projection of an element to its representative */
  def repr = Unary(of, this) {x => normalform(x)}
}
abstract class RQuotient[V](of: RSemanticType[V]) extends Quotient(of) with RSemanticType[V] {
  val cls = of.cls
}


/** types whose underlying representation uses integers */
trait IntegerRepresented extends RSemanticType[BigInt] {
  val cls = classOf[BigInt]
}

/** bundles functions that are typically used when defining literals based on integers */
abstract class IntegerLiteral extends Atomic[BigInt] with IntegerRepresented {
  def fromString(s: String) = BigInt(s)
  override def lex = Some(new parser.NumberLiteralLexer(false,false))
  override def enumerate(m: Int) = {
    val it = new Iterator[BigInt] {
      //TODO for testing, it would help to reach high numbers faster
      private val rand = scala.util.Random
      private var precomputed = 0
      def hasNext = true
      def next() = {
        val i = precomputed
        if(m == 0){
          precomputed = rand.nextInt()
        }
        else{
          precomputed = if (i > 0) -i else -i + m
        }
        i
      }
    }
    Some(it)
  }
}

/** standard integer numbers */
object StandardInt extends IntegerLiteral {
  def asString = "int"
  /** embedding into the rationals */
  override def embed(into: SemanticType) = super.embed(into) orElse {
    if (into == StandardRat)
      Some(Unary(this, into) {x => (x,1)})
    else
      None
  }
}

/** standard natural numbers */
object StandardNat extends RSubtype(StandardInt) {
  override def asString = "nat"
  def by(u: Any) = StandardInt.unapply(u).get >= 0
  //TODO something is going wrong with the RSubtype(StandardInt), and it won't access the enumerate there. For now,
  // I have this.
  override def enumerate(m: Int) = {
    val it = new Iterator[BigInt] {
      private val rand = scala.util.Random
      private var precomputed = 0
      def hasNext = true
      def next() = {
        val i = precomputed
        if(m == 0){
          precomputed = rand.nextInt()
          if(precomputed < 0) precomputed *= -1
        }
        else{
          precomputed = if (i < 0) 0 else i + m
        }
        i
      }
    }
    Some(it)
  }
}

/** standard positive natural numbers */
object StandardPositive extends RSubtype(StandardNat) {
  def by(u: Any) = StandardInt.unapply(u).get != 0
}

/** standard integers modulo, i.e., a finite type of size modulus */
class IntModulo(modulus: Int) extends RQuotient(StandardInt) {
  def by(u: Any) = StandardInt.unapply(u).get mod modulus
  /** overridden for efficiency and to ensure termination */
  override def enumerate(m: Int) = Some((0 until modulus).iterator.map(BigInt(_)))
}

/** standard rational numbers */
object StandardRat extends RQuotient(new RProduct(StandardInt,StandardPositive)) {
  override def asString = "rat"
  def by(u: Any): (BigInt,BigInt) = {
    val (e:BigInt,d:BigInt) = u
    val gcd = e gcd d
    (e / gcd, d / gcd)
  }
  override def toString(u: Any) = {
    val (e:BigInt,d:BigInt) = u
    if (d == 1) e.toString
    else (e.toString + "/" + d.toString)
  }
  private val matcher = utils.StringMatcher("","/","")
  override def fromString(s: String) = s match {
    case this.matcher(e,d) => (StandardInt.fromString(e), StandardNat.fromString(d))
    case s => (StandardInt.fromString(s.trim),BigInt(1))
  }
  override def lex = Some(new parser.NumberLiteralLexer(false,true))

  /** embedding into the complex numbers */
  override def embed(into: SemanticType) = super.embed(into) orElse {
    if (into == ComplexRat)
      Some(Unary(this, into) {x => (x,1)})
    else if (into == StandardDouble)
      Some(Unary(this, into) {case StandardRat(x,y) => x.toDouble/y.toDouble})
    else
      None
  }
}


/** rational complex numbers */
object ComplexRat extends RProduct(StandardRat, StandardRat) {
}
// switched to java.lang.Double, because that's what .toDouble returns and
// java.lang.Double =/= scala.Double (problem in RepresentationType.unapply)
object StandardDouble extends Atomic[java.lang.Double] {
  def asString = "double"
  val cls = classOf[java.lang.Double]
  val key = "OMF"
  def fromString(s: String) = s.toDouble //s.toDouble
  override def lex = Some(new parser.NumberLiteralLexer(true, false, true))
}

object StandardString extends Atomic[String] {
  def asString = "string"
  val cls = classOf[String]
  def fromString(s: String) = s
  override def lex = Some(new SymmetricEscapeLexer('\"', '\\'))
  override def enumerate(m: Int) = {
    val st = new Iterator[String] {
      var c = scala.util.Random.alphanumeric
      private val rand = scala.util.Random
      var len = 0
      var curlen = 0
      def hasNext = true
      def next() = {
        var s1 = ""
        while(len < curlen){
          c = c.tail
          val s2 = c.head.toString
          s1 = s1 + s2
          len += 1
        }
        if(m == 0){
          curlen = rand.nextInt() % 20
        } else {
          curlen = (curlen + 1)%m
        }
        len = 0
        s1
      }
    }
    Some(st)
  }
}

object StringOperations {
  import SemanticOperator._
  private val S = StandardString

  object Empty extends SemanticValue(S, "")

  object Concat extends Binary(S,S,S, {case (S(x),S(y)) => x+y}) {
    def invertLeft(x:Any,r:Any) = (x,r) match {
      case (S(x),S(r)) => if (r.startsWith(x)) Some(r.substring(x.length)) else None
    }
    def invertRight(r:Any,y:Any) = (r,y) match {
      case (S(r),S(y)) => if (r.endsWith(y)) Some(r.substring(0,y.length)) else None
    }
  }
}

object StandardBool extends Atomic[scala.Boolean] {
  def asString = "bool"
  val cls = classOf[scala.Boolean]
  def fromString(s: String) = s match {
    case "true" => true
    case "false" => false
  }
  override def lex = Some(FiniteKeywordsLexer(List("true","false")))
  override def enumerate(m: Int) = Some(List(true,false).iterator)

  // Annoyingly, this seems to be necessary, since at key points, the implicit conversions
  // java.lang.Boolean <=> scala.Boolean are not applied
  override def unapply(u: Any): Option[Boolean] = u match {
    case b : Boolean => Some(b)
    case b : java.lang.Boolean => Some(b)
    case _ => None
  }
}

import utils.URI
/** URI literals, concrete syntax is uri"..." */
object URILiteral extends Atomic[URI] {
  def asString = "uri"
  val cls = classOf[URI]
  def fromString(s: String) = URI(s)
  override def lex = quotedLiteral("uri")
}

/** UUIDs */
object UUIDLiteral extends Atomic[java.util.UUID] {
  def asString = "UUID"
  val cls = classOf[java.util.UUID]
  def fromString(s: String) = java.util.UUID.fromString(s)
  override def lex = quotedLiteral("uuid")
}

/** MMT terms as a semantic type, e.g., for reflection, quotation */
object TermLiteral extends Atomic[Term] {
  def asString = "term"
  val cls = classOf[Term]
  /** MMT parser is not called here as it depends on context */
  def fromString(s: String) = OMSemiFormal(Text("unparsed", s))
  override def atomicToString(t: Term): String = t.toStr(true)
}

/** defines [[SemanticOperator]]s for the standard arithmetic operations */
object Arithmetic {
  import SemanticOperator._
  private val N = StandardNat
  private val Z = StandardInt
  private val Q = StandardRat

  object Zero extends SemanticValue(N,BigInt(0)) {
    //alsoHasType(N)
  }

  object One extends SemanticValue(N,BigInt(1)) {
    //alsoHasType(N)
  }

  object Succ extends InvertibleUnary(Z,Z, {case Z(x) => x+1}, {case Z(x) => Some(x-1)}) {
    alsoHasType(N =>: StandardPositive)
  }

  object Plus extends InvertibleBinary(Z,Z,Z, {case (Z(x),Z(y)) => x+y}) with Commutative {
    alsoHasType(N =>: N =>: N)
    def invertLeft(x:Any,r:Any) = (x,r) match {case (Z(x),Z(r)) => Some(r-x)}
  }

  object Times extends InvertibleBinary(Z,Z,Z, {case (Z(x),Z(y)) => x*y}) with Commutative {
    alsoHasType(N =>: N =>: N)
    def invertLeft(x:Any,r:Any) = (x,r) match {case (Z(x),Z(r)) => if ((r mod x) == 0) Some(r/x) else None}
  }
}


object OpenMath {
  val _path = DPath(utils.URI("http", "www.openmath.org") / "cd") ? "OpenMath"
}
/** OpenMath's literals
  *  These should be moved to an OpenMath plugin, but they are used by the API, e.g., for metadata
  */
object OMLiteral {
  def apply[V <: Any](key: String, semType: Atomic[V]) =
    new RepresentedRealizedType(objects.OMS(OpenMath._path ? key), semType)
  /** OpenMath OMI - unlimited precision integers */
  val OMI = apply("OMI", StandardInt)
  /** OpenMath OMF - IEEE double precision floats */
  val OMF = apply("OMF", StandardDouble)
  /** OpenMath OMSTR - strings */
  val OMSTR = apply("OMSTR", StandardString)
  /** URIs (not actually part of OpenMath) */
  val URI = apply("URI", URILiteral)
}
