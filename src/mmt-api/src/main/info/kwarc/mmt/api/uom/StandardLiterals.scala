package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
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
class Product(val left: SemanticType, val right: SemanticType) extends SemanticType {
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
   override def enumerate: Option[Iterator[Any]] = {
     val lE = left.enumerate.getOrElse {return None}
     val rE = right.enumerate.getOrElse {return None}
     val i = new Iterator[Any] {
       private var lSeen: List[Any] = Nil
       private var rSeen: List[Any] = Nil
       /** a buffer of precomputed values */
       private var nextFromLeft = true
       private var precomputed: List[Any] = Nil
       def hasNext = precomputed.nonEmpty || lE.hasNext || rE.hasNext
       def next = {
         if (precomputed.nonEmpty) {
           val h = precomputed.head
           precomputed = precomputed.tail
           h
         } else {
           if (nextFromLeft && lE.hasNext) {
             val l = lE.next
             lSeen ::= l
             precomputed = rSeen map {r => (l,r)}
           } else {
             val r = rE.next
             rSeen ::= r
             precomputed = lSeen map {l => (l,r)}
           }
           nextFromLeft = ! nextFromLeft
           next
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
}

class RProduct[U,V](l: RSemanticType[U], r: RSemanticType[V]) extends Product(l, r) with RSemanticType[(U,V)] {
  val cls = classOf[(U,V)]
}

object ListType {
   val matcher = new utils.StringMatcher2Sep("[",",","]")
}
class ListType(val over: SemanticType) extends SemanticType {
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
}
class RList[U](o: RSemanticType[U]) extends ListType(o) with RSemanticType[List[U]] {
  val cls = classOf[List[U]]
}

object TupleType {
   val matcher = new utils.StringMatcher2Sep("(",",",")")
}
class TupleType(val over: SemanticType, val dim: Int) extends SemanticType {
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
}
class RTuple[U](o: RSemanticType[U], d: Int) extends TupleType(o,d) with RSemanticType[List[U]] {
  val cls = classOf[List[U]]
}

abstract class Subtype(val of: SemanticType) extends SemanticType {
   def by(u: Any): Boolean
   override def valid(u: Any) = of.valid(u) && by(u)
   override def normalform(u: Any) = of.normalform(u)
   def fromString(s: String) = of.fromString(s)
   def toString(u: Any) = of.toString(u)
   override def lex = of.lex
   /** for a finite subtype of an infinite type, hasNext will eventually run forever */ 
   override def enumerate = of.enumerate.map(i => i filter by)
   
   /** inclusion into the supertype */
   def incl = Unary(this, of){x => x}
   
   override def embed(into: SemanticType) = super.embed(into) orElse {of.embed(into) map {e => incl compose e}}
}
abstract class RSubtype[U](of: RSemanticType[U]) extends Subtype(of) with RSemanticType[U] {
  val cls = of.cls
}

abstract class Quotient(val of: SemanticType) extends SemanticType {
   def by(u: Any): Any
   override def valid(u: Any) = of.valid(u)
   override def normalform(u: Any) = by(of.normalform(u))
   def fromString(s: String) = by(of.fromString(s))
   def toString(u: Any) = of.toString(by(u))
   override def lex = of.lex
   /** for a finite quotient of an infinite type, hasNext will eventually run forever */ 
   override def enumerate = of.enumerate.map {i =>
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

abstract class Atomic[V] extends RSemanticType[V] {
   override def valid(u: Any) = unapply(u).isDefined
   def toString(u: Any) = unapply(u).get.toString
   /** narrower type */
   def fromString(s: String): V
}

trait IntegerRepresented extends RSemanticType[BigInt] {
  val cls = classOf[BigInt]
}

/** bundles functions that are typically used when defining literals based on integers */
abstract class IntegerLiteral extends Atomic[BigInt] with IntegerRepresented {
   def fromString(s: String) = BigInt(s)
   override def lex = Some(new parser.NumberLiteralLexer(false,false))
   override def enumerate = {
     val it = new Iterator[BigInt] {
       //TODO for testing, it would help to reach high numbers faster
       private var precomputed = 0
       def hasNext = true
       def next = {
         val i = precomputed
         precomputed = if (i > 0) -i else -i+1
         i
       }
     }
     Some(it)
   }
}

/** standard integer numbers */
object StandardInt extends IntegerLiteral {
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
   def by(u: Any) = StandardInt.unapply(u).get >= 0
}

/** standard positive natural numbers */
object StandardPositive extends RSubtype(StandardNat) {
   def by(u: Any) = StandardInt.unapply(u).get != 0
}

/** standard integers modulo, i.e., a finite type of size modulus */
class IntModulo(modulus: Int) extends RQuotient(StandardInt) {
   def by(u: Any) = StandardInt.unapply(u).get mod modulus
   /** overridden for efficiency and to ensure termination */
   override def enumerate = Some((0 until modulus).iterator)
}

/** standard rational numbers */
object StandardRat extends RQuotient(new RProduct(StandardInt,StandardPositive)) {
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
      case s => (StandardInt.fromString(s.trim),1)
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
   val cls = classOf[java.lang.Double]
   val key = "OMF"
   def fromString(s: String) = s.toDouble //s.toDouble
   override def lex = Some(new parser.NumberLiteralLexer(true, false))
}

object StandardString extends Atomic[String] {
   val cls = classOf[String]
   def fromString(s: String) = s
   override def lex = quotedLiteral("")
}

object StandardBool extends Atomic[java.lang.Boolean] {
   val cls = classOf[java.lang.Boolean]
   def fromString(s: String) = s match {
     case "true" => true
     case "false" => false
   }
   override def lex = Some(FiniteKeywordsLexer(List("true","false")))
   override def enumerate = Some(List(true,false).iterator)
}

import utils.URI
/** URI literals, concrete syntax is uri"..." */
object URILiteral extends Atomic[URI] {
   val cls = classOf[URI]
   def fromString(s: String) = URI(s)
   override def lex = quotedLiteral("uri")
}

/** defines [[SemanticOperator]]s for the standard arithmetic operations */
object Arithmetic {
  import SemanticOperator._
  private val N = StandardNat
  private val Z = StandardInt
  private val Q = StandardRat

  object Zero extends Value(Z)(0) {
    alsoHasType(N)
  }

  object One extends Value(Z)(1) {
    alsoHasType(N)
  }

  object Succ extends InvertibleUnary(Z,Z, {case Z(x) => x+1}, {case Z(x) => Some(x-1)}) {
    alsoHasType(N =>: N)
  }

  object Plus extends InvertibleBinary(Z,Z,Z, {case (Z(x),Z(y)) => x+y}) with Commutative {
    def invertLeft(x:Any,r:Any) = (x,r) match {case (Z(x),Z(r)) => Some(r-x)}
  }
  
  object Times extends InvertibleBinary(Z,Z,Z, {case (Z(x),Z(y)) => x*y}) with Commutative {
    def invertLeft(x:Any,r:Any) = (x,r) match {case (Z(x),Z(r)) => if ((r mod x) == 0) Some(r/x) else None}
  }
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