package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import uom.OpenMath

/** bundles functions that are typically used when defining literals based on integers */
trait IntegerLiteral extends SemanticType {
   type univ = BigInt
   def fromString(s: String) = s.toInt
   override def lex = Some(new parser.NumberLiteralLexer(false))
}

/** OpenMath OMI - unlimited precision integers */
object OMI extends RealizedType with IntegerLiteral {
   init(OpenMath.OMI.path, OpenMath._path)
}

/** OpenMath OMF - IEEE double precision floats */
object OMF extends RealizedType {
   init(OpenMath.OMF.path, OpenMath._path)
   type univ = Double
   def fromString(s: String) = s.toDouble
   override def lex = Some(new parser.NumberLiteralLexer(true))
}

/** OpenMath OMSTR - strings */ 
object OMSTR extends RealizedType {
   init(OpenMath.OMSTR.path, OpenMath._path)
   type univ = String
   def fromString(s: String) = s
   override def lex = quotedLiteral("")
}

/** URI literals, concrete syntax is uri"..." */
object URILiteral extends RealizedType {
   init(OpenMath._path ? "URI", OpenMath._path)
   type univ = utils.URI
   def fromString(s: String) = utils.URI(s)
   override def lex = quotedLiteral("uri")
}

/** standard natural numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class StandardInt extends RealizedType with IntegerLiteral

/** standard natural numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class StandardNat extends StandardInt {
   override def valid(u: BigInt) = u >= 0
}

/** standard integers modulo, i.e., a finite type of size modulus
 *  
 *  synType must still be set as desired after creating an instance
 */
class IntModulo(modulus: Int) extends RealizedType with IntegerLiteral {
   override def normalform(u: BigInt) = u mod modulus
}

/** standard rational numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class StandardRat extends Product(new StandardNat, new StandardNat) {
   type univS = (BigInt,BigInt) // equal to this.univ but Scala does not know that
   override def valid(u: univ) = u._2 != 0
   override def normalform(u: univ) = {
      val uS = u.asInstanceOf[univS] 
      val gcd = uS._1 gcd uS._2
      (uS._1 / gcd, uS._2 / gcd).asInstanceOf[univ]
   }
}

/** rational complex numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class ComplexRat extends Product(new StandardRat, new StandardRat) {
}
