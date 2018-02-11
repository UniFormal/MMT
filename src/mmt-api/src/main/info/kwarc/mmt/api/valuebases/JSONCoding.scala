package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._
import utils._
import uom._

/**
 * embeds string encodings into JSON encodings
 */
class EmbedStringToJSON(c: Codec[String]) extends Embedding[String,JSON](c) {
   def embed(s: String) = JSONString(s)
   def extract(j: JSON) = j match {
      case JSONString(s) => Some(s)
      case _ => None
   }
}

/** auxiliary class for transforming integer into different bases */
class BigIntSplitter(base: Int) {
   /** the representation of b>=0 in base 'base' using positive Ints as digits (least to most) */
   private def toDigits(b: BigInt): List[Int] = {
      if (b == 0) Nil
      else {
         val (div,mod) = b /% base
         mod.toInt :: toDigits(div)
      }
   }
   /** inverse of toDigits */
   private def fromDigits(ds: List[Int]): BigInt = {
      ds match {
         case Nil => 0
         case hd::tl => fromDigits(tl)*base + hd
      }
   }
   /** splits b into sign and list of digits (most to least significant) */
   def split(b: BigInt): (Int,List[Int]) = {
      val digits = toDigits(b.abs)
      (b.signum, digits.reverse)
   }
   /** inverse of split, per: sign in {-1,0,1} */
   def assemble(sign: Int, digits: List[Int]) = {
      sign * fromDigits(digits.reverse)
   }
}

/** encodes positive integers as list of (sign * number of digits) :: (digits most to least) relative to base 2^31
 *
 * Note that this encoding has the property that <= on integers corresponds to lexicographic ordering on lists.  
 */ 
class BigIntCodec(id: GlobalName, synType: Term) extends LiteralsCodec[BigInt,JSON](id, new RepresentedRealizedType(synType, StandardInt)) {
   val splitter = new BigIntSplitter(1 << 31) // 2^31

   def encodeRep(i: BigInt) = {
      val (sgn, abs) = splitter.split(i)
      JSONArray(JSONInt(sgn * abs.length) :: abs.map(JSONInt(_)) :_*)
   }

   def decodeRep(j: JSON): BigInt = {
      j match {
         case JSONArray(sgnLength, jdigits @_*) =>
           val sign = sgnLength match {
             case JSONInt(i) => i.signum
             case _ =>  throw CodecNotApplicable
           }
           val digits = jdigits.toList map {
              case JSONInt(i) => i
              case _ => throw CodecNotApplicable
           }
           splitter.assemble(sign, digits)
         case _ => throw CodecNotApplicable
      }
   }
}
