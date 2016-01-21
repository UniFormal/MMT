package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._
import utils._
import uom._

object Codecs {
   val thy: MPath = ???
   val list = thy ? "list"
   val intAsList = thy ? "IntAsList"
}

/**
 * embeds string encodings into JSON encodings
 */
object StringToJSON extends Embedding[String,JSON] {
   def embed(s: String) = JSONString(s)
   def extract(j: JSON) = j match {
      case JSONString(s) => Some(s)
      case _ => None
   }
}

class BigIntSplitter(base: BigInt) {
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
   /** splits b into sign, number of digits, and list of digits (most to least significant) */
   def split(b: BigInt): (Boolean,List[Int]) = {
      val negative = b.signum == -1
      val digits = toDigits(b.abs)
      (negative, digits.reverse)
   }
   def assemble(negative: Boolean, digits: List[Int]) = {
      val sgn = if (negative) -1 else 1
      sgn * fromDigits(digits.reverse)
   }
}

class BigIntCodec(synType: Term) extends AtomicCodec[BigInt,JSON](Codecs.intAsList, synType, StandardInt) {
   val splitter = new BigIntSplitter(BigInt(1) << 31) // 2^31
   
   def encodeRep(i: BigInt) = {
      val (sgn, abs) = splitter.split(i) // TODO sign currently ignored 
      JSONArray(JSONInt(abs.length) :: abs.map(JSONInt(_)) :_*)
   }
   
   def decodeRep(j: JSON): BigInt = {
      j match {
         case JSONArray(_, jdigits @_*) =>
           val digits = jdigits.toList map {
              case JSONInt(i) => i
              case _ => throw CodecNotApplicable
           }
           splitter.assemble(true, digits)
         case _ => throw CodecNotApplicable
      }
   }
}

/**
 * codes a list as a JSON array
 */
class ListAsArray(tp: GlobalName, nil: GlobalName, cons: GlobalName) extends ListCodec[JSON](Codecs.list, tp, nil, cons) {
   def encode(args: List[JSON]): JSON = {
      JSONArray(args :_*)
   }
   def decode(rep: JSON): Option[List[JSON]] = {
      rep match {
         case JSONArray(args @_*) => Some(args.toList)
         case _ => None
      }
   }
}