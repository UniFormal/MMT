package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._
import utils._

/**
 * codes a list as a string, all elements must be coded as strings
 * @param sep the separator
 */
class ListAsString(tp: GlobalName, nil: GlobalName, cons: GlobalName, sep: Char) extends ListCodec[String](tp, nil, cons) {
   def encode(args: List[String]): String = {
      args.mkString(sep.toString)
   }
   def decode(d: String): Option[List[String]] = {
      val args = stringToList(d, sep.toString)
      Some(args)
   }
}

