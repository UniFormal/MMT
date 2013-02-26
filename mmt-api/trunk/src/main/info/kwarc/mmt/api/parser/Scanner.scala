package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import utils.MyList._

import scala.annotation.tailrec

case class Ambiguous(notations: List[TextNotation]) extends {
   val message = "multiple notations apply: " + notations.map(_.toString).mkString(",")
} with Error(message)

import ActiveNotation._

/** scans a TokenList against some notations
 * @param tl the TokenList to scan
 * matched notations are applied to tl, i.e., tl always holds the current TokenList 
 */
class Scanner(val tl: TokenList, val report: frontend.Report) extends frontend.Logger {
   /** logging */
   val logPrefix = "scanner"
   private def logWithState(s: => String) {
      log(toString); log(s); log("")
   }
   override def toString = {
      "token list: " + tl + "\n" +
      "scanner: shifted " + numCurrentTokens.toString + "\n" +
      (active.reverseMap {an =>
          "\t" + an.notation.name + ": " + an.getFound.mkString("", " ", "") + ", shifted: " + an.numCurrentTokens
      } mkString("", "\n", "") )
   }
   /** the notations to scan for */
   private var notations: List[TextNotation] = Nil
   /** the number of Token's currently in tl */
   private var numTokens = tl.length
   def length = numTokens
   /** the index of the first non-processed Token */
   private var currentIndex: Int = 0
   /** if non-null, the Token at currentIndex */
   private var currentToken : Token = null
   /** the number of Token's before the left-most ActiveNotation */ 
   private var numCurrentTokens = 0

   // these could be moved into a separate class
   /** the number of Token's picked since the last resetPicker */
   private var picked = 0
   /** reset picked */
   private def resetPicker {picked = 0}
   /** obtain picked */
   private def getPicked = picked
   /** pick some of Token's from the end of the shifted and not-yet-picked Token's
    * @param length the number of Token's to pick
    * @return the TokenSlice representing the picked Token's 
    */
   def pick(length: Int): TokenSlice = {
      val sl = TokenSlice(tl, currentIndex-picked-length, currentIndex-picked)
      picked += length
      log("picking " + length + " tokens (" + picked + " in total so far): " + sl)
      sl
   }
   
   /** the currently open notations, inner-most first */
   private var active: List[ActiveNotation] = Nil
   /**
    * precondition: ans.length + closable == active.length
    * @return (n,b):
    *   the first n active notations are closable
    *   b after closing the first n notations, there is an active notation left and it is applicable   
    */
   private def checkActive(ans: List[ActiveNotation], closable: Int) : (Int, Applicability) = ans match {
      case Nil => (closable, NotApplicable)
      case an::rest => 
         an.applicable(currentToken, currentIndex) match {
            case Applicable =>
               (closable, Applicable)
            case Abort =>
               (closable, Abort)
            case NotApplicable =>
               if (an.closable == Applicable) {
                  checkActive(rest, closable + 1)
               } else {
                  (closable, NotApplicable)
               }
         }
   }
   /** applies the first active notation (precondition: must be applicable)
    * @param leftOpen notation may be left-open, i.e., it has not been applied before
    */
   private def applyFirst(leftOpen: Boolean) {
      val an = active.head
      // left-open notations get all Token's that were shifted in the surrounding group
      if (leftOpen) {
         active.tail match {
            case Nil =>
               an.numCurrentTokens = numCurrentTokens
               numCurrentTokens = 0
            case hd::_ =>
               an.numCurrentTokens = hd.numCurrentTokens
               hd.numCurrentTokens = 0
         }
         an.applicable(currentToken, currentIndex) //true by invariant but must be called for precondition of an.apply
      }
      log("applying current notation at " + currentToken + ", found so far: " + an)
      resetPicker
      val toClose = an.apply
      // we count how much active.head picked and
      // give the remaining Tokens back to the surrounding group
      if (leftOpen) {
         active.tail match {
            case Nil =>
               numCurrentTokens = an.numCurrentTokens-getPicked+1
            case hd::_ =>
               hd.numCurrentTokens = an.numCurrentTokens-getPicked+1
         }
      }
      an.numCurrentTokens = 0
      if (toClose)
         closeFirst(false)
   }
   /** closes the first active notation (precondition: must be closable)
    * @param rightOpen true if the notation may still pick from the right during a call to its close method
    */
   private def closeFirst(rightOpen: Boolean) {
       log("closing current notation")
       val an = active.head
       if (rightOpen) {
          resetPicker
          an.close
       }
       active = active.tail
       log("closed current notation, found: " + an)
       val (from,to) = tl.reduce(an)
       val numReducedTokens = to - from
       // the closed notation reduces to 1 additional Token that is shifted in the surrounding group 
       currentIndex -= numReducedTokens - 1
       numTokens -= numReducedTokens - 1
       log("reduced from (inclusive) " + from + " to (inclusive) " + (to-1))
  }
   /** close as many active notations as possible
    * @return true if all notations were closed
    */
   private def closeAllPossible: Boolean = {
      active match {
         case Nil => true
         case hd::_ =>
            if (hd.closable == Applicable) {
               closeFirst(true)
               closeAllPossible
            } else
               false
      }
   }
   private def advance {
      log("shifting 1 Token: " + tl(currentIndex))
      active match {
        case Nil => numCurrentTokens += 1
        case hd::_ => hd.numCurrentTokens += 1
      }
   }
   def scanRecursively(t: TokenListElem) {
      t match {
         case ml: MatchedList =>
            ml.tokens foreach scanRecursively
            ml.flatten
         case ul: UnmatchedList => 
            if (ul.scanner == null) ul.scanner = new Scanner(ul.tl, report) //initialize scanner if necessary
            ul.scanner.scan(notations)
         case e: Escaped =>
         case t: Token =>
            // impossible in MatchedList produced by TokenList.reduce, not called for UnmatchedList or Token
            throw ImplementationError("single Token in MatchedList")
      }
   }
   
   /** tail-recursively going through the Token's */
   @tailrec
   private def next {
      var goToNextToken = true
      tl(currentIndex) match {
         case ml: MatchedList =>
            scanRecursively(ml)
            advance
         case ul: UnmatchedList =>
            scanRecursively(ul)
            advance
         case e: Escaped =>
            advance
         case t : Token =>
            currentToken = t
            //determine if/how an active notation is applicable
            val (closable, anyActiveApplicable) = checkActive(active, 0)
            log("closable: " + closable + ", next applicable: " + anyActiveApplicable)
            anyActiveApplicable match {
               case Abort =>
                  log("aborting")
                  //drop innermost notations up to and including the one to be aborted (= active(closable+1))
                  currentIndex = active(closable).firstToken + 1
                  active = active.drop(closable+1)
                  goToNextToken = false
                  //numCurrentTokens must be reset as well
               case Applicable => 
                  //close the first active notations that are closable, then apply the next one 
                  Range(0,closable) foreach {_ => closeFirst(true)}
                  applyFirst(false)
               case NotApplicable =>
                  //determine if a new notation can be opened 
                  val applicable = notations filter {a => a.applicable(numCurrentTokens, currentToken)}
                  applicable match {
                     case hd :: Nil =>
                        //open the notation and apply it
                        log("opening notation at " + currentToken)
                        if (hd.isLeftOpen) {
                           /* at this point, all active notations are right-open
                            * thus, opening a left-open notation, leads to the ambiguity of association, e.g.,
                            * there is no unique reading for a-b+c
                            * closing all closable notations has the effect of associating to the left, i.e., (a-b)+c
                            * that corresponds to the left-to-right reading convention
                            * alternatively, one could flag an error
                            */
                           Range(0,closable) foreach {_ => closeFirst(true)}
                        }
                        val an = hd.open(this, currentIndex)
                        active ::= an
                        applyFirst(true)
                     case Nil =>
                        //move one token forward
                        advance
                     case l => throw Ambiguous(l) //some kind of ambiguity-handling here (maybe look for next delimiter)
                  }
            }
      }
      if (goToNextToken) {
         currentIndex += 1
         //check if there is next token left
         if (currentIndex < numTokens) {
            next
         } else {
            //close remaining notations
            val allClosed = closeAllPossible
            if (! allClosed) {
               log("backtracking")
               //active notations left, but no further tokens left -> backtracking
               currentIndex = active.head.firstToken + 1
               active = active.tail
               //TODO numCurrentTokens must be reset as well
               next
            }
         }
      } else {
         next
      }
   }
   /** scans for some notations and applies matches to tl
    * @param nots the notations to scan for
    */
   def scan(nots: List[TextNotation]) {
      log("scanning " + tl)
      notations = nots
      currentIndex = 0
      numCurrentTokens = 0
      next
   }
}

/** Objects of type Found represent [[info.kwarc.mmt.api.parser.TokenSlice]]s that were found in the input
 *  
 *  The subclasses correspond to the subclasses of Marker.
 */
sealed abstract class Found {
   /** @return start (inclusive) and end (exclusive) of this Token, None if length 0 */
   def fromTo: Option[(Int, Int)]
}
/** represents a [[info.kwarc.mmt.api.parser.Delimiter]] that was found */
case class FoundDelim(pos: Int, delim: Delimiter) extends Found {
   override def toString = delim.toString
   def fromTo = Some((pos, pos+1))
}
/** represents a [[info.kwarc.mmt.api.parser.Arg]] that was found
 * @param slice the TokenSlice where it was found
 * (as TokenList's are mutable, slice is not necessarily valid in the future)
 * @param n the number of the Arg
 */
case class FoundArg(slice: TokenSlice, n: Int) extends Found {
   override def toString = n.toString + ":" + slice.toString
   def fromTo = Some((slice.start,slice.next))
}
/** represents an [[info.kwarc.mmt.api.parser.SeqArg]] that was found
 * @param n the number of the SeqArg
 * @param args the arguments that were found 
 */
case class FoundSeqArg(n: Int, args: List[FoundArg]) extends Found {
   override def toString = n.toString + args.map(_.toString).mkString(":(", " ", ")")
   def fromTo = if (args.isEmpty) None else Some((args.head.slice.start, args.last.slice.next))
}
/** represents a [[info.kwarc.mmt.api.parser.Var]] that was found
 * @param n the number of the Var
 * @param name the variable name
 * @param tp the found type, if provided 
 */
case class FoundVar(marker: Var, pos: Int, name: Token, tp: Option[FoundArg]) extends Found {
   override def toString = name.toString + marker.key.s + tp.map(_.toString).getOrElse("_")
   def fromTo = if (tp.isEmpty) Some((pos, pos+1)) else Some((pos, tp.get.slice.next))
}
/** represents an [[info.kwarc.mmt.api.parser.SeqVar]] that was found
 * @param n the number of the SeqVar
 * @param vrs the variables that were found 
 */
case class FoundSeqVar(n: Int, vrs: List[FoundVar]) extends Found {
   override def toString = n.toString + ":" + vrs.map(_.toString).mkString("(", " ", ")")
   def fromTo = if (vrs.isEmpty) None else Some((vrs.head.fromTo.get._1, vrs.last.fromTo.get._2))
}