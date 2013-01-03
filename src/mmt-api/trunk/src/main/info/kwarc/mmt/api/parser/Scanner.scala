package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import utils.MyList._

import scala.annotation.tailrec

case class Ambiguous() extends java.lang.Throwable

/** the type of objects that may occur in a [[info.kwarc.mmt.api.parser.TokenList]] */
trait TokenListElem

/** A Token is the basic TokenListElem
 * @param word the characters making up the Token (excluding whitespace)
 * @param first the index of the first character (starting from 0)
 * @param whitespaceBefore true iff the Token was preceded by whitespace (true for the first Token, too)
 */
case class Token(word: String, first:Int, whitespaceBefore: Boolean) extends TokenListElem {
   override def toString = word //+ "@" + first.toString
}

/** helper object */
object TokenList {
   import java.lang.Character._
   /** the Tokenizer
    * @param the string to tokenize
    * @return the resulting TokenList
    * 
    * tokens are sequences of letters or individual other Unicode characters
    * 
    * whitespace separates Tokens and is never part of a Token
    * 
    * connectors connect the preceding and the succeeding Tokens into a single Token;
    * the connector is part of the Token
    */
   def apply(s: String) : TokenList = {
      val l = s.length
      // lexing state
      var i = 0        // position of net Char in s
      var current = "" // previously read prefix of the current Token
      var connect = false // current.last.getType == CONNECTOR_PUNCTUATION
      var whitespace = true //there was a whitespace before the current Token
      var tokens : List[Token] = Nil // Token's found so far in reverse order
      // the lexing loop
      while (i < l) {
         val c = s(i) // current Char
         val tp = c.getType // current Char's type
         // whitespace always starts a new Token, 
         if (c.isWhitespace) {
            if (current != "") {
               tokens ::= Token(current, i-current.length, whitespace)
               current = ""
            } 
            whitespace = true
         } else {
            // we are in a multi-character Token
            tp match {
               // after a connector, everything continues the Token
               case _ if connect =>
                  current += c
                  connect = false
               // letters, marks, and numbers continue the Token
               case _ if c.isLetter =>
                  current += c
               case COMBINING_SPACING_MARK | ENCLOSING_MARK | NON_SPACING_MARK =>
                  current += c
               case DECIMAL_DIGIT_NUMBER | LETTER_NUMBER | OTHER_NUMBER =>
                  current += c
               // connectors are remembered
               case CONNECTOR_PUNCTUATION =>
                  current += c
                  connect = true
               // everything else:
               case _ =>
                  // end previous Token, if any
                  if (current != "") {
                     tokens ::= Token(current, i-current.length, whitespace)
                     current = ""
                     whitespace = false
                  }
                  // look ahead: if a connector follows, start a multi-character Token
                  // otherwise, create a single-character Token
                  if (i < l-1 && s(i+1).getType == CONNECTOR_PUNCTUATION) {
                     current += c
                  } else {
                     tokens ::= Token(c.toString, i, whitespace)
                  }
            }
            whitespace = false
         }
         i += 1
      }
      //add the last Token, if any
      if (current != "")
         tokens ::= Token(current, i-current.length, whitespace)
      new TokenList(tokens.reverse)
   }
}

/**
 * A MatchedList is a TokenListElem resulting by reducing a sublist using a notation
 * @param tokens the TokenListElem's that were reduced
 * @param an the notation used for reduction (which stores the information about how the notation matched the tokens)
 * 
 * invariant: tokens.length == an.getFound.length
 * 
 * TokenSlice's in an.getFound are invalid 
 */
class MatchedList(var tokens: List[TokenListElem], val an: ActiveNotation) extends TokenListElem {
   override def toString = tokens.map(_.toString).mkString("{" + an.notation.name + " ", " ", " " + an.notation.name + "}")
   def scan(nots: List[TextNotation]) {
      tokens = tokens map {
         case ml: MatchedList =>
            ml.scan(nots)
            ml
         case ul: UnmatchedList =>
            ul.scanner.scan(nots)
            if (ul.scanner.length == 1)
               ul.scanner.tl(0)
            else
               ul
         case t: Token =>
            t
      }
   }
}

/**
 * An UnmatchedList is a TokenListElem resulting by reducing a sublist without using a notation
 * 
 * Other notations have determined that this sublist must be parsed into a subtree, but it is
 * not known (yet) which notation should be used.
 * 
 * @param tokens the TokenListElem's that are to be reduced
 */
class UnmatchedList(val tl: TokenList) extends TokenListElem {
   var scanner: Scanner = null
   override def toString = "{ " + tl.toString + " }"
}

/** A TokenList is a wrapper for a mutable list of TokenListElem with a few special methods for parsing
 * 
 * @param tokens the underlying list
 * 
 * TokenListElem's are always indexed starting from 0.
 */
class TokenList(private var tokens: List[TokenListElem]) {
   /** returns a Token in a given position */
   def apply(n: Int) = tokens(n)
   /** returns a sublist of elements */
   def apply(from: Int, to: Int) = tokens.slice(from, to)
   /**
    * @param an the notation to reduce
    * @return the slice reduced into 1 Token 
    */
   def reduce(an: ActiveNotation): (Int,Int) = {
      val found = an.getFound
      var newTokens : List[TokenListElem] = Nil
      def doFoundArg(fa: FoundArg) {fa match {case FoundArg(sl, n) =>
            if (sl.length == 1) {
               newTokens ::= tokens(sl.start)
            } else {
               newTokens ::= new UnmatchedList(new TokenList(sl.toList))
            }
      }}
      found foreach {
         case _:FoundDelim =>
         case fa: FoundArg =>
            doFoundArg(fa)
         case FoundSeqArg(n, args) =>
            args foreach doFoundArg
         case FoundVar(n, vr, tpOpt) =>
            doFoundArg(vr)
            tpOpt foreach doFoundArg
      }
      val (from,to) = an.fromTo
      val matched = new MatchedList(newTokens.reverse, an)
      tokens = tokens.take(from) ::: matched :: tokens.drop(to)
      (from,to)
   }
   /** the length of the list */
   def length = tokens.length
   override def toString = tokens.mkString("", " ", "") 
}

/** A reference to a sublist of a TokenList that does not copy the element
 * @param tokens the underlying TokenList
 * @param start the first Token (inclusive)
 * @param next the last Token (exclusive)
 */
case class TokenSlice(tokens: TokenList, start: Int, next: Int) {
   override def toString = tokens(start, next).mkString(""," ","")
   /** derefences the TokenSlice and returns the list */
   def toList = tokens(start,next)
   /** the length of the slice */
   def length = next - start
}

/** a helper object */
object ActiveNotation {
   /** a type to express the result of testing whether a notation is applicable
    * 
    * essentially, a Notation is applicable if the next Token matches the next Delimiter it expects
    */
   type Applicability = Int
   /** a value of type Applicability expressing that the notation is applicable */
   val Applicable = 1
   /** a value of type Applicability expressing that the notation is not applicable now
    * (but may be applicable later if the current Token is shifted)
    */
   val NotApplicable = 0
   /** a value of type Applicability expressing that the notation cannot be applied anymore
    * (and parsing should backtrack)
    */
   val Abort = -1
}

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
   /** applies the first active notation (precondition: must be applicable) */
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
   /** closes the first active notation (precondition: must be closable) */
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
   /** tail-recursively going through the Token's */
   @tailrec
   private def next {
      var goToNextToken = true
      tl(currentIndex) match {
         case ml: MatchedList =>
            ml.scan(notations)
            advance
         case ul: UnmatchedList =>
            if (ul.scanner == null) ul.scanner = new Scanner(ul.tl, report) //initialize scanner if necessary
            ul.scanner.scan(notations)
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
                  val applicable = notations filter {a => a.applicable(currentToken)}
                  applicable match {
                     case hd :: Nil =>
                        //open the notation and apply it
                        /* opening a left-open notation when the current notation is closable (e.g., because it's right-open)
                           should be an error: no unique reading for - a + b */
                        log("opening notation at " + currentToken)
                        val an = hd.open(this, currentIndex)
                        active ::= an
                        an.applicable(currentToken, currentIndex) //true by invariant but must be called for precondition of apply
                        applyFirst(true)
                     case Nil =>
                        //move one token forward
                        advance
                     case _ => throw Ambiguous() //some kind of ambiguity-handling here (maybe look for next delimiter)
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
   override def toString = "D:" + delim
   def fromTo = Some((pos, pos+1))
}
/** represents a [[info.kwarc.mmt.api.parser.Arg]] that was found
 * @param slice the TokenSlice where it was found 
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
 * @param vr the found variable
 * @param tp the found type, if provided 
 */
case class FoundVar(n: Int, vr: FoundArg, tp: Option[FoundArg]) extends Found {
   override def toString = vr.toString + ":" + tp.map(_.toString).getOrElse("_")
   def fromTo = if (tp.isEmpty) vr.fromTo else Some((vr.slice.start, tp.get.slice.next))
}
/** represents an [[info.kwarc.mmt.api.parser.SeqVar]] that was found
 * @param n the number of the SeqVar
 * @param vrs the variables that were found 
 */
case class FoundSeqVar(n: Int, vrs: List[FoundVar]) extends Found {
   override def toString = n.toString + ":" + vrs.map(_.toString).mkString("(", " ", ")")
   def fromTo = if (vrs.isEmpty) None else Some((vrs.head.fromTo.get._1, vrs.last.fromTo.get._2))
}


/** An ActiveNotation is a notation whose firstDelimToken has been scanned
 *  An ActiveNotation maintains state about the found and still-expected markers
 */
class ActiveNotation(scanner: Scanner, val notation: TextNotation, val firstToken: Int) {
   // invariant: found.reverse (qua List[Marker]) ::: left == markers

   override def toString = found.reverse.mkString("", " ", "")
   private val markers = notation.markers

   /** the markers that have been found in the token list */
   private var found : List[Found] = Nil
   /** all found tokens */
   def getFound = found.reverse
   /** @return start (inclusive) and end (exclusive) of this notation
    * only well-defined if the notation is closed
    */
   def fromTo : (Int, Int) = {
      val from = found.reverse.mapFind {f => f.fromTo map(_._1)}.get
      val to   = found.mapFind {f => f.fromTo map(_._2)}.get
      (from,to)
   }
   /** the markers that are still expected */
   private var left : List[Marker] = markers
   /** the number of tokens that are part of the current argument(s),
    * i.e., the tokens scanned since the previously found delimiter */
   var numCurrentTokens : Int = 0

   /** remove n Markers from left */
   private def delete(n: Int) {
      left = left.drop(n)
   }
   /** move a delimiter from left to found */
   private def deleteDelim(index: Int) {
      found ::= FoundDelim(index, left.head.asInstanceOf[Delimiter])
      left = left.tail
   }
   /** pick all available Token's as Arg(n) */
   private def PickAll(n: Int) {
      found ::= FoundArg(scanner.pick(numCurrentTokens), n)
   }
   /** pick the exactly ns.length available Token's as ns.map(Arg(_)) */
   private def PickSingles(ns: List[Int]) {
      val fs = ns reverseMap {n => FoundArg(scanner.pick(1), n)}
      found = fs ::: found
   }
   
   private def PickAllSeq(n: Int) {
      val a = FoundArg(scanner.pick(numCurrentTokens), n)
      found.headOption match {
         case Some(FoundSeqArg(m, args)) if m == n =>
            found = FoundSeqArg(n, args ::: List(a)) :: found.tail
         case _ =>
            found ::= FoundSeqArg(n, List(a))
      }
   }
   private def SeqDone(n: Int) {
      found.headOption match {
         case Some(FoundSeqArg(m, _)) if m == n =>
         case _ =>
            found ::= FoundSeqArg(n, Nil)
      }
   }
   
   /** remember stores operations for picking arguments
    *  these are remembered between calls of successive invocations of
    *  applicable-apply and closable-close
    *  only well-defined after applicable or closable returned true
    */
   private var remember : Unit => Unit = null
   
   /** stores an operation in remember for later execution */
   private def onApply(act: => Unit): Applicability = {
      remember = _ => act
      Applicable
   }
   
   private def inSeqArg(n: Int) = found match {
      case FoundSeqArg(m, _) :: _ if m == n => true
      case _ => false
   }
   private def inVar(n: Int) = found match {
      case FoundVar(m, _, None) :: _ if m == n => true
      case _ => false
   }
  /**
   * @param currentToken the currently scanned token
   * @return true if the notation can be applied at this point
   * i.e., currentToken is (one of the) delimiter(s) expected next and currentTokens matches the currently expected arguments
   */
  def applicable(currentToken: Token, currentIndex: Int): Applicability = {
      Arg.split(left) match {
         case (ns, Delim(s) :: _) if s == currentToken.word => ns match {
            case Nil =>
               onApply {
                  deleteDelim(currentIndex)
               }
            case List(n) =>
               onApply {
                  PickAll(n)
                  delete(1)
                  deleteDelim(currentIndex)
               }
            case _ if ns.length == numCurrentTokens =>
               onApply {
                  PickSingles(ns)
                  delete(numCurrentTokens)
                  deleteDelim(currentIndex)
               }
         }
         case (Nil, SecDelim(s, wsAllowed) :: _) =>
             if (s == currentToken.word && (wsAllowed || ! currentToken.whitespaceBefore) && numCurrentTokens == 0) {
                onApply {
                   deleteDelim(currentIndex)
                }
             } else {
                Abort
             }
         case (Nil, SeqArg(n, Delim(s)) :: _) if s == currentToken.word =>
              onApply {
                 PickAllSeq(n)
              }
         case (Nil, SeqArg(n, Delim(t)) :: Delim(s) :: _) if t != currentToken.word && s == currentToken.word =>
              if (inSeqArg(n) && numCurrentTokens > 0) {
                 onApply {
                    PickAllSeq(n)
                    delete(1)
                    deleteDelim(currentIndex)
                 }
              } else if (! inSeqArg(n) && numCurrentTokens == 0) {
                 onApply {
                    SeqDone(n)
                    delete(1)
                    deleteDelim(currentIndex)
                 }
              } else
                 NotApplicable //abort?
         case (Nil, Var(n, Delim(s)) :: _) if ! inVar(n) && s == currentToken.word =>
              if (numCurrentTokens == 1) {
                 onApply {
                    val vr = FoundArg(scanner.pick(1), n)
                    found ::= FoundVar(n, vr, None)
                 }
              } else {
                 NotApplicable //abort
              }
         case (Nil, Var(n, _) :: Delim(s) :: _) if inVar(n) && s == currentToken.word =>
                 onApply {
                    found match {
                       case FoundVar(_, vr, _) :: tail =>
                          val fa = FoundArg(scanner.pick(numCurrentTokens), n)
                          found = FoundVar(n, vr, Some(fa)) :: tail
                          delete(1)
                          deleteDelim(currentIndex)
                       case _ => // impossible
                    }
                 }
         case _ => NotApplicable
      }
   }
   /**
    * precondition: this.applicable(scanner.currentToken)
    * terminate the current argument(s) and match the current token to the next expected delimiter
    * @return true iff the notation is fully applied, i.e., no further arguments or delimiters can be matched  
    */
   def apply: Boolean = {
      remember()
      if (left.isEmpty) {
         true
      } else {
         false
      }
   }
   /**
    * @return true if the notation can close
    * i.e., the current arguments can be the last arguments of the notation with no further delimiter expected
    */
   def closable : Applicability = {
      Arg.split(left) match {
         case (Nil, SeqArg(n, Delim(s)) :: Nil) =>
              if (inSeqArg(n) && numCurrentTokens > 0) {
                 onApply {
                    PickAllSeq(n)
                    delete(1)
                 }
              } else if (! inSeqArg(n) && numCurrentTokens == 0) {
                 onApply {
                    SeqDone(n)
                    delete(1)
                 }
              } else
                 NotApplicable
         case (Nil, Var(_,_) :: _) =>
            // we can never close if we are waiting for a variable
            NotApplicable
         case (List(n), Nil) =>
            // one argument taking all available Token's
            onApply {
               PickAll(n)
               delete(1)
            }
            // as many arguments as there are Token's
            // should we abort immediately if the numbers do not match up?
         case (ns, Nil) if ns.length == numCurrentTokens =>    
             onApply {
                PickSingles(ns)
                delete(ns.length)
             }
         case _ => NotApplicable
      }
   }
   /**
    * precondition: this.closable
    * terminate the current argument(s)
    */
   def close {
      remember()
   }
}