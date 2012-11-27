package info.kwarc.mmt.api.parser.test
import info.kwarc.mmt.api._
import utils.MyList._

import scala.annotation.tailrec

case class Ambiguous() extends java.lang.Throwable

trait TokenListElem

case class Token(word: String, first:Int, whitespaceBefore: Boolean) extends TokenListElem {
   override def toString = word //+ "@" + first.toString
}

object TokenList {
   // a simple tokenizer that splits Token's at white space
   def apply(s: String) : TokenList = {
      val l = s.length
      var whitespace = true //there was a whitespace before the current Token
      var i = 0  // position of net Char in s
      var current = "" // previously read prefix of the current Token
      var tokens: List[Token] = Nil // previously read Token's in reverse order
      while (i < l) {
         // whitespace always breaks Token's
         if (s(i).isWhitespace) {
            tokens ::= Token(current, i-current.length, whitespace)
            current = ""
            whitespace = true
            //skip subsequent whitespace
            while (i+1<l && s(i+1).isWhitespace) {
               i += 1
            }
         } else {
            current = current + s(i)
         }
         //go to next Char
         i += 1
      }
      //add the last Token if any
      if (current != "")
         tokens ::= Token(current, i-current.length, whitespace)
      new TokenList(tokens.reverse)
   }
}

/**
 * invariant: tokens.length == an.getFound.length
 * TokenSlice's in an.getFound are invalid 
 */
class MatchedList(var tokens: List[TokenListElem], an: ActiveNotation) extends TokenListElem {
   override def toString = tokens.map(_.toString).mkString("{" + an.notation.name + " ", " ", " " + an.notation.name + "}")
   def scan(nots: List[Notation]) {
      tokens = tokens map {
         case ml: MatchedList =>
            ml.scan(nots)
            ml
         case ul: UnscannedList =>
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
class UnscannedList(tl: TokenList) extends TokenListElem {
   val scanner = new Scanner(tl)
   override def toString = "{ " + tl.toString + " }"
}

class TokenList(private var tokens: List[TokenListElem]) {
   def apply(n: Int) = tokens(n)
   def apply(from: Int, to: Int) = tokens.slice(from, to)
   def update(n: Int, t: TokenListElem) {
      tokens = tokens.take(n) ::: t :: tokens.drop(n+1)
   }
   /**
    * @param an the notation to reduce
    * @return the slice reduced into 1 Token 
    */
   def reduce(an: ActiveNotation): (Int,Int) = {
      val found = an.getFound
      var numReducedTokens = 0
      var newTokens : List[TokenListElem] = Nil
      def doFoundArg(fa: FoundArg) {fa match {case FoundArg(sl, n) =>
            val len = sl.length
            numReducedTokens += len
            if (len == 1) {
               newTokens ::= tokens(sl.start)
            } else {
               newTokens ::= new UnscannedList(new TokenList(sl.toList))
            }
      }}
      found foreach {
         case _:Delim =>
            numReducedTokens += 1
         case fa: FoundArg =>
            doFoundArg(fa)
         case FoundSeqArg(n, args) =>
            args foreach doFoundArg
            numReducedTokens += args.length-1
         case FoundVar(n, vr, tpOpt) =>
            doFoundArg(vr)
            numReducedTokens += 1
            tpOpt foreach doFoundArg
      }
      val from = found.head match {
         case _: Delim => an.firstToken
         case FoundArg(slice,_) => slice.start
         case FoundSeqArg(_, args) => args match {
            case Nil => an.firstToken
            case fa::_ => fa.slice.start
         }
         case FoundVar(_, vr, _) => vr.slice.start
      }
      val to = from + numReducedTokens
      val matched = new MatchedList(newTokens.reverse, an)
      tokens = tokens.take(from) ::: matched :: tokens.drop(to)
      (from,to)
   }
   def length = tokens.length
   override def toString = tokens.mkString("", " ", "") 
}

case class TokenSlice(tokens: TokenList, start: Int, next: Int) {
   override def toString = tokens(start, next).mkString(""," ","")
   def toList = tokens(start,next)
   def length = next - start
}

/** scans a TokenList against some notations
 * @param tl the TokenList to scan
 * matched notations are applied to tl, i.e., tl always holds the current TokenList 
 */
class Scanner(val tl: TokenList) {
   /** logging */
   private def log(s: => String) {
      //println(toString); println(s); println
   }
   override def toString = {
      "token list: " + tl + "\n" +
      "scanner: shifted " + numCurrentTokens.toString + "\n" +
      (active.reverseMap {an =>
          "\t" + an.notation.name + ": " + an.getFound.mkString("", " ", "") + ", shifted: " + an.numCurrentTokens
      } mkString("", "\n", "") )
   }
   /** the notations to scan for */
   private var notations: List[Notation] = Nil
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
   private def checkActive(ans: List[ActiveNotation], closable: Int) : (Int, Boolean) = ans match {
      case Nil => (closable, false)
      case an::rest => 
         if (an.applicable(currentToken)) {
            (closable, true)
         } else {
            if (an.closable) {
               checkActive(rest, closable + 1)
            } else {
               (closable, false)
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
       log("closed current notation, found: " + an)
       val (from,to) = tl.reduce(an)
       log("reduced from " + from + " to " + to)
       val numReducedTokens = to - from
       active = active.tail
       // the closed notation reduces to 1 additional Token that is shifted in the surrounding group 
       currentIndex -= numReducedTokens - 1
       numTokens -= numReducedTokens - 1
   }
   /** close as many active notations as possible
    * @return true if all notations were closed
    */
   private def closeAllPossible: Boolean = {
      active match {
         case Nil => true
         case hd::_ =>
            if (hd.closable) {
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
      tl(currentIndex) match {
         case ml: MatchedList =>
            ml.scan(notations)
            advance
         case ul: UnscannedList =>
            ul.scanner.scan(notations)
            advance
         case t : Token =>
            currentToken = t
            //determine if/how an active notation is applicable
            val (closable, anyActiveApplicable) = checkActive(active, 0)
            log("closable: " + closable + ", next applicable: " + anyActiveApplicable)
            if (anyActiveApplicable) {
               //close the first active notations that are closable, then apply the next one 
               Range(0,closable) foreach {_ => closeFirst(true)}
               applyFirst(false)
            } else {
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
                        an.applicable(currentToken) //true by invariant but must be called for precondition of apply
                        applyFirst(true)
                     case Nil =>
                        //move one token forward
                        advance
                     case _ => throw Ambiguous() //some kind of ambiguity-handling here (maybe look for next delimiter)
                  }
            }
      }
      currentIndex += 1
      //check if there is next token left
      if (currentIndex < numTokens)
        //continue with the next token
        next
      else {
         //close remaining notations
         val allClosed = closeAllPossible
         if (! allClosed) {
            log("backtracking")
            //active notations left, but no further tokens left -> backtracking
            currentIndex = active.head.firstToken + 1
            //numCurrentTokens must be reset as well
            active = active.tail
            next
         }
      }
   }
   /** scans for some notations and applies matches to tl
    * @param nots the notations to scan for
    */
   def scan(nots: List[Notation]) {
      log("scanning " + tl)
      notations = nots
      currentIndex = 0
      next
   }
}

sealed abstract class Marker
case class Delim(s: String) extends Marker with Found {
   override def toString = s
}
case class SecDelim(s: String, wsAllowed: Boolean = true) extends Marker with Found {
   override def toString = (if (wsAllowed) "" else "_") + s
}
/** @param n absolute value is the argument position, negative iff it is in the binding scope */
case class Arg(n: Int) extends Marker {
   override def toString = n.toString
}
/** @param n absolute value is the argument position, negative iff it is in the binding scope
 *  @param sep the delimiter between elements of the sequence 
 */
case class SeqArg(n: Int, sep: Delim) extends Marker {
   override def toString = n.toString + "/" + sep
}
case class Var(n: Int, key: Delim) extends Marker {
   override def toString = n.toString + key + "K"
}
/*
case class SeqVar(n: Int, key: Delim, sep: Delim) extends Marker {
   override def toString = "V" + n + key + "K/" + sep
}
*/

/**
 * helper object 
 */
object Arg {
   private def splitAux(ns: List[Int], ms: List[Marker]) : (List[Int], List[Marker]) = ms match {
      case Arg(n) :: rest => splitAux(n :: ns, rest)
      case rest => (ns.reverse, rest)
   }
   /** splits a List[Marker] into
    *  a List[Int] (possibly Nil) that corresponds to a List[Arg]
    * and the remaining List[Marker]
    */
   def split(ms: List[Marker]) = splitAux(Nil,ms)
}

sealed trait Found
case class FoundArg(slice: TokenSlice, n: Int) extends Found {
   override def toString = n.toString + ":" + slice.toString
}
case class FoundSeqArg(n: Int, args: List[FoundArg]) extends Found {
   override def toString = n.toString + args.map(_.toString).mkString(":(", " ", ")")
}
case class FoundVar(n: Int, vr: FoundArg, tp: Option[FoundArg]) extends Found {
   override def toString = vr.toString + ":" + tp.toString
}

class Notation(val name: String, val markers: List[Marker]) {
   override def toString = "Notation for " + name + ": " + markers.map(_.toString).mkString(" ") 
   // the first delimiter of this notation
   private val firstDelimString : Option[String] = markers mapFind {
      case Delim(s) => Some(s)
      case SeqArg(_, Delim(s)) => Some(s)
      case _ => None
   }
   /** @return firstDelimToken == next */
   def applicable(next: Token): Boolean = {
      firstDelimString == Some(next.word)
   }
   /** creates a new ActiveNotation with this notation's markers */
   def open(scanner: Scanner, firstToken: Int): ActiveNotation = {
      val an = new ActiveNotation(scanner, this, firstToken)
      an
   }
}

object Notation {
   def apply(name: String)(ms: Any*): Notation = {
      val markers : List[Marker] = ms.toList map {
         case i: Int => Arg(i)
         case s: String => Delim(s)
         case m: Marker => m
         case _ => throw ParseError("not a valid marker")
      }
      new Notation(name, markers)
   }
}

/** An ActiveNotation is a notation whose firstDelimToken has been scanned
 *  An ActiveNotation maintains state about the found and still-expected markers
 */
class ActiveNotation(scanner: Scanner, val notation: Notation, val firstToken: Int) {
   // invariant: found.reverse (qua List[Marker]) ::: left == markers

   override def toString = found.reverse.mkString("", " ", "")
   private val markers = notation.markers

   /** the markers that have been found in the token list */
   private var found : List[Found] = Nil
   /** all found tokens */
   def getFound = found.reverse
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
   private def deleteDelim {
      found ::= left.head.asInstanceOf[Found]
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
   private def onApply(act: => Unit): Boolean = {
      remember = _ => act
      true
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
  def applicable(currentToken: Token): Boolean = {
      Arg.split(left) match {
         case (Nil, SeqArg(n, Delim(s)) :: _) if s == currentToken.word =>
              onApply {
                 PickAllSeq(n)
              }
         case (Nil, SeqArg(n, Delim(t)) :: Delim(s) :: _) if t != currentToken.word && s == currentToken.word =>
              if (inSeqArg(n) && numCurrentTokens > 0) {
                 onApply {
                    PickAllSeq(n)
                    delete(1)
                    deleteDelim
                 }
              } else if (! inSeqArg(n) && numCurrentTokens == 0) {
                 onApply {
                    SeqDone(n)
                    delete(1)
                    deleteDelim
                 }
              } else
                 false //abort?
         case (Nil, Var(n, Delim(s)) :: _) if ! inVar(n) && s == currentToken.word =>
              if (numCurrentTokens == 1) {
                 onApply {
                    val vr = FoundArg(scanner.pick(1), n)
                    found ::= FoundVar(n, vr, None)
                 }
              } else {
                 false //abort
              }
         case (Nil, Var(n, _) :: Delim(s) :: _) if inVar(n) && s == currentToken.word =>
                 onApply {
                    found match {
                       case FoundVar(_, vr, _) :: tail =>
                          val fa = FoundArg(scanner.pick(numCurrentTokens), n)
                          found = FoundVar(n, vr, Some(fa)) :: tail
                          delete(1)
                          deleteDelim
                       case _ => // impossible
                    }
                 }
         case (Nil, SecDelim(s, wsAllowed) :: _) =>
             if (s == currentToken.word && (wsAllowed || ! currentToken.whitespaceBefore) && numCurrentTokens == 0) {
                onApply {
                   deleteDelim
                }
             } else {
                false //abort
             }
         case (ns, Delim(s) :: _) if s == currentToken.word => ns match {
            case Nil =>
               onApply {
                  deleteDelim
               }
            case List(n) =>
               onApply {
                  PickAll(n)
                  delete(1)
                  deleteDelim
               }
            case _ if ns.length == numCurrentTokens =>
               onApply {
                  PickSingles(ns)
                  delete(numCurrentTokens)
                  deleteDelim
               }
         }
         case _ => false
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
   def closable : Boolean = {
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
                 false
         case (Nil, Var(_,_) :: _) =>
            // we can never close if we are waiting for a variable
            false
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
         case _ => false
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
