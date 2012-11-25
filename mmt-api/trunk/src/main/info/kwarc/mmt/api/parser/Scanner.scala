package info.kwarc.mmt.api.parser.test
import info.kwarc.mmt.api._

case class Ambiguous() extends java.lang.Throwable

trait TokenListElem
case class Token(word: String, first:Int) extends TokenListElem {
   override def toString = word + "@" + first.toString
}

object TokenList {
   // a simple tokenizer that splits Token's at white space
   def apply(s: String) : TokenList = {
      val l = s.length
      var i = 0  // position of net Char in s
      var current = "" // previously read prefix of the current Token
      var tokens: List[Token] = Nil // previously read Token's in reverse order
      while (i < l) {
         // whitespace always breaks Token's
         if (s(i).isWhitespace) {
            tokens ::= Token(current, i-current.length)
            current = ""
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
         tokens ::= Token(current, i-current.length)
      new TokenList(tokens.reverse)
   }
}

/**
 * invariant: tokens.length == an.getFound.length
 * TokenSlice's in an.getFound are invalid 
 */
class MatchedList(tokens: List[TokenListElem], an: ActiveNotation) extends TokenListElem {
  override def toString = "(" + tokens.mkString("", " ", "") + " matched" + ")"
}

class TokenList(private var tokens: List[TokenListElem]) extends TokenListElem {
   def apply(n: Int) = tokens(n)
   def apply(from: Int, to: Int) = tokens.slice(from, to)
   def reduce(an: ActiveNotation) {
      val found = an.getFound
      val numTokens = found.foldLeft(0) {
        case (a, _:Delim) => a+1
        case (a, FoundArg(slice,_)) => a+slice.length
      }
      val from = found.head match {
         case _: Delim => an.firstToken
         case FoundArg(slice,_) => slice.start
      }
      val to = from + numTokens
      val matched = new MatchedList(tokens.slice(from,to), an)
      println("reducing: " + from + " " + to)
      println(tokens)
      tokens = tokens.take(from) ::: matched :: tokens.drop(to)
      println(tokens)
   }
   def length = tokens.length
   override def toString = "(" + tokens.mkString("", " ", "") + ")" 
}

case class TokenSlice(tokens: TokenList, start: Int, next: Int) {
   override def toString = tokens(start, next).mkString(""," ","")
   def length = next - start
}

/** scans a TokenList against some notations
 * @param tl the TokenList to scan
 * matched notations are applied to tl, i.e., tl always holds the current TokenList 
 */
class Scanner(val tl: TokenList) {
   /** logging */
   private def log(s: => String) {println(s)}
   /** the notations to scan for */
   private var notations: List[Notation] = Nil
   /** the number of Token's currently in tl */
   private def numTokens = tl.length
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
      log("picking " + sl)
      picked += length
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
      log("applying current notation at " + currentToken + ", found so far: " + an)
      // left-open notations get all Token's that were shifted before active.head was opened ...
      if (leftOpen) {
         active.tail match {
            case Nil => an.numCurrentTokens = numCurrentTokens
            case hd::_ => an.numCurrentTokens = hd.numCurrentTokens
         }
      }
      resetPicker
      val toClose = an.apply
      // we count how much active.head picked and
      // then give the remaining Tokens to the surrounding group
      if (leftOpen) {
         active.tail match {
            // -1: we add one Token to the group for the eventual result of active.head 
            case Nil => numCurrentTokens -= getPicked-1
            case hd::_ => hd.numCurrentTokens -= getPicked-1
         }
      }
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
       tl.reduce(an)
       active = active.tail
   }
   private def advance {
      active match {
        case Nil => numCurrentTokens += 1
        case hd::_ => hd.numCurrentTokens += 1
      }
   }
   private def next {
      while (! tl(currentIndex).isInstanceOf[Token]) {
         currentIndex += 1
         advance
      }
      currentToken = tl(currentIndex).asInstanceOf[Token]
      //determine if/how an active notation is applicable
      val (closable, anyActiveApplicable) = checkActive(active, 0)
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
                  log("opened notation at " + currentToken)
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
      currentIndex += 1
      //check if there is next token left
      if (currentIndex < numTokens)
        //continue with the next token
        next
      else {
         //check if all active notations can be closed
         //TODO: cover the weird case where a new notation was opened in the current invocation of the method and not immediately closed
         if (closable == active.length) {
            //close them and we're done
            Range(0,closable) foreach {_ => closeFirst(true)}
         } else {
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
      next
   }
}

abstract class Marker
case class Delim(s: String) extends Marker with Found {
   override def toString = s
}
case class Arg(n: Int) extends Marker {
   override def toString = n.toString
}
case class SqArg(n: Int, sep: Delim) extends Marker

trait Found
case class FoundArg(slice: TokenSlice, n: Int) extends Found {
   override def toString = n.toString + ":" + slice.toString
}
case class FoundSeqArg(slice: TokenSlice, delims: List[Delim], args: List[FoundArg]) extends Found

class Notation(markers: List[Marker]) {
   // the first delimiter of this notation
   private val firstDelimString : String = {
      markers find {_.isInstanceOf[Delim]} match {
         case Some(Delim(s)) => s
         case _ => null 
      }
   }
   /** @return firstDelimToken == next */
   def applicable(next: Token): Boolean = {
      firstDelimString == next.word
   }
   /** creates a new ActiveNotation with this notation's markers */
   def open(scanner: Scanner, firstToken: Int): ActiveNotation = {
      val an = new ActiveNotation(scanner, markers, firstToken)
      an
   }
}

object Notation {
   def apply(ms: Any*): Notation = {
      val markers : List[Marker] = ms.toList map {
         case i: Int => Arg(i)
         case s: String => Delim(s)
         case _ => throw ParseError("not a valid marker")
      }
      new Notation(markers)
   }
}

/** An ActiveNotation is a notation whose firstDelimToken has been scanned
 *  An ActiveNotation maintains state about the found and still-expected markers
 */
class ActiveNotation(scanner: Scanner, markers: List[Marker], val firstToken: Int) {
   // invariant: found.reverse (qua List[Marker]) ::: left == markers
   /** the markers that have been found in the token list */
   private var found : List[Found] = Nil
   /** all found tokens */
   def getFound = found.reverse
   /** the markers that are still expected */
   private var left : List[Marker] = markers
   /** the number of tokens that are part of the current argument(s),
    * i.e., the tokens scanned since the previously found delimiter */
   var numCurrentTokens : Int = 0

   //auxiliary variable to remember the argument number between calls successive applicable-apply and closable-close invocations
   private var remember : Int = -1

   override def toString = found.reverse.mkString("", " ", "")

  /**
   * @param currentToken the currently scanned token
   * @return true if the notation can be applied at this point
   * i.e., currentToken is (one of the) delimiter(s) expected next and currentTokens matches the currently expected arguments
   */
  def applicable(currentToken: Token): Boolean = {
      left match {
         case Arg(n) :: Delim(t) :: _ =>
            remember = n
            currentToken == t
         case Delim(t) :: _ =>
            remember = -1
            currentToken == t
         case _ => false
      } 
   }
   /**
    * precondition: this.applicable(scanner.currentToken)
    * terminate the current argument(s) and match the current token to the next expected delimiter
    * @return true iff the notation is fully applied, i.e., no further arguments or delimiters can be matched  
    */
   def apply: Boolean = {
      if (remember != -1) {
         found ::= FoundArg(scanner.pick(numCurrentTokens), remember)
         left = left.tail
      }
      found ::= left.head.asInstanceOf[Delim]
      left = left.tail
      numCurrentTokens = 0
      if (markers.isEmpty) {
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
      left match {
         case Arg(n) :: Nil =>
            remember = n
            true
         case _ => false
      }
   }
   /**
    * precondition: this.closable
    * terminate the current argument(s)
    */
   def close {
      found ::= FoundArg(scanner.pick(numCurrentTokens), remember)
      left = left.tail
   }
}
