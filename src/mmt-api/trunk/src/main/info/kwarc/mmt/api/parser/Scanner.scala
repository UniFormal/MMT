package info.kwarc.mmt.api.parser.test

case class Ambiguous() extends java.lang.Throwable

class Token

class TokenList(tokens: List[Token]) {
   def apply(n: Int) = tokens(n)
   val length = tokens.length
}

case class TokenSlice(tokens: TokenList, start: Int, next: Int) { 
}

/** scans a TokenList against a set of notations */
class Scanner(tl: TokenList, notations: List[Notation]) {
   private val numTokens = tl.length 
   private var currentIndex: Int = 0
   private var currentToken : Token = null
   def pick(length: Int) = TokenSlice(tl, currentIndex - length, currentIndex)
   // the currently open notations, inner-most first
   private var active: List[ActiveNotation] = Nil
   /*
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
   // applies the first active notation (precondition: must be applicable)
   private def applyFirst {
      val toClose = active.head.apply
      if (toClose)
         active = active.tail
   }
   // closes the first active notation (precondition: must be closable)
   private def closeFirst {
       active.head.close
       //TODO: remember how the notation matched and recurse
       active = active.tail
   }
   private def advanceFirst {
      active.head.advance
   }
   private def next {
      currentToken = tl(currentIndex)
      //determine if/how an active notation is applicable
      val (closable, anyActiveApplicable) = checkActive(active, 0)
      if (anyActiveApplicable) {
         //close the first active notations that are closable, then apply the next one 
         Range(0,closable) foreach {_ => closeFirst}
         applyFirst
      } else {
            //determine if a new notation can be opened 
            val applicable = notations filter {a => a.applicable(currentToken)}
            applicable match {
               case hd :: Nil =>
                  //open the notation and apply it
                  val an = hd.open(this, currentIndex)
                  advanceFirst
                  active ::= an
                  applyFirst
               case Nil =>
                  //move one token forward
                  advanceFirst
                  currentIndex += 1
               case _ => throw Ambiguous() //some kind of ambiguity-handling here (maybe look for next delimiter)
            }
      }
      //check if there is next token left
      if (currentIndex < numTokens)
        //continue with the next token
        next
      else {
         //check if all active notations can be closed
         //TODO: cover the weird case where a new notation was opened in the current invocation of the method and not immediately closed
         if (closable == active.length) {
            //close them and we're done
            Range(0,closable) foreach {_ => closeFirst}
         } else {
            //active notations left, but no further tokens left -> backtracking
            currentIndex = active.head.firstToken + 1
            active = active.tail
            next
         }
      }
   }
   /** starts scanning */
   def scan {
      next
   }
}


abstract class Marker
case class Delim(t: Token) extends Marker
case class Arg(n: Int) extends Marker
case class SqArg(n: Int, sep: Delim) extends Marker

abstract class Found
case class FoundDelim(delim: TokenSlice) extends Found
case class FoundArg(arg: TokenSlice, n: Int) extends Found
case class FoundSeqArg(seqarg: TokenSlice, delims: List[FoundDelim], args: List[FoundArg]) extends Found

class Notation(markers: List[Marker]) {
   // the first delimiter of this notation
   private val firstDelimToken : Token = {
      markers find {_.isInstanceOf[Delim]} match {
         case Some(Delim(t)) => t
         case _ => null 
      }
   }
   /** @return firstDelimToken == next */
   def applicable(next: Token): Boolean = {
      firstDelimToken == next
   }
   /** creates a new ActiveNotation with this notation's markers */
   def open(scanner: Scanner, firstToken: Int): ActiveNotation = {
      val an = new ActiveNotation(scanner, markers, firstToken)
      an
   }
}

/** An ActiveNotation is a notation whose firstDelimToken has been scanned
 *  An ActiveNotation maintains state about the found and still-expected markers
 */
class ActiveNotation(scanner: Scanner, markers: List[Marker], val firstToken: Int) {
   // invariant: found.reverse (qua List[Marker]) ::: left == markers
   /** the markers that have been found in the token list */
   private var found : List[Found] = Nil
   /** the markers that are still expected */
   private var left : List[Marker] = markers
   /** the number of tokens that are part of the current argument(s),
    * i.e., the tokens scanned since the previously found delimiter */
   private var currentTokens : Int = 0

   //auxiliary variable to remember the argument number between calls successive applicable-apply and closable-close invocations
   private var remember : Int = 0

  /**
   * add one token to the current argument(s)
   */
   def advance {currentTokens += 1}

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
         case _ => false
      } 
   }
   /**
    * precondition: this.applicable(scanner.currentToken)
    * terminate the current argument(s) and match the current token to the next expected delimiter
    * @return true iff the notation is fully applied, i.e., no further arguments or delimiters can be matched  
    */
   def apply: Boolean = {
      found ::= FoundArg(scanner.pick(currentTokens), remember)
      left = left.tail
      currentTokens = 0
      found ::= FoundDelim(scanner.pick(1))
      left = left.tail
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
      found ::= FoundArg(scanner.pick(currentTokens), remember)
      left = left.tail
   }
}
