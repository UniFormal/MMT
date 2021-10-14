package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import notations._
import utils._
import ActiveNotation._

/** remembers the relevant Scanner state at the time when an [[ActiveNotation]] was created */
abstract class ScannerBacktrackInfo
case class ScannerBacktrack(currentIndex: Int, numCurrentTokens: Int) extends ScannerBacktrackInfo
/** cut, i.e, fail if this notation is not applicable */
case object ScannerNoBacktrack extends ScannerBacktrackInfo

/** meant to replace ActiveNotation as a more general interface in Scanner and related classes
 *  That would computational notations.
 */
abstract class ActiveParsingRule {
  val rules: List[NotationRule]
  val backtrackInfo: ScannerBacktrackInfo  
  
  def applicable(currentToken: Token, futureTokens: String): Applicability
  
  def apply(currentIndex: Int): Boolean
  
  def closable: Applicability
  
  def close: Unit
}

/** An ActiveNotation is a notation whose firstDelimToken has been scanned
 *  An ActiveNotation maintains state about the found and still-expected markers
 *
 *  @param scanner back-pointer to the owning scanner
 *  @param rules the parsing rules
 *    must be non-empty;
 *    if not a singleton, all [[TextNotation]]s must TextNotation.agree
 *  @param backtrackInfo information for restoring the state before activating this notation
 */
class ActiveNotation(scanner: UnmatchedList,val rules: List[NotationRule],val backtrackInfo: ScannerBacktrackInfo) extends ActiveParsingRule {
   // invariant: found.reverse (qua List[Marker]) ::: left == markers of each rule

   override def toString = toShortString + " " + found.reverse.mkString("", " ", "")
   def toShortString = rules.map(_.name.last).mkString("/")

   /** the markers that have been found in the token list (in reverse concrete syntax order) */
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
   private var left : List[Marker] = rules.head.notation.parsingMarkers.map {
      case d: Delimiter => d.expand(rules.head.name, rules.head.alias) //only possible if rules.length == 1
      case m => m
   }
   /** the number of tokens that are part of the current argument(s),
    * i.e., the tokens scanned since the previously found delimiter */
   private[parser] var numCurrentTokens : Int = 0

   /** remove n Markers from left */
   private def delete(n: Int) {
      left = left.drop(n)
   }
   /**
    * move a delimiter from left to found and addPrepickedDelims if necessary
    */
   private def deleteDelim(index: Int) {
      val delim = left.head.asInstanceOf[Delimiter]
      found ::= FoundDelim(index, delim)
      left = left.tail
      val token = scanner.tl(index).asInstanceOf[Token]
      addPrepickedDelims(delim, token)
   }
   /**
    * pre: delim.startsWith(token)
    *
    * for each additional character c in delim, add Delim(c) to the front
    * this has the effect of skipping the subsequent Tokens that have been used already after matching token against delim
    */
   private def addPrepickedDelims(delim: Delimiter, token: Token) {
      val prepicked = delim.text.substring(token.word.length).toList.map(c => Delim(c.toString))
      left = prepicked ::: left
   }
   /** pick exactly ms.length available Tokens, each as as FoundSimp */
   private def PickSingles(ms: List[ChildMarker]) {
    val fs = ms map {m =>
      val f = FoundSimp(scanner.pick(firstPickedToken+numPickedTokens, 1), m)
      numPickedTokens += 1
      f
    }
    found = fs.reverse ::: found
   }
   /** pick all available Token's and returns the FoundSimp without changing local state*/
   private def PickRemainingAux(m: ChildMarker) = {
      val first = firstPickedToken+numPickedTokens
      val ts = scanner.pick(first, numCurrentTokens-first)
      numPickedTokens += numCurrentTokens
      FoundSimp(ts, m)
   }
   /** pick all available Token's */
   private def PickRemaining(m: ChildMarker) {
     found ::= PickRemainingAux(m)
   }
   /** like pickAll, but appends to a previously started sequence or starts a new sequence */
   private def PickRemainingSeq(m: ChildMarker) {
      val f = PickRemainingAux(m)
      (found.headOption, m) match {
         case (Some(FoundSeq(s, args)), m) if s.number == m.number =>
            found = FoundSeq(s, args ::: List(f)) :: found.tail
         case (_, m) =>
            found ::= FoundSeq(m, List(f))
      }
   }
   /** inserts an empty sequence if a sequence is ended before any elements were found */
   private def SeqDone(m: ChildMarker) {
      found.headOption match {
         case Some(FoundSeq(s, _)) if s.number == m.number =>
         case _ =>
            val f = FoundSeq(m, Nil)
            found ::= f
      }
   }

   /** remember stores operations for picking arguments
    *  these are remembered between calls of successive invocations of
    *  applicable-apply and closable-close
    *  only well-defined after applicable or closable returned true
    */
   private var remember : Int => Unit = null
   /** during apply/close, keeps track of how many tokens have been picked */
   private var numPickedTokens = 0
   /** during apply/close, keeps track of where picking started */
   private var firstPickedToken = 0
   private[parser] def getPicked = numPickedTokens

   /** stores an operation in remember for later execution */
   private def onApply(act: => Unit): Applicability = {
      remember = _ => act
      Applicable
   }
   /** stores an operation in remember for later execution */
   private def onApplyI(act: Int => Unit): Applicability = {
      remember = act
      Applicable
   }

   private def inSeq(m: ChildMarker) = found match {
      case (fs: FoundSeq) :: _ if fs.number == m.number => true
      case _ => false
   }

   /** splits a List[Marker] into a list of a simple (plain, label, or variable) markers and the remaining markers
    */
   private def splitOffSimples(ms: List[Marker], ns: List[ChildMarker] = Nil) : (List[ChildMarker], List[Marker]) = ms match {
      case (la:LabelArg) :: rest => splitOffSimples(rest, la :: ns)
      case (vm @ Var(n, typed, None, _)) :: rest => splitOffSimples(rest, vm :: ns)
      case (sa:SimpArg) :: rest => splitOffSimples(rest, sa :: ns)
      case Delim("%w") :: rest => splitOffSimples(rest, ns)
      case rest => (ns.reverse, rest)
   }

  /**
   * @param currentToken the currently scanned token
   * @param futureTokens a string containing those succeeding Tokens that could be merged into the current Token
   *   this is used to parse multi-symbol delimiters, where only the first symbol is in currentToken
   * @return true if the notation can be applied at this point
   * i.e., currentToken is (one of the) delimiter(s) expected next and numCurrentTokens matches the currently expected arguments
   */
  def applicable(currentToken: Token, futureTokens: String): Applicability = {
     //shortcut: true if delimiter s matches at the currentIndex
     def matches(s: String) = ActiveNotation.matches(s, currentToken.word, futureTokens)
     splitOffSimples(left) match {
        // the notation expects some arguments followed by the current token: all previously shifted tokens become arguments
        case (ns,Delimiter(s) :: _) if matches(s) =>
           ns match {
              // no arguments expected: just consume the delimiter
              case Nil =>
                 onApplyI {currentIndex =>
                    deleteDelim(currentIndex)
                 }
              // create a single arg with all available tokens
              case List(n) if numCurrentTokens > 0 =>
                 onApplyI {currentIndex =>
                    PickRemaining(n)
                    delete(1)
                    deleteDelim(currentIndex)
                 }
              case _ =>
                 if (ns.length == numCurrentTokens) {
                    // create a single arg for each available token
                    onApplyI {currentIndex =>
                       PickSingles(ns)
                       delete(numCurrentTokens)
                       deleteDelim(currentIndex)
                    }
                 } else if (ns.length < numCurrentTokens) {
                    // if more tokens available than needed, merge all remaining tokens into the last argument
                    //TODO use matched tokens as delimiters, merge in between them?
                    onApplyI {currentIndex =>
                       PickSingles(ns.init)
                       PickRemaining(ns.last)
                       delete(ns.length)
                       deleteDelim(currentIndex)
                    }
                 } else {
                    NotApplicable
                 }
           }
        // the notation expects a sequence whose separator is the current token: all previously shifted tokens become the first element, and we start the sequence
        case (Nil, (cm: ChildMarker) :: _) if cm.isSequenceVia.exists(s => matches(s.text)) =>
           onApply {
              PickRemainingSeq(cm)
              addPrepickedDelims(cm.isSequenceVia.get,currentToken)
           }
        // the notation expects a sequence followed by the current token: end the (possibly empty) sequence
        case (Nil,(cm:ChildMarker) :: Delimiter(s) :: _) if cm.isSequenceVia.exists(t => !matches(t.text)) && matches(s) =>
           if (numCurrentTokens > 0) {
              //picks the last element of the sequence (possibly the only one)
              onApplyI {currentIndex =>
                 PickRemainingSeq(cm)
                 delete(1)
                 deleteDelim(currentIndex)
              }
           } else if (numCurrentTokens == 0) {
              //picks nothing and finds an empty sequence
              onApplyI {currentIndex =>
                 SeqDone(cm)
                 delete(1)
                 deleteDelim(currentIndex)
              }
           } else
              NotApplicable //abort?
         case _ => NotApplicable
      }
   }
   /**
     * precondition: this.applicable(scanner.currentToken)
     * terminate the current argument(s) and match the current token to the next expected delimiter
     *
     * @param currentIndex the index of currentToken
     * @return true iff the notation is fully applied, i.e., no further arguments or delimiters can be matched
     */
   //currentIndex must be passed here because it is not known yet when applicable is called (because other notations may be closed in between)
   def apply(currentIndex: Int): Boolean = {
      // setting this to 0 means we always take all token available to the left,
      // for the left-open arguments, one might extend this to only take some of the tokens, e.g., as many as we need without merging the remaining ones
      firstPickedToken = 0
      numPickedTokens = 0
      remember(currentIndex)
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
      splitOffSimples(left) match {
         // the notation expects a sequence
         case (Nil,Nil) => onApply {}
         case (Nil, (m: ChildMarker) :: Nil) if m.isSequence =>
           if (numCurrentTokens > 0) {
              // the available tokens are the last (possibly only) element
              onApply {
                 PickRemainingSeq(m)
                 delete(1)
              }
           } else if (!inSeq(m)) {
               // no tokens available and the sequence has not been started yet: empty sequence
               onApply {
                 SeqDone(m)
                 delete(1)
               }
           } else
              NotApplicable
         // as many arguments as there are Token's
         // should we abort immediately if the numbers do not match up?
         case (ns, Nil) =>
            if (numCurrentTokens == ns.length) {
               onApply {
                  PickSingles(ns)
                  delete(ns.length)
               }
            } else if (numCurrentTokens > ns.length) {
              // if last marker is for single token only: do not merge
              val mergeRemaining = ns.last match {
                 case a:SimpArg => !a.singleTokenOnly
                 case _ => true
              }
              if (mergeRemaining) onApply {
                PickSingles(ns.init)
                PickRemaining(ns.last)
                delete(ns.length)
              } else onApply {
                PickSingles(ns)
                delete(ns.length)
              }
            } else {
              NotApplicable
            }
         case _ => NotApplicable
      }
   }
   /**
    * precondition: this.closable
    * terminate the current argument(s)
    */
   def close {
      // setting this to 0 means we start picking right-open arguments from the left
      // if there are more tokens than we pick, they are left for other notations
      firstPickedToken = 0
      numPickedTokens = 0
      remember(-13) // dummy value because closable never uses the currentIndex argument
   }
}

/** a helper object */
object ActiveNotation {
   /** a type to express the result of testing whether a notation is applicable
    *
    * essentially, a Notation is applicable if the next Token matches the next Delimiter it expects
    */
   sealed abstract class Applicability
   /** a value of type Applicability expressing that the notation is applicable */
   case object Applicable extends Applicability
   /** a value of type Applicability expressing that the notation is not applicable now
    * (but may be applicable later if the current Token is shifted)
    */
   case object NotApplicable extends Applicability
   /** a value of type Applicability expressing that the notation cannot be applied anymore
    * (and parsing should backtrack)
    */
   case object Abort extends Applicability

   /**
    * true if delim equals currentToken, possibly after extending currentToken with some characters from futureTokens
    */
   def matches(delim: String, currentToken: String, futureTokens: String) = {
      delim.length >= currentToken.length && (currentToken+futureTokens).startsWith(delim)
   }
}
