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
  val rules: List[ParsingRule]
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
class ActiveNotation(scanner: Scanner, val rules: List[ParsingRule], val backtrackInfo: ScannerBacktrackInfo) extends ActiveParsingRule {
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
   private def delete(n: Int): Unit = {
      left = left.drop(n)
   }
   /**
    * move a delimiter from left to found and addPrepickedDelims if necessary
    */
   private def deleteDelim(index: Int): Unit = {
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
   private def addPrepickedDelims(delim: Delimiter, token: Token): Unit = {
      val prepicked = delim.text.substring(token.word.length).toList.map(c => Delim(c.toString))
      left = prepicked ::: left
   }
   /** pick all available Token's as a FoundSimpArg */
   private def PickAll(m: ChildMarker) = {
      val ts = scanner.pick(numCurrentTokens)
      FoundSimp(ts, m)
   }
   /** pick exactly ms.length available Tokens, each as as FoundSimp */
   private def PickSingles(ms: List[ChildMarker]): Unit = {
      val fs = ms reverseMap {m =>
         FoundSimp(scanner.pick(1), m)
      }
      found = fs ::: found
   }
   /** like pickAll, but appends to a previously started sequence or starts a new sequence */
   private def PickAllSeq(m: ChildMarker): Unit = {
      val f = PickAll(m)
      (found.headOption, m) match {
         case (Some(FoundSeq(s, args)), m) if s.number == m.number =>
            found = FoundSeq(s, args ::: List(f)) :: found.tail
         case (_, m) =>
            found ::= FoundSeq(m, List(f))
      }
   }
   /** inserts an empty sequence if a sequence is ended before any elements were found */
   private def SeqDone(m: ChildMarker): Unit = {
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
      /* DELETE after testing 2020-07-07
      // now the actual applicability check in 2 cases
      // first: if we are in a bound variable, step through the corresponding state machine
      val result: Applicability = found match {
         case (fv : FoundVar) :: _ =>
            val vm = fv.marker
            val nextDelim = left match {
               case Var(_,_,_,_) :: Delimiter(s) :: _=> Some(s)
               case Var(_,_,_,_) :: Nil => None
               case _ => throw ImplementationError("invalid notation") //variable must be followed by nothing or Delim
            }
            //we are in a bound variable (sequence)
            fv.state match {
               case FoundVar.BeforeName => onApplyI {currentIndex =>
                  fv.newVar(currentIndex, currentToken)
                  fv.state = FoundVar.AfterName
               }
               case FoundVar.AfterName =>
                  if (nextDelim.isDefined && matches(nextDelim.get)) {
                     onApplyI {currentIndex =>
                        fv.state = FoundVar.Done
                        delete(1)
                        deleteDelim(currentIndex)
                     }
                  } else if (vm.sep.isDefined && vm.sep.get.text == currentToken.word) {
                     onApply {
                        fv.state = FoundVar.BeforeName
                     }
                  } else if (vm.typed) {
                     fv.state = FoundVar.InType
                     NotApplicable //start shifting the type
                  } else {
                     Abort
                  }
               case FoundVar.InType =>
                  if (nextDelim.isDefined && matches(nextDelim.get)) {
                     onApplyI {currentIndex =>
                        val fa = PickAll(vm.number,None)
                        fv.newType(fa)
                        fv.state = FoundVar.Done
                        delete(1)
                        deleteDelim(currentIndex)
                     }
                  } else if (vm.sep.isDefined && vm.sep.get.s == currentToken.word) {
                     onApply {
                        val fa = PickAll(vm.number,None)
                        fv.newType(fa)
                        fv.state = FoundVar.BeforeName
                     }
                  } else NotApplicable //shift and stay in this state

               case FoundVar.Done => null //nothing to do anymore, skip to remaining cases
            }
         case _ => null //skip to all other cases
      }
      if (result != null) return result
      // second: otherwise, try to match the current Token against an upcoming delimiter
       */
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
                    found ::= PickAll(n)
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
                    //TODO use matched tokens as delimiters, merge in between them
                    onApplyI {currentIndex =>
                       PickSingles(ns.init)
                       PickAll(ns.last)
                       delete(numCurrentTokens)
                       deleteDelim(currentIndex)
                    }
                 } else {
                    NotApplicable
                 }
           }
        // the notation expects a sequence whose separator is the current token: all previously shifted tokens become the first element, and we start the sequence
        case (Nil, (cm: ChildMarker) :: _) if cm.isSequenceVia.exists(s => matches(s.text)) =>
           onApply {
              PickAllSeq(cm)
              addPrepickedDelims(cm.isSequenceVia.get,currentToken)
           }
        // the notation expects a sequence followed by the current token: end the (possibly empty) sequence
        case (Nil,(cm:ChildMarker) :: Delimiter(s) :: _) if cm.isSequenceVia.exists(t => !matches(t.text)) && matches(s) =>
           if (numCurrentTokens > 0) {
              //picks the last element of the sequence (possibly the only one)
              onApplyI {currentIndex =>
                 PickAllSeq(cm)
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
/*  DELETE after testing 2020-07-07
       // the notation expects a variable sequence followed by the current token: find the empty sequence
         case (Nil, (vm @ Var(n, _, Some(Delimiter(t)), _)) :: Delimiter(s) :: _) if ! matches(t) && matches(s) =>
            onApplyI {currentIndex =>
              val fv = new FoundVar(vm)
              fv.state = FoundVar.Done
              delete(1)
              deleteDelim(currentIndex)
            }
         // start a variable, interpreting the current token as the variable name
         case (Nil, (vm @ Var(n, _, sep, _)) :: rest) =>
            numCurrentTokens match {
               case 0 => onApplyI {currentIndex =>
                   //start a variable and take the current Token as the first variable name
                   val fv = new FoundVar(vm)
                   fv.newVar(currentIndex, currentToken)
                   fv.state = FoundVar.AfterName
                   found ::= fv
               }
*/
               /*
               // removed because
               //  - there is no way to make sure the shifted Token is a variable name
               //  - it's confusing that it does not also check for the type key or the separator
               case 1 =>
                  rest match {
                     case Delimiter(s) :: _ if matches(s) => onApply {
                          //parse a single untyped variable
                          //the previous Token is the name
                          //the current Token must be the next Delim
                          //this case is only possible if vm is the first marker and the next Delim is what opened the notation
                          val vr = scanner.pick(1)
                          val name = vr.toList(0) match {
                             case t: Token => t
                             case _ => ???
                          }
                          val fv = new FoundVar(vm)
                          fv.newVar(vr.start, name)
                          fv.state = FoundVar.Done
                          found ::= fv
                          delete(1)
                          deleteDelim(currentIndex)
                     }
                     case _ => Abort
                  }
               case _ => Abort // should be impossible
            }*/
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
         case (Nil, (m: ChildMarker) :: Nil) if m.isSequence =>
              if (numCurrentTokens > 0) {
                 // the available tokens are the last (possibly only) element
                 onApply {
                    PickAllSeq(m)
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
           /*
         case (Nil, (vm: Var) :: Nil) => found match {
            case (fv: FoundVar) :: _ =>
                  //the last thing found was a variable, probably fv.marker == vm
                  fv.state match {
                     case FoundVar.BeforeName =>
                        //separator found before, need name now
                        NotApplicable
                     case FoundVar.AfterName => onApply {
                        //close leaving variable untyped
                        fv.state = FoundVar.Done
                        delete(1)
                     }
                     case FoundVar.InType => onApply {
                        //pick type and close
                        val fa = PickAll(vm.number,None)
                        fv.newType(fa)
                        fv.state = FoundVar.Done
                        delete(1)
                     }
                     case FoundVar.Done =>
                        //fv is done already, then vm must refer to another variable (i.e., fv.marker != vm)
                        // can't close because we still need to parse vm
                        NotApplicable
                  }
            case _ =>
                // can't close because we've never started vm, still need to parse vm
                NotApplicable
         }*/
         case (List(m), Nil) if numCurrentTokens > 0 =>
            // one argument taking all available Token's
            onApply {
               found ::= PickAll(m)
               delete(1)
            }
            // as many arguments as there are Token's
            // should we abort immediately if the numbers do not match up?
         case (ns, Nil) if numCurrentTokens == ns.length =>
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
   def close: Unit = {
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
