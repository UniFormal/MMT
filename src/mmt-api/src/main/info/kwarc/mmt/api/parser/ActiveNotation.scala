package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import notations._
import utils.MyList._

import ActiveNotation._

case class ScannerBacktrackInfo(currentIndex: Int, numCurrentTokens: Int)

/** An ActiveNotation is a notation whose firstDelimToken has been scanned
 *  An ActiveNotation maintains state about the found and still-expected markers
 *  
 *  @param scanner back-pointer to the owning scanner
 *  @param rules the parsing rules 
 *    must be non-empty;
 *    if not a singleton, all [[TextNotation]]s must TextNotation.agree 
 *  @param firstToken the index of the Token that caused this notation to be opened (i.e., the first delimiter token)
 */
class ActiveNotation(scanner: Scanner, val rules: List[ParsingRule], val backtrackInfo: ScannerBacktrackInfo) {
   // invariant: found.reverse (qua List[Marker]) ::: left == markers of each rule

   override def toString = toShortString + " " + found.reverse.mkString("", " ", "")
   def toShortString = rules.map(_.name.last).mkString("/")

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
   /** pick all available Token's as SimpArg(n) or LabelArg(n, typed, defined) */
   private def PickAll(n: Int, isOML:Option[LabelInfo]) : FoundArg = isOML match {
      case None => FoundSimpArg(scanner.pick(numCurrentTokens), n)
      case Some(io) => FoundOML(scanner.pick(numCurrentTokens),n,io)
   }
   /** pick exactly ns.length available Tokens as ns.map(Arg(_)) or ns.map(LabelArg(_,...)) */
   private def PickSingles(ns: List[(Int,Option[LabelInfo])]) = {
      val fs = ns reverseMap {case (n,isOML) =>
         isOML match {
            case None => FoundSimpArg(scanner.pick(1), n)
            case Some(io) => FoundOML(scanner.pick(1), n, io)
         }
      }
      found = fs ::: found
   }
   
   private def PickAllSeq(n: Int, infoOpt:Option[LabelInfo]) = {
      val ts = scanner.pick(numCurrentTokens)
      val a = infoOpt match {
         case None => FoundSimpArg(ts, n)
         case Some(info) => FoundOML(ts, n, info)
      }
      found.headOption match {
         case Some(FoundSimpSeqArg(m, args)) if m == n && infoOpt.isEmpty =>
            found = FoundSimpSeqArg(n, args ::: List(a.asInstanceOf[FoundSimpArg])) :: found.tail
         case Some(FoundSeqOML(m, args, info)) if m == n && infoOpt.isDefined =>
            found = FoundSeqOML(n, args ::: List(a.asInstanceOf[FoundOML]), info) :: found.tail
         case _ =>
            val f = infoOpt match {
               case None => FoundSimpSeqArg(n, List(a.asInstanceOf[FoundSimpArg]))
               case Some(info) => FoundSeqOML(n, List(a.asInstanceOf[FoundOML]), info)
            }
            found ::= f
      }
   }

   private def SeqDone(n: Int, isOML:Option[LabelInfo]) {
      found.headOption match {
         case Some(FoundSimpSeqArg(m, _)) if m == n =>
         case Some(FoundSeqOML(m,_,_)) if m == n =>
         case _ =>
            val f = isOML match {
               case None => FoundSimpSeqArg(n, Nil)
               case Some(info) => FoundSeqOML(n,Nil,info)
            }
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
   
   private def inSeqArg(n: Int) = found match {
      case FoundSimpSeqArg(m, _) :: _ if m == n => true
      case FoundSeqOML(m,_,_) :: _ if m == n => true
      case _ => false
   }
   
   /** splits a List[Marker] into
    *  a List[Int] (possibly Nil) that corresponds to a List[Arg]
    * and the remaining List[Marker]
    */
   private def splitOffArgs(ms: List[Marker], ns: List[(Int,Option[LabelInfo])] = Nil) : (List[(Int,Option[LabelInfo])], List[Marker]) = ms match {
      case (la@LabelArg(n,_,_,_)) :: rest => splitOffArgs(rest, (n,Some(la.info)) :: ns)
      case SimpArg(n, _) :: rest => splitOffArgs(rest, (n,None) :: ns)
      case Delim("%w") :: rest => splitOffArgs(rest, ns)
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
      splitOffArgs(left) match {
         case (ns, Delimiter(s) :: _) if matches(s) =>
            ns match {
            case Nil =>
               onApplyI {currentIndex =>
                  deleteDelim(currentIndex)
               }
            case List((n,isOML)) =>
               onApplyI {currentIndex =>
                  found ::= PickAll(n,isOML)
                  delete(1)
                  deleteDelim(currentIndex)
               }
            case _ if ns.length == numCurrentTokens =>
               onApplyI {currentIndex =>
                  PickSingles(ns)
                  delete(numCurrentTokens)
                  deleteDelim(currentIndex)
               }
         }
         case (Nil, SimpSeqArg(n, Delimiter(s),_) :: _) if matches(s) =>
              onApply {
                 PickAllSeq(n,None)
                 addPrepickedDelims(Delim(s), currentToken)
              }
         case (Nil, (lsa @ LabelSeqArg(n,Delimiter(s),_,_,_,_)) :: _) if matches(s) =>
            onApply {
               PickAllSeq(n,Some(lsa.info))
               addPrepickedDelims(Delim(s),currentToken)
            }
         case (Nil, SimpSeqArg(n, Delimiter(t),_) :: Delimiter(s) :: _) if ! matches(t) && matches(s) =>
              if (numCurrentTokens > 0) {
                 //picks the last element of the sequence (possibly the only one)
                 onApplyI {currentIndex =>
                    PickAllSeq(n,None)
                    delete(1)
                    deleteDelim(currentIndex)
                 }
              } else if (numCurrentTokens == 0) {
                 //picks nothing and finds an empty sequence
                 onApplyI {currentIndex =>
                    SeqDone(n,None)
                    delete(1)
                    deleteDelim(currentIndex)
                 }
              } else
                 NotApplicable //abort?
         case (Nil, (lsa @ LabelSeqArg(n,Delimiter(t),_,_,_,_)) :: Delimiter(s) :: _) if ! matches(t) && matches(s) =>
            if (numCurrentTokens > 0) {
               //picks the last element of the sequence (possibly the only one)
               onApplyI {currentIndex =>
                  PickAllSeq(n,Some(lsa.info))
                  delete(1)
                  deleteDelim(currentIndex)
               }
            } else if (numCurrentTokens == 0) {
               //picks nothing and finds an empty sequence
               onApplyI {currentIndex =>
                  SeqDone(n,Some(lsa.info))
                  delete(1)
                  deleteDelim(currentIndex)
               }
            } else
               NotApplicable //abort?
         // start a variable
         case (Nil, (vm @ Var(n, _, sep, _)) :: rest) =>
            numCurrentTokens match {
               case 0 => onApplyI {currentIndex =>
                   //start a variable and take the current Token as the first variable name
                   val fv = new FoundVar(vm)
                   fv.newVar(currentIndex, currentToken)
                   fv.state = FoundVar.AfterName
                   found ::= fv
               } /*
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
                  }*/
               case _ => Abort // should be impossible
            }
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
      splitOffArgs(left) match {
         case (Nil, SimpSeqArg(n, Delimiter(s), _) :: Nil) =>
              if (inSeqArg(n) && numCurrentTokens > 0) {
                 onApply {
                    PickAllSeq(n,None)
                    delete(1)
                 }
              } else if (! inSeqArg(n) && numCurrentTokens == 0) {
                 onApply {
                    SeqDone(n,None)
                    delete(1)
                 }
              } else
                 NotApplicable
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
         }
         case (List((n,Some(info))), Nil) if numCurrentTokens > 0 =>
            // one argument taking all available Token's
            onApply {
               found ::= PickAll(n,Some(info))
               delete(1)
            }
         // as many arguments as there are Token's
         // should we abort immediately if the numbers do not match up?
         case (List((n,None)), Nil) if numCurrentTokens > 0 =>
            // one argument taking all available Token's
            onApply {
               found ::= PickAll(n,None)
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
   def close {
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