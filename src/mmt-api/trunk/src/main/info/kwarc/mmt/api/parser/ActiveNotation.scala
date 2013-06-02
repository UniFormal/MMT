package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import utils.MyList._

import ActiveNotation._

/** An ActiveNotation is a notation whose firstDelimToken has been scanned
 *  An ActiveNotation maintains state about the found and still-expected markers
 */
class ActiveNotation(scanner: Scanner, val notation: TextNotation, val firstToken: Int) {
   // invariant: found.reverse (qua List[Marker]) ::: left == markers

   override def toString = notation.name.toPath + " " + found.reverse.mkString("", " ", "")
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
   def addPrepickedDelims(delim: Delimiter, token: Token) {
      val prepicked = delim.text.substring(token.word.length).toList.map(c => Delim(c.toString))
      left = prepicked ::: left
   }
   /** pick all available Token's as Arg(n) */
   private def PickAll(n: Int) : FoundArg = {
      FoundArg(scanner.pick(numCurrentTokens), n)
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
   
  /**
   * @param currentToken the currently scanned token
   * @return true if the notation can be applied at this point
   * i.e., currentToken is (one of the) delimiter(s) expected next and currentTokens matches the currently expected arguments
   */
  def applicable(currentToken: Token, currentIndex: Int, futureTokens: String): Applicability = {
      //shortcut: true if delimiter s matches at the currentIndex
      def matches(s: String) = ActiveNotation.matches(s, currentToken.word, futureTokens)
      // now the actual applicability check in 2 cases
      // first: if we are in a bound variable, step through the corresponding state machine
      val result: Applicability = found match {
         case (fv : FoundVar) :: _ =>
            val vm = fv.marker
            val nextDelim = left match {
               case Var(_,_,_) :: Delimiter(s) :: _=> Some(s)
               case Var(_,_,_) :: Nil => None
               case _ => throw ImplementationError("invalid notation") //variable must be followed by nothing or Delim
            }
            //we are in a bound variable (sequence)
            fv.state match {
               case FoundVar.BeforeName => onApply {
                  fv.newVar(currentIndex, currentToken)
                  fv.state = FoundVar.AfterName
               }
               case FoundVar.AfterName =>
                  if (nextDelim.isDefined && matches(nextDelim.get)) {
                     onApply {
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
                     onApply {
                        val fa = PickAll(vm.number)
                        fv.newType(fa)
                        fv.state = FoundVar.Done
                        delete(1)
                        deleteDelim(currentIndex)
                     }
                  } else if (vm.sep.isDefined && vm.sep.get.s == currentToken.word) {
                     onApply {
                        val fa = PickAll(vm.number)
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
      Arg.split(left) match {
         case (ns, Delimiter(s) :: _) if matches(s) =>
            ns match {
            case Nil =>
               onApply {
                  deleteDelim(currentIndex)
               }
            case List(n) =>
               onApply {
                  found ::= PickAll(n)
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
         case (Nil, SeqArg(n, Delimiter(s)) :: _) if matches(s) =>
              onApply {
                 PickAllSeq(n)
                 addPrepickedDelims(Delim(s), currentToken)
              }
         case (Nil, SeqArg(n, Delimiter(t)) :: Delimiter(s) :: _) if ! matches(t) && matches(s) =>
              if (numCurrentTokens > 0) {
                 //picks the last element of the sequence (possibly the only one)
                 onApply {
                    PickAllSeq(n)
                    delete(1)
                    deleteDelim(currentIndex)
                 }
              } else if (numCurrentTokens == 0) {
                 //picks nothing and finds an empty sequence
                 onApply {
                    SeqDone(n)
                    delete(1)
                    deleteDelim(currentIndex)
                 }
              } else
                 NotApplicable //abort?
         // start a variable
         case (Nil, (vm @ Var(n, _, sep)) :: rest) =>
            numCurrentTokens match {
               case 0 => onApply {
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
         case (Nil, SeqArg(n, Delimiter(s)) :: Nil) =>
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
                        val fa = PickAll(vm.number)
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
         case (List(n), Nil) =>
            // one argument taking all available Token's
            onApply {
               found ::= PickAll(n)
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