package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import utils.MyList._

import ActiveNotation._

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
      case FoundVar(m, _, _, None) :: _ if m == n => true
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
                    val vr = scanner.pick(1)
                    val name = vr.toList(0).asInstanceOf[Token]
                    found ::= FoundVar(n, vr.start, name, None)
                 }
              } else {
                 NotApplicable //abort
              }
         case (Nil, Var(n, _) :: Delim(s) :: _) if inVar(n) && s == currentToken.word =>
                 onApply {
                    found match {
                       case FoundVar(_, pos, name, _) :: tail =>
                          val fa = FoundArg(scanner.pick(numCurrentTokens), n)
                          found = FoundVar(n, pos, name, Some(fa)) :: tail
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