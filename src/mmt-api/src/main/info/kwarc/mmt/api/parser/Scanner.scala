package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._

import scala.annotation.tailrec

case class Ambiguous(notations: List[ParsingRule]) extends {
  val message = "multiple notations apply: " + notations.map(_.toString).mkString(",")
} with Error(message)

import info.kwarc.mmt.api.parser.ActiveNotation._

/** scans a TokenList against some notations
  * @param tl the TokenList to scan
  *           matched notations are applied to tl, i.e., tl always holds the current TokenList
  */
class Scanner(val tl: TokenList, parsingUnitOpt: Option[ParsingUnit], ruleTableInit: ParsingRuleTable,
              val report: frontend.Report) extends frontend.Logger {
  val logPrefix = "scanner"

  private def logState() {
    log("token list: " + tl)
    log("shifted " + numCurrentTokens)
    active.reverseMap { an =>
      log(an.toString + ", shifted: " + an.numCurrentTokens)
    }
  }

  /** the rules with which we still have to scan
   */
  private var ruleTable = ruleTableInit
  def addRules(that: ParsingRuleTable) {
    ruleTable = ruleTable.add(that)
  }
  
  /** the notations to scan for in the current call to scan */
  private def currentGroup: ParsingRuleGroup = ruleTable.groups.head // ruleTable.groups is nonempty during scanning
  /** the number of Token's currently in tl */
  private var numTokens = tl.length

  /** the current length of tl */
  def length: Int = numTokens // TODO can this be private?

  /** the index of the first non-processed Token */
  private var currentIndex: Int = 0
  /** if non-null, the Token at currentIndex */
  private var currentToken: Token = null
  /** the number of Token's before the left-most ActiveNotation */
  private var numCurrentTokens = 0

  // these could be moved into a separate class
  /** the number of Token's picked since the last resetPicker */
  private var picked = 0

  /** reset picked */
  private def resetPicker() {
    picked = 0
  }

  /** obtain picked */
  private def getPicked = picked

  /** pick some of Token's from the end of the shifted and not-yet-picked Token's
    * @param length the number of Token's to pick
    * @return the TokenSlice representing the picked Token's
    */
  private[parser] def pick(length: Int): TokenSlice = {
    val sl = TokenSlice(tl, currentIndex - picked - length, currentIndex - picked)
    picked += length
    log("picking " + length + " tokens (" + picked + " in total so far): " + sl)
    sl
  }

  /**
   * a string consisting of all the single-character Tokens that follow the current one without whitespace
   *
   * these may be used to lengthen the current Token to match Tokens that were taken apart by the lexer
   */
  private def availableFutureTokens: String = {
    val len = tl.length
    var delim = ""
    var i = currentIndex + 1
    while (i < len) {
      tl(i) match {
        case Token(w, _, false, _) if w.length == 1 =>
          delim += w
          i += 1
        case _ => i = len // stop while rather than return delim
      }
    }
    delim
  }

  /** the currently open notations, inner-most first; initialized with the topRule or empty list */
  private var active: List[ActiveNotation] = {
    parsingUnitOpt.flatMap(_.top).toList.map {topRule => 
       new ActiveNotation(this, List(topRule), ScannerBacktrackInfo(0,0))
    }
  }

  /**
   * precondition: ans.length + closable == active.length
   * @return (n,b):
   *         the first n active notations are closable
   *         b after closing the first n notations, there is an active notation left and it is applicable
   */
  private def checkActive(ans: List[ActiveNotation], closable: Int): (Int, Applicability) = ans match {
    case Nil => (closable, NotApplicable)
    case an :: rest =>
      an.applicable(currentToken, availableFutureTokens) match {
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
        case hd :: _ =>
          an.numCurrentTokens = hd.numCurrentTokens
          hd.numCurrentTokens = 0
      }
      val app = an.applicable(currentToken, availableFutureTokens) //true by invariant but must be called for precondition of an.apply
      assert(app == Applicable) // catch subtle implementation errors
    }
    log(s"applying current notation at $currentToken, found so far: $an, shifted tokens: $numCurrentTokens")
    resetPicker()
    val toClose = an.apply(currentIndex)
    // we count how much active.head picked and
    // give the remaining Tokens back to the surrounding group
    if (leftOpen) {
      active.tail match {
        case Nil =>
          numCurrentTokens = an.numCurrentTokens - getPicked + 1
        case hd :: _ =>
          hd.numCurrentTokens = an.numCurrentTokens - getPicked + 1
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
      resetPicker()
      an.close
    }
    active = active.tail
    log("closed current notation, found: " + an)
    val (from, to) = tl.reduce(an, ruleTable, report)
    val numReducedTokens = to - from
    // the closed notation reduces to 1 additional Token that is shifted in the surrounding group
    currentIndex -= numReducedTokens - 1
    numTokens -= numReducedTokens - 1
    log("reduced from (inclusive) " + from + " to (inclusive) " + (to - 1))
  }

  /** close as many active notations as possible
    * @return true if all notations were closed
    */
  private def closeAllPossible: Boolean = {
    active match {
      case Nil => true
      case hd :: _ =>
        if (hd.closable == Applicable) {
          closeFirst(true)
          closeAllPossible
        } else
          false
    }
  }

  private def advance() {
    log("shifting 1 Token: " + tl(currentIndex))
    active match {
      case Nil => numCurrentTokens += 1
      case hd :: _ => hd.numCurrentTokens += 1
    }
  }
  
  private def restoreBacktrackInfo(an: ActiveNotation) {
    val bti = an.backtrackInfo
    currentIndex = bti.currentIndex
    active match {
      case Nil => numCurrentTokens = bti.numCurrentTokens
      case hd::_ => hd.numCurrentTokens = bti.numCurrentTokens
    }
  }

  /** tail-recursively going through the Token's */
  @tailrec
  private def next() {
    tl(currentIndex) match {
      case ml: MatchedList =>
        advance()
      case ul: UnmatchedList =>
        advance()
      case e: ExternalToken =>
        advance()
      case t: Token =>
        currentToken = t
        logState()
        log("current Token: " + t.word)
        //determine if/how an active notation is applicable
        val (closable, anyActiveApplicable) = checkActive(active, 0)
        log("closable: " + closable + ", next applicable: " + anyActiveApplicable)
        anyActiveApplicable match {
          case Abort =>
            log("aborting")
            // go back to the beginning of the aborted notation
            val aborted = active(closable)
            // drop innermost notations up to and including the one to be aborted, i.e., closable+1 notations
            active = active.drop(closable + 1)
            restoreBacktrackInfo(aborted)
            // shift the token that opened the aborted notation
            advance()
          case Applicable =>
            //close the first active notations that are closable, then apply the next one
            Range(0, closable) foreach {_ => closeFirst(true)}
            applyFirst(false)
          case NotApplicable =>
            val futureTokens = availableFutureTokens
            //openable is the list of notations that can be opened, paired with the length of the delim they match
            //if multiple notations can be opened, we open the one with the longest first delim
            val openable = currentGroup.rules flatMap { not =>
              val delim = not.firstDelimString
              val m = delim.isDefined && ActiveNotation.matches(delim.get, currentToken.word, futureTokens)
              val openArgs = not.notation.openArgs(false)
              // n is true iff there are enough tokens shifted for not to pick from the left
              val n = openArgs == 0 || {
                val availableTokens = active match {
                  case Nil => numCurrentTokens
                  case hd :: _ => hd.numCurrentTokens
                }
                openArgs <= availableTokens
              }
              // the number of tokens shifted in the surrounding group that can be picked
              if (m && n)
                List((not, delim.get.length))
              else
                Nil
            }
	         // TODO add notations without firstDelimString
            log("openable: " + openable.map(_._1).mkString(", "))
            //the longest firstDelim of an openable notation
            val longestDelim = if (openable.isEmpty) -1 else openable.maxBy(_._2)._2
            openable.collect {case o if o._2 == longestDelim => o._1} match {
              case hd :: others =>
                // we allow for syntax overloading: if all alternatives have the same markers, we proceed  
                if (!others.forall(pr => TextNotation.agree(pr.notation, hd.notation))) {
                   throw Ambiguous(hd::others) //better ambiguity-handling could go here (maybe look for next delimiter)
                }
                //open the notation and apply it
                log("opening notation at " + currentToken)
                if (hd.notation.isLeftOpen) {
                  /* at this point, all active notations are right-open
                   * thus, opening a left-open notation, leads to the ambiguity of association, e.g.,
                   * there is no unique reading for a-b+c
                   * closing all closable notations has the effect of associating to the left, i.e., (a-b)+c
                   * that corresponds to the left-to-right reading convention
                   * alternatively, one could flag an error
                   */
                  Range(0, closable) foreach { _ => closeFirst(true) }
                }
                val nct = active match {
                  case Nil => numCurrentTokens
                  case hd::_ => hd.numCurrentTokens
                }
                val an = new ActiveNotation(this, hd::others, ScannerBacktrackInfo(currentIndex, nct))
                active ::= an
                applyFirst(true)
              case Nil =>
                //move one token forward
                advance()
            }
        }
    }
    // go to the next token if possible
    currentIndex += 1
    if (currentIndex < numTokens) {
      next()
    } else {
      //close remaining right-open notations
      val allClosed = closeAllPossible
      if (!allClosed) {
        log("backtracking")
        //active notations left, but no further tokens left -> backtracking
        //drop innermost active notation and shift the token that triggered it
        val abort = active.head
        active = active.tail
        restoreBacktrackInfo(abort)
        advance()
        // got to next token
        currentIndex += 1
        if (currentIndex < numTokens) {
          next()
        }
      }
    }
  }

  /** scans for some notations and applies matches to tl
    * @param ns the notations to scan for
    */
  private def scan(group: ParsingRuleGroup) {
    log("scanning " + tl + " with notations " + group.rules.mkString(","))
    currentIndex = 0
    numCurrentTokens = 0
    next()
    if (active != Nil) throw ImplementationError("active notation left after scanning")
  }
  
  /** scan using all notations in the ruleTable */
  def scan() {
    ruleTable.groups foreach {g =>
       if (!parsingUnitOpt.exists(_.isKilled))
         scan(g)
       ruleTable = ruleTable.tail
    }
  }
}

/** Objects of type Found represent [[TokenSlice]]s that were found in the input
  *
  * The subclasses correspond to the subclasses of Marker.
  */
sealed abstract class Found {
  /** @return start (inclusive) and end (exclusive) of this Token, None if length 0 */
  def fromTo: Option[(Int, Int)]
}

/** represents a [[notations.Delimiter]] that was found */
case class FoundDelim(pos: Int, delim: Delimiter) extends Found {
  override def toString: String = delim.toString

  def fromTo: Some[(Int, Int)] = Some((pos, pos + 1))
}

/** anything but a delimiter */
sealed abstract class FoundContent extends Found {
  /** the number of the corresponding [[Marker]] */
  def number: Int
}

/** represents a [[notations.Arg]] that was found
  * @param slice the TokenSlice where it was found
  *              (as TokenList's are mutable, slice is not necessarily valid in the future)
  * @param n the number of the Arg
  */
sealed abstract class FoundArg extends FoundContent {
  val slice: TokenSlice
  override def toString: String = slice.toString

  def fromTo: Some[(Int, Int)] = Some((slice.start, slice.next))

}

case class FoundSimpArg(slice : TokenSlice, number : Int) extends FoundArg

case class FoundOML(slice : TokenSlice, number: Int, info: LabelInfo) extends FoundArg

/** represents an [[notations.SeqArg]] that was found
  * @param n the number of the SeqArg
  * @param args the arguments that were found
  */
sealed abstract class FoundSeqArg extends FoundContent {
  val args: List[FoundArg]
  override def toString: String = number.toString + args.map(_.toString).mkString(":(", " ", ")")

  def fromTo: Option[(Int, Int)] = if (args.isEmpty) None else Some((args.head.slice.start, args.last.slice.next))

}

case class FoundSimpSeqArg(number : Int, args : List[FoundSimpArg]) extends FoundSeqArg

case class FoundSeqOML(number: Int, args: List[FoundOML], info: LabelInfo) extends FoundSeqArg

/** helper class for representing a single found variable
  * @param pos first Token
  * @param name the variable name
  * @param tp the optional type
  */
case class SingleFoundVar(pos: Int, name: Token, tp: Option[FoundArg]) {
  def to: Int = tp match {
    case Some(t) => t.fromTo.get._2
    case None => pos + 1
  }
}

/** represents a [[notations.Var]] that was found
  *
  * the sequence variable parser is a state machine
  * {{{
  *                                        |---------------- next delim -----------------------------------------|
  * state:  0                       1      |                             2                                   -1  V
  * expect: name --- pick name ---> type, sep., next delim --- else ---> sep, next delim --- next delim ---> skip delim, end
  *   ^                                    | sep                     sep |   | else ^
  *   | ------------ skip sep -------------|------------------------------   |------|
  * }}}
  *
  * @param marker the Var marker found
  */
class FoundVar(val marker: Var) extends FoundContent {
  def number = marker.number
  /** the found variables in reverse order */
  private var vrs: List[SingleFoundVar] = Nil
  /** the current state */
  var state: FoundVar.State = FoundVar.BeforeName

  /** the found variables */
  def getVars: List[SingleFoundVar] = vrs.reverse

  /** start a new variable */
  def newVar(pos: Int, name: Token) {
    vrs ::= SingleFoundVar(pos, name, None)
  }

  /**
   * set the type of the current variable
   * pre: variable exists and has no type
   */
  def newType(fa: FoundArg) {
    vrs = vrs.head.copy(tp = Some(fa)) :: vrs.tail
  }

  override def toString: String = "{V" + (if (state != FoundVar.Done) state else "") +
    " " + getVars.map(v => v.name.toString + v.tp.map(":" + _.toString).getOrElse("")).mkString(",") + " V}"

  def fromTo: Some[(Int, Int)] = Some((vrs.last.pos, vrs.head.to))
}

/** helper object */
object FoundVar {
  type State = Int
  /** state in FoundVar: just before parsing the variable name */
  val BeforeName = 0
  /** state in FoundVar: just after parsing the variable name */
  val AfterName = 1
  /** state in FoundVar: there is a type, some tokens of which have been shifted */
  val InType = 2
  /** final state in FoundVar */
  val Done = -1
}
