package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import collection.immutable.{List, HashMap}
import symbols.{Alias, Declaration, Constant}

/* Error Handling */

/**
 * Generic Error Class
 */
object ParsingError {
  def apply(msg : String, sr : SourceRegion) = SourceError("object-parser", SourceRef(null, sr), msg) //TODO container missing
//("Parsing Error: " + msg + " at (" + start + ","+ end + ")") {
  //def stackTrace : String = getStackTrace.map(x => x.toString).mkString("","\n","")
}


/**
 * A class representing a match of a notation in a list of tokens
 * @param op the operator
 * @param matches the positions of the matches for the String Markers
 */
case class NotationMatch(op : Operator, matches : List[Int])  {
   val beginning = matches.head - op.notation.args.head.length
   val ending = matches.last + op.notation.args.last.length
   val argNr = op.notation.argNr

   def extend(nm : NotationMatch) = NotationMatch(op.extend(nm.op.notation), matches ::: nm.matches)

   override def toString = op.toString + "@(" + matches.mkString(",") + ") "
}


/**
 * Generic Parser Class
 * @param grammar the grammar used for parsing
 */
class Parser(grammar : Grammar, report : frontend.Report) {

  var str : String = ""

  private var operators : List[Operator] = grammar.operators

  private def log(msg : => String) = report("parser",msg)

  private def getOpByPrecedence : List[List[Operator]] = {
    val ops = operators.sortWith((x,y) => x.notation.precedence < y.notation.precedence)
    ops.foldLeft[List[List[Operator]]](Nil)((r,x) =>
      if (r.length == 0)
        List(List(x))
      else if (x.notation.precedence == r.head.head.notation.precedence)
        (x :: r.head) :: r.tail
      else List(x) :: r
    )
  }

  private def toPos(index: Int) : SourcePosition = {
    //val pair  = linesStarts.filter(p => (p._1 <= index)).last
    SourcePosition(index, 0, index)  // the column may be the bogus space character at the end of the line
  }

  private def toRegion(start : Int, stop : Int) : SourceRegion = {
    SourceRegion(toPos(start), toPos(stop))
  }


  def init() {
    grammar.compile()
  }

  private def makeOperators(scope : List[Declaration]) : List[Operator] = {
    val np =  NotationProperties(Precedence(0), AssocNone())
    scope collect {
      case c : Constant =>
        c.not match {
          case Some(not) => new Operator(c.path, not)
          case None =>
            new Operator(c.path, Notation(StrMk(c.path.last) :: Nil, np))
        }
      case a : Alias =>
        new Operator(a.path, Notation(StrMk(a.path.last) :: Nil, np))

      //TODO case Assignment etc
    }
  }

  def parse(o : OMSemiFormal, scope : List[Declaration], offset : Int = 0) : Term = {

    operators = grammar.operators ::: makeOperators(scope)
    log("Started parsing " + o.toString + " with operators : ")
    operators.map(o => log(o.name + " ->" + o.notation.markers))
    log("###")

    o.tokens match {
      case Formal(obj) :: Nil => obj
      case Text(mmt,s) :: Nil =>
        grammar.init(s.length)

        val tklist = tokenize(s, "", offset)._1
        rewrite(tklist).t
      case _  => throw ParsingError("unexpected input: " + o, toRegion(offset, offset))
    }

  }

  /**
   * tokenizes a string
   * @param s the input string
   * @param end the end token marking where tokenization should stop. Used for recursive parsing of pair separators
   * @return the parsed list of tokens and the position in the initial string where tokenization was halted
   */
  private def tokenize(s : String, end : String, offset : Int) : (List[Token], Int) = {
    var i = 0
    var tokens : List[Token] = Nil
    var last = i
    while (i < s.length) {

      checkSeparator(s,i) match {
        case (1,SingSep(m)) =>
          if (last != i) {
            tokens = StrTk(s.substring(last,i), TokenProperties(last + offset,i + offset)) :: tokens
          }
          i += m.length
          last = i
        case (2, SingSep(m)) =>
          if (last != i) {
            tokens = StrTk(s.substring(last,i), TokenProperties(last + offset, i + offset)) :: tokens
          }
          tokens = StrTk(s.substring(i, i + m.length), TokenProperties(i + offset, i + m.length + offset)) :: tokens
          i += m.length
          last = i
        case (1,PairSep(l,r)) =>
          val res = tokenize(s.substring(i + l.length), r, offset + i + l.length)
          tokens = ExpTk(l, res._1, TokenProperties(offset + i, offset + i + l.length + res._2)) :: tokens  //TODO replace l in ExpTk(l,..) with someting else
          i += l.length + res._2
          last = i
        case (2,PairSep(l,r)) =>
          if (last != i) {
            tokens = StrTk(s.substring(last,i), TokenProperties(offset + last, offset + i)) :: tokens
          }
          if (r == end) {
            return (tokens.reverse, i + r.length)
          } else {
            throw ParsingError("Expecting " + end + " found " + r, toRegion(offset + i, offset + i + 1))
          }
        case _ =>
          i += 1
      }
    }
    if (last != i) {
      tokens = StrTk(s.substring(last,i), TokenProperties(offset + last, offset + i)) :: tokens
    }
    (tokens.reverse,s.length)
  }


  /**
   * check if the input string has a valid separator at a certain position
   * @param s the input string
   * @param i the position
   * @return a int encoding the kind of separator found (Single, left in a Pair, right in a pair) and a separator
   */
  private def checkSeparator(s : String, i : Int) : (Int, Separator) = {
    val separators = grammar.separators
    separators.find(x => x match {
      case SingSep(m) => s.substring(i).startsWith(m)
      case PairSep(l,r) => s.substring(i).startsWith(l) || s.substring(i).startsWith(r)
    }) match {
      case None => (0, SingSep(s))
      case Some(SingSep(m)) =>  if (grammar.isDecl(SingSep(m))) (1,SingSep(m)) else (2,SingSep(m))
      case Some(PairSep(l,r)) => if (s.substring(i).startsWith(l)) (1,PairSep(l,r)) else (2,PairSep(l,r))
    }

  }


  private def getTkProps(hd : Token, tl : List[Token]) : TokenProperties = {
    val (min,max) = tl.foldLeft((hd.start, hd.end))((r,x) => (if (x.start < r._1) x.start else r._1,if (x.end > r._2) x.end else r._2))
    TokenProperties(min, max)
  }

  /**
   * rewrites a list of input tokens into a formal object
   * @param in the token list
   * @return the formal term
   */
  private def rewrite(in : List[Token]) : TermTk = {
    rewrite(getOpByPrecedence, in) match {
      case Nil => throw ParsingError("Empty Expression", toRegion(0, 0))
      case hd :: Nil => hd
      case hd :: tl => TermTk(OMA(hd.t,tl.map(_.t)), getTkProps(hd, tl))
    }
  }

  /**
   * Rewrites a list of input tokens by trying to apply operators.
   * @param ops a list of sets of operators representing all operators ordered in equal precedence tiers
   * @param in the list of input tokens
   * @return  the list of rewritten tokens
   */
  private def rewrite(ops : List[List[Operator]],in : List[Token]) : List[TermTk] = {
    var i = 0
    var input = in
    while (i < ops.length) {
      var a = new HashMap[Int,List[NotationMatch]]()
      var j = 0
      while (j < input.length) {
        a += j -> Nil
        j += 1
      }
      tryMatch(ops(i), input) match {
        case None =>
          i += 1
        case Some(l) =>
          log("did rewrite :" + l)
          input = l
          i = 0
      }
    }
    lookupRefs(input)
  }

  /**
   * looks up references in a list of semi formal objects. References can either be symbols that are in scope or variable that are in context
   * @param objs the list of semiformal objects
   * @return the list of formal objects after the lookup was performed
   */
  def lookupRefs(objs : List[Token]) : List[TermTk] = objs map { tk =>
    lookupRef(tk)
  }


  def lookupRef(tk : Token) : TermTk = tk match {
    case TermTk(t,tkp) => TermTk(t, tkp)
    case ExpTk(s, tks, tkp) => rewrite(tks)
    case StrTk(s, tkp) =>
      val matchedOps = grammar.operators.filter(p => p.name.name.toString == s) //TODO: use Names.resolve to find all possible matches?
      val ln = LocalName.parse(s)
      val inContext = grammar.context.isDeclared(ln)
      matchedOps match {
        case Nil =>
          if (inContext) {
            TermTk(OMV(ln), tkp)
          } else {
            TermTk(OMV(ln), tkp) //free variables for more flexible queries
            //throw ParsingError("Lookup failed for symbol " + s, toRegion(tkp.start, tkp.end))
          }
        case hd :: Nil =>
          if (inContext) {
            TermTk(OMV(ln), tkp) //context shadows signature
          } else {
            TermTk(OMID(hd.name), tkp)
          }
        case l => throw ParsingError("Ambiguous lookup for symbol " + s + " : \n" + l.mkString("\n"), toRegion(tkp.start, tkp.end))
      }
  }

  /*
  /**
   * Finds (and applies) the first (if any) valid match in a list of tokens given a list of operators. Being recursive, it has some auxiliary arguments
   * A match is valid if it doesn't cross overlap with another match. This implies that applying it doesn't affect the applicability of other
   * valid matches.
   * @param ops the list of operators
   * @param alltks the initial list of tokens
   * @param tks the list of tokens as searched, always a recursive tail of alltks
   * @param matches the matches (not necessarily valid) found so far indexed by ending position
   * @param pos the current offset position relativ to the initial tokens list. More precisely alltks(pos) is tks.head
   *@return  optionally the list of tokens after the found match is applied
   */
  def findValidMatch(ops : List[Operator], alltks : List[SemiFormalObject],tks : List[SemiFormalObject],
                     matches : HashMap[Int,List[NotationMatch]], pos : Int) : Option[List[SemiFormalObject]] =
    tks match {
    case Nil => None
    case _ =>
      val nwMatches = _addMatches(findMatches(ops, tks, pos), matches)
      log("nwmatches: " + nwMatches)
      val validMatches = nwMatches(pos) map {
        x => (x,_overlaps(x, nwMatches.flatMap(x => x._2).toList))
      } collect {
        case (x,Some(l)) => (x,l)
      }

      log("validMatches: " + validMatches.toString )

      validMatches match {
        case Nil => findValidMatch(ops, alltks, tks.tail, nwMatches, pos + 1)
        case (x,Nil) :: Nil =>  Some(_doApply(alltks, x, pos))
        case (x, y :: Nil) :: Nil =>
          if (x.op.name == y.op.name) {
            x.op.notation.assoc match {
              case AssocNone() =>
                throw ParsingError("Parsing Ambiguous, found consecutive occurences of the same operator with Association None : " + y.toString + " and "+  x.toString)
              case AssocLeft() => Some(_doApply(alltks, x, pos))
              case AssocRight() => findValidMatch(ops, alltks, tks.tail, _addMatches(List(y),matches), pos + 1)
              case AssocSeq() =>
                val nm = x.extend(y)
                log(nm)
                findValidMatch(ops, alltks, tks.tail, _addMatches(List(nm),matches - x.ending), pos + 1)
            }

          } else {
            throw ParsingError("Parsing Ambiguous, equal precedences operators following each other : " +  y.toString + " and " + x.toString)
          }
        case (x, l) :: Nil => throw ParsingError("Parsing Ambiguous, several equal precedence overlapping operators for " + x + " namely :\n" + l.mkString("\n"))
        case l => throw ParsingError("Parsing Ambiguous, cannot decide between several equal precedence matches : \n" + l.mkString("\n"))
      }
      /*
      log("got to fvm with" + tks.toString() + matches.toString)
      (findMatches(ops, tks, pos), matches(pos)) match {
        case (Nil, Nil) => findValidMatch(ops, alltks, tks.tail, matches, pos + 1)
        case (Nil, hd :: Nil) => Some(_doApply(alltks, hd, pos))
        case (Nil, l) => throw ParsingError("Parsing Ambiguous, expected one valid match at this position found : " + l.toString)
        case (nw :: Nil, old :: Nil) =>
          if (old.op.name == nw.op.name) {
            old.op.notation.assoc match {
              case AssocNone() => throw ParsingError("Parsing Ambiguous")
              case AssocLeft() => Some(_doApply(alltks, old, pos))
              case AssocRight() => findValidMatch(ops, alltks, tks.tail, _addMatches(List(nw),matches), pos + 1)
              case AssocSeq() =>
                val nm = old.extend(nw)
                findValidMatch(ops, alltks, tks.tail, _addMatches(List(nm),matches), pos + 1)
            }

          } else {
            throw ParsingError("Parsing Ambiguous equal associativity operators following each other : " +  nw.toString + " and " + old.toString)
          }
        case (l1 , l2) => findValidMatch(ops, alltks, tks.tail, _addMatches(l1,matches), pos + 1)
      }
      */
  }

  /**
   * returns the matches with which a notationmatch overlaps if any
   * @param nm the notation match
   * @param matches the list of notation matches in which we lookup
   * @return  the list of overlapping notation matches
   */
  private def _overlaps(nm : NotationMatch, matches : List[NotationMatch]) : Option[List[NotationMatch]]= {
    val tmpOverlaps =  matches.foldLeft(Nil : List[NotationMatch])( (r,x) => (if (x == nm || nm.beginning > x.ending || nm.ending < x.beginning) r else x :: r))
    log(nm + " tmpOverlaps :" + tmpOverlaps)
    val invalid = tmpOverlaps exists {x =>
     x.matches exists {k =>
        (nm.beginning < k && k < nm.ending && !nm.matches.contains(k))
      }
    }
    log("invalid" + invalid)
    if (invalid){
      None
    } else {
      val overlaps = tmpOverlaps filterNot {x =>
        nm.matches exists {k =>
         (x.beginning < k && k < x.ending && !x.matches.contains(k))
        }
      }
      log("overlaps" + overlaps)
      Some(overlaps)
    }

  }


  /**
   * Adds newly found matches to a hashmap which indexes them by ending position. This is used to detect overlap
   * @param nm the list of matches to be added
   * @param matches the current matches
   * @return the new hashmap
   */
  private def _addMatches(nm : List[NotationMatch], matches : HashMap[Int,List[NotationMatch]]) : HashMap[Int,List[NotationMatch]] = nm match {
    case Nil => matches
    case n :: l =>
      val i = n.ending
      var mtch = _addMatches(l, matches)
      mtch + (i -> (n :: mtch(i)))
  }

  /**
   * Attempts to find a (possibly invalid) match in a list of tokens
   * @param markers the markers being searched
   * @param tks the token list
   * @param pos the offset position
   * @param firstCall whether this is the first function call, used to ensure starting position is correct
   * @return optionally the list of positions where the markers matched
   */
  private def findMatch(markers : List[StrMk], tks : List[SemiFormalObject], pos : Int, firstCall : Boolean) : Option[List[Int]] = (markers,tks) match {
    case (Nil,_) => Some(Nil)
    case (_,Nil) => None
    case (_, Text(mmt, value) :: _) =>
      if (value == markers.head.value) {
        findMatch(markers.tail, tks.tail, pos+1, false)  match {
         case None => None
         case Some(l) => Some(pos :: l)
        }
      } else if (firstCall) {
        None
      } else {
        findMatch(markers, tks.tail,  pos+1, false)
      }
    case _ =>
      if (firstCall) {
        None
      } else {
        findMatch(markers, tks.tail,  pos+1, false)
      }
  }

  /**
   * Finds the matches at a certain position in a list of tokens given a list of available operators
   * @param ops the operators
   * @param tks the token list already starting at the expected offset
   * @param pos the offset
   * @return the list of notation matches
   */
  private def findMatches(ops : List[Operator],tks : List[SemiFormalObject], pos : Int) : List[NotationMatch] = {
    val matches = ops.map(op => (op,findMatch(op.notation.markers, tks, pos, false))) collect {
      case (op, Some(l)) => NotationMatch(op,l)
    }  filter {nm => nm.beginning == pos}
    log("in matches" + pos + matches)
    matches
  }


  /**
   * Applies a notation match to alist of tokens
   * @param tks the tokens
   * @param not the notation
   * @param pos
   * @return the new list of tokens
   */
  private def _doApply(tks : List[SemiFormalObject], not : NotationMatch, pos : Int) : List[SemiFormalObject] = {
    log("got here with" + tks.toString + not.toString + pos.toString)

    /**
     * slices a list of tokens into several lists according to the cut places marked by the matched tokens
     * @param matches the places where string tokens matched
     * @param tks  the list of tokens
     * @param startPos the starting position of the match (including arguments)
     * @param endPos  the ending position of the match (including arguments)
     * @return the sliced list (a list of lists)
     */
    def _getSlices(matches : List[Int], tks : List[SemiFormalObject], startPos : Int, endPos : Int) : List[List[SemiFormalObject]] = matches match {
      case Nil => tks.slice(startPos, endPos + 1) :: Nil
      case _   => tks.slice(startPos, matches.head) :: _getSlices(matches.tail,  tks, matches.head + 1, endPos)
    }


    /**
     * generate arguments from a sliced list
     * @param slices the list slices
     * @param args the arguments, corresponding to the slices (args.length == slices.length is assumed)
     * @param finalArgs auxiliary variable holding the final result through the recursive calls
     * @return the final processed arguments
     */
    def _genArgs(slices : List[List[SemiFormalObject]], args : List[List[ArgMk]],
                 finalArgs : Array[List[SemiFormalObject]]) : List[List[SemiFormalObject]] =
      slices match {
      case Nil => finalArgs.toList
      case _ =>      //TODO arguments appearing twice will be silently overridden
        if (slices.head.length == args.head.length) {
          slices.head.zip(args.head).map(p => finalArgs(p._2.pos) = List(p._1))
        } else if (args.head.length == 1 && slices.head.length > 1) {
          finalArgs(args.head.head.pos) = slices.head
        } else {
          throw ParsingError("Ambiguous args" + slices.toString() + "\n" + args.toString() + finalArgs)
        }
        _genArgs(slices.tail,  args.tail,  finalArgs)
    }


    val slices = _getSlices(not.matches, tks, not.beginning, not.ending)
    log(slices)
    val opArgs = _genArgs(slices, not.op.notation.args, new Array[List[SemiFormalObject]](not.argNr)) map {
      case null => List(Formal(OMHID))
      case l => l
    }
    log("opargs" + opArgs)
    not.op match {
      case b : Binder =>
        val fnArgs = opArgs.zipWithIndex map {x =>
          if (x._2 == b.nameArgNr) {
            if (x._1.length == 1)
              x._1.head
            else
              throw ParsingError("Expected one argument for bound variable name, found : " + x._1.toString)
          } else {
            rewrite(x._1)
          }
        }
        log( "fnargs " + fnArgs)
        val con = b.context(fnArgs)
        grammar.addContext(con)
        val ret = tks.slice(0, not.beginning) ::: List(Formal(OMBIND(OMID(not.op.name), con, rewrite(tks.slice(not.ending + 1, tks.length)).obj)))
        grammar.removeContext(con)
        ret
      case _ =>
        val fnArgs = opArgs.map(rewrite)

        val tm  = if (fnArgs == Nil) {
          Formal(OMID(not.op.name))
        } else {
          Formal(OMA(OMID(not.op.name), fnArgs.map(_.obj)))
        }

        tks.slice(0, not.beginning) ::: (tm :: tks.slice(not.ending + 1, tks.length))
    }

  }
  */

















  private def tryMatch(ops : List[Operator], tks : List[Token]) : Option[List[Token]] = {
    log("trying level : " + ops.toString)
    log("with input : " + tks.toString)
    var i = 0
    while (i < tks.length) {
      matches(ops, tks.slice(i, tks.length)) match {
        case None => None
        case Some(m) =>
          log("found match " + m.toString)
          return Some(checkMatch(ops, tks, m, i, i))
      }
      i += 1
    }
    None
  }

  /**
   * Checks if a match is un-ambiguous and if so applies it
   * @param ops the list of operators with equal precedence to the operator that did match
   * @param tks the token list
   * @param old the operator that matched
   * @param pos the position until which checking was done
   * @param oldStart the start position of the notation for the operator that matched
   * @return the updated list of tokens if match was unambiguous, throws an error otherwise
   */
  private def checkMatch(ops : List[Operator], tks : List[Token], old : Operator, pos : Int, oldStart : Int) : List[Token] = {
    var i = pos + 1
    log("got here with" + old + pos + oldStart)
    while (i < pos + old.notation.mrks.length) {
      val slice = tks.slice(i, tks.length)
      matches(ops, slice) match {
        case None => None
        case Some(nw) => 
        if (nw.name == old.name && i == (oldStart + old.notation.mrks.length - 1)) {
          nw.notation.assoc match {
            case AssocNone()  =>
              val tkProps = getTkProps(tks(i), tks.slice(i + 1, i + nw.notation.mrks.length))
              throw ParsingError("Ambiguous parse for " + tks.toString + ". Perhaps wrong associativity", toRegion(tkProps.start, tkProps.end))

            case AssocLeft()  => return doApply(old, tks, pos)
            case AssocRight() => return checkMatch(ops, tks, nw, i, i)
            case AssocSeq()   => return checkMatch(ops, tks, old.extend(nw.notation), i, oldStart)
          }
        } else {
          val tkProps = getTkProps(tks(i), tks.slice(i + 1, i + nw.notation.mrks.length))
          throw ParsingError("Ambiguous parse at: " + tks.toString, toRegion(tkProps.start, tkProps.end))
        }
      }      
      i += 1
    }
    doApply(old, tks, oldStart)
  }

  /**
   * Checks if one of the operators in a list matches in a list of tokens
   * @param ops the operator list
   * @param tks the token list
   * @return Optionally the matched operator
   */
  private def matches(ops : List[Operator], tks : List[Token]) : Option[Operator] = {
    ops.filter(op =>
      if (op.notation.mrks.length > tks.length){
        false
      } else {
        op.notation.mrks.zipWithIndex.foldLeft(true)((v,r) => v && matches(tks(r._2),r._1))
      }
    ) match {
      case Nil => None
      case m :: Nil => Some(m)
      case l =>
        val tkp = getTkProps(tks.head, tks.tail)
        throw ParsingError("Ambiguous parse at: " + tks.toString + " with " + l, toRegion(tkp.start, tkp.end))
    }
  }

  /**
   * checks if an individual token matches a marker
   * @param tk the token
   * @param mk marker
   * @return true if it matches false otherwise
   */
  private def matches(tk : Token, mk : Marker) : Boolean = { mk match {
    case ArgMk(state) => tk match {
      case TermTk(t,_) => true
      case StrTk(s,_) => if (grammar.isSep(s)) false else true //isBinder || (getType(s) match { case Some(v) => v == state case _ => true})
      case ExpTk(n, tks,_) => true //TODO check
    }
    case StrMk(value) => tk match {
      case StrTk(s,_) => s == value
      case _ => false
    }
  }}

  /**
   * applies an operator in a list of tokens, by extracting the arguments and replacing the sub-list of
   * notation markers with one occurrence of the operator.
   * e.g. for op * and tokens (a, +, b, *, c, +, d) and pos 3 it will return (a, +, *(b,c), +, d)
   * @param op the operator which is to be applied
   * @param tks the list of tokens in which to replace
   * @param pos the start position
   * @return the new list of tokens
   */
  private def doApply(op : Operator, tks : List[Token], pos : Int) : List[Token] = {
    log("op")
    log(op.toString)
    log(tks.toString)
    log(pos.toString)

    val left = tks.slice(0,pos)
    val args = tks.slice(pos, pos + op.notation.mrks.length).zipWithIndex.filter(x => op.notation.mrks(x._2) match {case ArgMk(s) => true case _ => false}).map(_._1)
    val right = tks.slice(pos + op.notation.mrks.length, tks.length)
    op match {
      case binder : Binder =>
        val fnargs = getArgs(op, args)
        log(fnargs.toString)
        val context = binder.context(fnargs)
        grammar.addContext(context)
        val ret = binder.binding match {
          case BindLeft() => TermTk(OMBIND(OMID(op.name), context, rewrite(left).t), getTkProps(tks(pos), args ::: left)) :: right
          case BindRight() => left ::: List(TermTk(OMBIND(OMID(op.name), context, rewrite(right).t), getTkProps(tks(pos), args ::: right)))
        }
        grammar.removeContext(context)
        ret
      case _ => args match {
        case Nil => left ::: TermTk(OMID(op.name),getTkProps(tks(pos), args)) :: right
        case hd :: tl => left ::: TermTk(OMA(OMID(op.name), lookupRefs(args).map(_.t)), getTkProps(tks(pos), args)) :: right
      }

    }
  }

  private def getArgs(op : Operator, tks : List[Token]) : List[Token] = {
    val args : Array[Token] = new Array(op.notation.argNr)
    log("tks" + tks)
    log("op" + op)
    val argMks = op.notation.mrks collect  {
      case x : ArgMk => x
    }
    tks.zip(argMks) collect {
      case (tk,ArgMk(pos)) =>
        log("got here" + tk.toString + " " + ArgMk(pos))
        args(pos) = tk
    }

    log("args" + args)
    log("toList" + args.toList)
    val fnArgs = args.toList map {
      case null => TermTk(OMHID,TokenProperties(0,0))
      case x => x
    }
    op match {
      case b : Binder =>
        fnArgs.zipWithIndex map { p =>
          if (b.nameArgNr == p._2)
            p._1 else {
            lookupRef(p._1)
          }
        }
      case _ =>
        lookupRefs(fnArgs)
    }
  }

}

