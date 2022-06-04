package info.kwarc.mmt.lf.itp

import info.kwarc.mmt.api.{ErrorHandler, ErrorThrower}
import info.kwarc.mmt.api.documents.InterpretationInstructionContext
import info.kwarc.mmt.api.objects.{Context, PlainSubstitutionApplier, Stack, Term, Typing, VarDecl}
import info.kwarc.mmt.api.parser.{NotationBasedParser, ParseResult, ParsingUnit, SourceRef}
import info.kwarc.mmt.api.proving.itp.Goal
import info.kwarc.mmt.lf.itp.Tactics._

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.{CharArrayReader, CharSequenceReader}

/**
  * class containing hard coded parser for tactics and several generic helper parser
  * @param ip the proof manager, needed as an argument for concrete tactics like [[assume]]
  */
class TacticParser(val ip :InteractiveProof) extends JavaTokenParsers {

  /**
    * parser representing a valid hypothesis name
    */
  val hypName: Regex = "([a-z]|[A-Z])+([a-z]|[A-Z]|[0-9]|[/_])*".r

  /**
    * parser representing a valid hypothesis name with a slash somewhere after the first character (usually used for explicitely named unknownws)
    */
  val hypNameWithSlash : Regex = "([a-z]|[A-Z]|/)+([a-z]|[A-Z]|[0-9]|[/_])*".r
  /**
    * parser that matches any expression
    */
  val anyExpr : Regex  = ".*".r
  /**
    * parser that matches any expression inside of matching round brackets
    */
  val anyExprBrack : Regex  = "\\(.*\\)".r
  /**
    * parser that matches any expression in matching round brackets unless it contains keywords (currently only the keyword `in`)
    */
  val anyExprBrackNoKeyWord : Parser[String]  = anyExprBrack >> {s => if (s.contains("in")) {failure("should not contain keyword \"in\"")} else {success(s)} }
  /**
    * parser that matches an arbitrary amount of whitespace characters
    */
  val spaces : Regex = """[ \n\t\s]*""".r
  /**
    * parser that matches at least one whitespace character
    */
  val nespaces : Regex = """[ \n\t\s]+""".r

  /**
    * prefents the implicit skipping of whitespaces in parser, otherwise a parser of the shape A ~ B could parse strings like AB
    */
  override val skipWhitespace  = false


  /**
    * parses a raw mmt term
    * @param t the mmt expression
    * @return the parsed term and the found unknowns
    */
  def simpleTermParser(t : String) : ( Term, Context) = {

    val c = ip.slvr.controller
    val nbp = c.extman.get(classOf[NotationBasedParser]).head
    val cxt = ip.pr.currentState.head.ctx  ++ (if (ip.mp.isDefined) Context(ip.mp.get) else Context())
    val pu = ParsingUnit(SourceRef.anonymous("interactive") , cxt , t ,InterpretationInstructionContext(ip.slvr.controller.getNamespaceMap) )
    val eh : ErrorHandler =  ErrorThrower
    val ParseResult(u,f,tt) = nbp(pu)(eh)
    (tt, u )
  }


  /**
    * parses a raw mmt term and adds unknowns to the solver but does not add them as goals; this behaviour is useful for when
    * one is shure the unknowns get solved anyway and therefore don't need to be added explicitely as goals to the current proof
    * @param t the mmt expression as string which will be parsed
    * @return the parsed term and the found unknowns
    */
  def currGoalParseTerm(t : String) : ( Term, Context) = {

    val c = ip.slvr.controller
    // needed for parsing of the parameter "t"
    val nbp = c.extman.get(classOf[NotationBasedParser]).head
    val cxt = ip.pr.currentState.head.ctx  ++ (if (ip.mp.isDefined) Context(ip.mp.get) else Context())
    val pu = ParsingUnit(SourceRef.anonymous("interactive") , cxt , t ,InterpretationInstructionContext(ip.slvr.controller.getNamespaceMap) )
    val eh : ErrorHandler =  ErrorThrower
    // do the actual parsing
    val ParseResult(u,f,tt) = nbp(pu)(eh)
    val Goal(g , gctx  , ukn) = ip.pr.getCurrentGoal
    ip.pr.update(Goal(g , gctx  , ukn )) //TODO: probably useless line since it just replaces the old goal with the new one (i.e. remove it)
    ip.slvr.addUnknowns(u, None)
    (tt, u )
  }


  /**
    * parses a raw mmt term and adds unknowns to the goals of the current proof. This version also tries to infer types of unknowns more eagerly
    * @param t the mmt expression as string which will be translated to MMT's [[Term]]-class
    * @return the parsed term and the found unknowns
    */
  def currGoalParseTermA(t : String) : ( Term, Context) = {

    // helper function for generating new names that are
    def newnames(ctx0 : Context, tt : Term): (Context, Term) = {
      val ps = ip.slvr.getPartialSolution
      var res = Context()
      var restt = tt

      def loop(ctx : List[VarDecl]) : Unit = ctx match {
        case Nil =>
        case (x::xs) => {
          val (newn , subn) = Context.pickFresh(Context(xs : _ *) ++ res ++ ps , x.name)
          restt = restt.substitute(subn)(PlainSubstitutionApplier)
          res = res ++ x.copy(name = newn)
          loop(xs)
        }
      }
      loop(ctx0.variables.toList)
      (res , restt)
    }

    val c = ip.slvr.controller
    val nbp = c.extman.get(classOf[NotationBasedParser]).head
    val cxt = ip.pr.currentState.head.ctx  ++ (if (ip.mp.isDefined) Context(ip.mp.get) else Context())
    val pu = ParsingUnit(SourceRef.anonymous("interactive") , cxt  , t ,InterpretationInstructionContext(ip.slvr.controller.getNamespaceMap) )
    val eh : ErrorHandler =  ErrorThrower
    val ParseResult(u0,frees,tt0) = nbp(pu)(eh)
    val gl@Goal(g , gctx , ukn) = ip.pr.getCurrentGoal

    val (u,tt1) = newnames(u0,tt0)
    lazy val sub = u.map(x => new ProofUtil.ExtendUnkownInTerm( x.name , gctx ))
    val tt = if (gctx.isEmpty) tt1 else sub.foldLeft(tt1)((tres , sub) => sub(tres , gctx))
    ip.slvr.addUnknowns(u , None)
    val tmp =  ip.slvr.inferType(tt , false)(Stack(ip.pr.getCurrCtx), ip.hist)
    val tmp1 = ip.slvr.getPartialSolution.filter(x => u.index(x.name).isDefined)
    val ukres = tmp match {
      case None => throw new TacticError("could not infer type of: " + ip.slvr.presentObj(tt))
      case Some(ttp) => {
        val (res, ures ) = ProofUtil.standAloneTypeCheck(tt , ttp , tmp1 , ip.pr.getCurrCtx, ip.slvr)
        Context(ures.map(x => if (x.df.isDefined && ProofUtil.containsMissing(x.df.get))   x.copy(df = None) else x ) : _ *)
      }
    }


    ukres.foreach(x => {
      if (x.df.isDefined && ! ProofUtil.containsMissing(x.df.get)) {
        ip.slvr.solve(x.name , x.df.get)(ip.hist)
      }
      if (x.tp.isDefined && ! ProofUtil.containsMissing(x.tp.get)) ip.slvr.solveTyping(Typing(Stack(gctx) , x.toTerm , x.tp.get ))(ip.hist)
    })


    val newuks = ukres.filter(x => x.df.isEmpty)
    val newukgoals = newuks.map(v => Goal(ProofUtil.remOuterFree(v.tp.get , gctx) , ip.pr.getCurrCtx, v.name))
    ip.pr.currentState.append(newukgoals : _*)
    val ukret = ukres.map(v => {
      val newtp = v.tp match {
        case None => None
        case Some(tpp) => if (ProofUtil.containsMissing(tpp)) None else Some(tpp)
      }
      val newdf = v.df match {
        case None => None
        case Some(dff) => if (ProofUtil.containsMissing(dff)) None else Some(dff)
      }
      v.copy(tp = newtp , df = newdf)
    })
    val ttres = ip.slvr.substituteSolution(tt)
    (ttres ,ukret)
  }

  //TODO: this probably needs rework since these parsers were made during a time where the author did not fully understand how parser combinators work and are therefore potentially redundant
  /**
    * this class adds several additional little helper parers
    * @tparam T return type of the parsers
    */
  abstract class EParser[+T] extends Parser[T] {

    /**
      * parser that chains two
      * @param p
      * @tparam A
      * @return
      */
    def ?~>[A](p : => Parser[A]) : Parser[Option[A]] = {
      this ~> """\s*""".r ~ p.? >> {
        case ws ~ (r@Some(pr)) => if (ws.length > 0) success(r) else failure("arguments have to be separated by whitespace")
        case ws ~ None => success(None)
      }
    }

    /**
      *
      * @param p
      * @tparam A
      * @return
      */
    def ??~>[A](p : => Parser[Option[A]]) : Parser[Option[A]] = {
      this ~> """\s*""".r ~ p >> {
        case ws ~ (r@Some(pr)) => if (ws.length > 0) success(r) else failure("arguments have to be separated by whitespace")
        case ws ~ None => success(None)
      }
    }

    /**
      *
      * @param p
      * @tparam A
      * @return
      */
    def &~>[A](p : => Parser[A]) : Parser[A] = {
      this ~> """\s+""".r ~> p

    }

    /**
      *
      * @param p
      * @tparam A
      * @return
      */
    def &~[A](p : => Parser[A]) : Parser[~[T,A]] = {
      this ~ """\s+""".r ~ p ^^ {case a ~ b ~ c => new ~(a , c) }

    }

    /**
      *
      * @param p
      * @tparam A
      * @return
      */
    def ??~[A](p : => Parser[Option[A]]) : Parser[~[T,Option[A]]] = {
      this ~ """\s*""".r ~ p >> {
        case a ~ ws ~ (r@Some(pr)) => if (ws.length > 0) success(new ~(a,r)) else failure("arguments have to be separated by whitespace")
        case a~ ws ~ None => success(new ~(a,None))
      }
    }

    def ?~[A](p : => Parser[A]) : Parser[~[T,Option[A]]] = {
      this ~ """\s*""".r ~ p.? >> {
        case a ~ ws ~ (r@Some(pr)) if ws.nonEmpty => success(new ~(a,Some(pr)))
        case a ~ ws ~ (Some(pr))  if (ws.length <= 0) => {
          val aa = a
          val prr = pr
          val dbg = ()
          failure("arguments have to be separated by whitespace error(123)")
        }
        case a~ ws ~ None => success(new ~(a,None))
      }
    }

    def ?~:[A](p : => Parser[A]) : Parser[~[T,Option[A]]] = {
      this ~ """\s*""".r ~ p.? >> {
        case a ~ ws ~ (r@Some(pr)) if ws.nonEmpty => success(new ~(a,Some(pr)))
        case a ~ ws ~ (Some(pr))  if (ws.length <= 0) => {
          val aa = a
          val prr = pr
          val dbg = ()
          failure("arguments have to be separated by whitespace error(123)")
        }
        case a~ ws ~ None => success(new ~(a,None))
      }
    }


    /**
      * turns a parser p into a parser that removes/ignores surrounding white space
      * @param p
      * @tparam A
      * @return
      */
    def done[A](p : => Parser[A]) : Parser[A] = {
      """\s*""".r ~> p <~ """\s*""".r
    }


  }

  /**
    * implicit conversion from [[Parser]] to [[EParser]]
    * @param p
    * @tparam A
    * @return
    */
  implicit def ptoep[A](p : Parser[A]) : EParser[A] = {
    new EParser[A]{override def apply(i : Input) = p.apply(i)}
  }

  /**
    * needed so the implicit literal parsers also work for [[EParser]]
    * @param s
    * @return
    */
  implicit def eliteral(s : String): EParser[String] = {
    ptoep[String](literal(s))
  }

  /**
    * needed so the implicit regex parsers also work with [[EParser]]
    * @param r
    * @return
    */
  implicit def eregex(r : Regex) : EParser[String] = {
    ptoep(regex(r))
  }

  /**
    * parser for terms that have to be bracketed unless they are atomic. Examples include  (a + b), a, (a)
    * @param p the parser for the term in brackets/the atomic term
    * @param initcount this parameter signalizes how many open brackets should be assumed. A value of >0 indicates that
    *                  the parsed term should be treated as if there where already initcount-many opening brackets
    *                  For example when initcount = 2 then the term "a + b)*(c + d))" would be valid
    * @param removeOuterBrackets if true removes the outer brackets from the parse result
    * @tparam A
    * @return
    */
  def inbalbracksOpt[A](p : => Parser[A] , initcount: Int  = 0 , removeOuterBrackets : Boolean = false) : Parser[A] = {
    new EParser[A]{
      override def apply(i : Input): ParseResult[A] = {
        if(i.atEnd) return Failure("empty expression", i)
        var curr : Char = i.first
        var rest = i.rest
        var oldrest = rest
        while((! i.atEnd) && List(' ' , '\t' , '\n' ).contains(curr)  ){
          curr = rest.first
          oldrest = rest
          rest = rest.rest
        }
        if(curr == '(' || initcount > 0) {
          var bcount = initcount + {if (curr == '(') 1 else 0}
          val newi : ArrayBuffer[Char] = ArrayBuffer[Char](curr)
          while ((!rest.atEnd) && bcount != 0){
            curr = rest.first
            oldrest = rest
            rest = rest.rest
            if (curr == '(') {
              bcount += 1
            }else if (curr == ')'){
              bcount -= 1
            }
            newi.append(curr)

          }
          if(removeOuterBrackets){newi.remove(0) ; newi.remove(newi.length  - 1)}
          val newi0  = new CharArrayReader(newi.toArray,0)
          if (rest.atEnd) oldrest  = rest
          p.apply(newi0) match {
            case Success(result, next) => {
              if (! next.atEnd) return Failure("could not parse argument", next)
              else {
                Success(result , rest)
              }
            }
            case ns : NoSuccess => ns
          }
        }else {
          val newi : ArrayBuffer[Char] = ArrayBuffer[Char](curr)
          while((! rest.atEnd) && ! List(' ' , '\t' , '\n' ).contains(curr)){
            curr = rest.first
            oldrest = rest
            rest = rest.rest
            if (! List(' ' , '\t' , '\n' ).contains(curr))  newi.append(curr)
          }
          if (rest.atEnd) oldrest  = rest
          val newi0  = new CharArrayReader(newi.toArray,0)
          p.apply(newi0) match {
            case Success(result, next) => {
              if (! next.atEnd) return Failure("could not parse argument", next)
              else {
                Success(result , oldrest)
              }
            }
            case ns : NoSuccess => ns
          }
        }
      }
    }
  }


  /**
    * tactic for chaining multiple tactics together as in "fix x; assume h ; use h"
    * @return
    */
  def multiTacticPA : Parser[Tactic] = {
    new EParser[Tactic] {
      override def apply(in: Input): ParseResult[Tactic] = {
        val src = in.source.toString
        val strs = src.split(";")
        Success(multiTactic(strs.toList, TacticParser.this) , new CharSequenceReader("", 0))
      }
    }
  }
  // parsers for hardcoded tactics
  def useP : Parser[Tactic] = ("use"  &~> inbalbracksOpt(anyExpr)) ^^ {h => { val tmp = currGoalParseTermA(h) ; use(tmp._1 , tmp._2)}}
  def assumeP : Parser[Tactic] = ("assume"| "asm") ?~>  hypName ^^ {h => assume(h)}

  def letP : Parser[Tactic] = "let"  &~> hypName  &~ ( anyExprBrack | hypName ) ^^ {case h  ~ t => let(h ,  currGoalParseTermA(t))}


  def applyP : Parser[Tactic] = ( "apply" | "app")  &~> hypName &~ rep1sep( inbalbracksOpt(anyExpr), nespaces) ^^ {case h  ~ ls => applyT(h, ls.map(s => currGoalParseTermA(s)))}

  def subproofP : Parser[Tactic] = "subproof"  &~> hypName  &~( hypName| anyExprBrack ) ^^ {case h  ~ t => subproof(h , currGoalParseTermA(t))}
  def preferP : Parser[Tactic] = "prefer" &~> decimalNumber ^^ (d => prefer(d.toInt))
  def backwardP : Parser[Tactic] = ("bwd" | "backward") &~> anyExpr ^^ {ex =>backward(currGoalParseTermA(ex)) }
  def fixP : Parser[Tactic] = ("fix") ?~> hypName ^^ {h => fix(h) }
  // def undoP : Parser[Tactic] = "undo" ^^ {x => undo()}

  def unfoldArgsP : Parser[Option[String] ~ Option[String]] = {
    val opt1 = "in" &~> hypName
    val opt2 = "at" &~> decimalNumber
    ( opt1 ?~ opt2 >> {case x ~ y => success(new ~(Some(x) , y))} | opt2 >> {y => success(new ~(None, Some(y)))})
  }


  def unfoldP: Parser[Tactic] = ("unfold" | "unf" ) &~> hypName ?~ unfoldArgsP ^^ {case x ~ Some(in ~ at)  => unfold(x,in,at) ; case x ~ None => unfold(x,None, None)}
  def deleteP : Parser[Tactic] = ("delete" | "del") &~> hypName ^^ {x => delete(x)}

  def multiTacticP : Parser[Tactic] =  rep1sep("""[^;]*""".r , ";") ^^ {ls =>multiTactic(ls, this) }

  // def addunknownP : Parser[Tactic]= ("aduk" | "addunkown") &~> hypName ^^ {x => addunknown(x)}
  //  def setunkownP : Parser[Tactic] = ("setuk" | "setunkown") &~> hypNameWithSlash &~ ( hypNameWithSlash| anyExprBrack ) ^^ {case h ~ t => setunkown(h , currGoalParseTermA(t))}
  def voidP : Parser[Tactic] = "void" ^^ {x => void()}
  def ignoreP : Parser[Tactic] = "ignore" ^^ {x => ignore()}
  def messageP : Parser[Tactic] = "message" &~> anyExprBrack ^^ {exp => message(exp)}

  def simpArgsP : Parser[Option[String] ~ Option[String]]  =  (((anyExprBrackNoKeyWord &~ ("in" &~> hypName)) >> {case a ~ b => success(new ~(Some(a) , Some(b))) }) | (failure("").? ~ ("in" &~> hypName) >> {case None ~ b => success(new ~(None.asInstanceOf[Option[String]], Some(b)))}) | (anyExprBrackNoKeyWord >> {m => success(new ~(Some(m) , None))}) )
  def simpP : Parser[Tactic] = "simp" ?~>  simpArgsP ^^ {case Some(Some(e) ~ x) => simp(Some(currGoalParseTermA(e)) , x); case Some(None ~ x) => simp(None , x) ; case None => simp(None, None)}
 // def tryP : Parser[Tactic]  = "try" &~> "(" ~> parseSingleTactic <~ ")" ^^ {t => Try(t)}
  def repeatP : Parser[Tactic] = ("repeat" | "rep" ) &~> decimalNumber &~ ("(" ~> parseSingleTactic <~ ")") ^^ {case d ~ t => repeat(Some(d.toInt) , t)}
  def applytoP : Parser[Tactic] = ("applyto" | "apt") &~> repsep(decimalNumber , ":") &~ ("(" ~> parseSingleTactic <~ ")") ^^ {case ls ~ t => applyto(ls.map(_.toInt) ,  t )}
  def revertP : Parser[Tactic] = ("rev" | "revert") &~> rep1sep(hypName  , whiteSpace) ^^ {ls => revert(ls)}

  def fixsP : Parser[Tactic] = "fixs" ?~>  rep1sep(hypName , nespaces) ^^ {x => fixs(x)}
  def gettypeP : Parser[Tactic] = "gettype" &~> """.*""".r ^^ {x => val tmp = simpleTermParser(x)  ; gettype(tmp._1 ,tmp._2)}
  def gettyperawP : Parser[Tactic] = "gettyperaw" &~> """.*""".r ^^ {x => val tmp = simpleTermParser(x)  ; gettyperaw(tmp._1 ,tmp._2)}
  def printrawP : Parser[Tactic] = "printraw" &~> """.*""".r ^^ {x => val tmp = simpleTermParser(x) ; printraw(tmp._1)}
  def buildP : Parser[Tactic] = "build" &~> """.*""".r ^^ {x => val tmp = simpleTermParser(x) ; build(tmp._1 , tmp._2)}
  def forwardP : Parser[Tactic] = ("fwd" | "forward") &~> hypName &~ inbalbracksOpt(anyExpr) &~ rep(inbalbracksOpt(anyExpr)) ^^ {case h ~ x ~ ls => val tmp = currGoalParseTermA(x); val tmps = ls.map(y => currGoalParseTermA(y)); forward(h , tmp._1 , tmp._2, tmps) }


  /**
    * list containing all available parser
    */
  lazy val newTactics : List[Parser[Tactic]] = ip.slvr.rules.getOrdered(classOf[InteractiveTacticParseRule]).map(_.parseTactic(this)).map(x => remws(x))
  lazy val allTactics0 : List[Parser[Tactic]] = (List(fixP, fixsP, simpP ,assumeP, useP, letP,  applyP, backwardP  , preferP , voidP , messageP , subproofP , unfoldP  , ignoreP, deleteP , repeatP , applytoP, gettypeP,  gettyperawP, printrawP, buildP , forwardP)).map(x => remws(x)) ++ newTactics // ++ newTactis.map(x => x.tacticParserGenerator(ip).asInstanceOf[Parser[Tactic]])).map(x => remws(x))
  lazy val allTactics : List[Parser[Tactic]] = allTactics0.map(x =>  x <~ not(".*"))
  /**
    * removes/skips the left/rightmost whitespace from an expression(needed since whitepsace isn't skipped by default)
    * @param p the concrete parser
    * @tparam A
    * @return parser without the outmost whitespaces
    */
  def remws[A](p : Parser[A])  = spaces ~> p <~ spaces


  /**
    * Parser that goes though all available tactics-parser an tries all until one doesn't fail. Fails otherwise
    * @return the succseeding parser
    */
  def parseSingleTactic : Parser[Tactic] = {

    // the inner `loop` of the function
    def parseSingelTacticL(ls : List[Parser[Tactic]]) : Parser[Tactic] = ls match {
      case List() => failure("no tactic found")
      case x::xs => (x ||| parseSingelTacticL(xs))
    }

    parseSingelTacticL(allTactics)
  }
}