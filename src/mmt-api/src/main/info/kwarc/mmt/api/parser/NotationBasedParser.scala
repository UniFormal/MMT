package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import modules._
import notations._
import objects._
import symbols._
import documents._
import utils._

/** couples an identifier with its notation */
case class ParsingRule(name: ContentPath, alias: List[LocalName], notation: TextNotation) {
  /** the first delimiter of this notation, which triggers the rule */
  val firstDelim: Option[Delim] = notation.parsingMarkers collectFirst {
    case d: Delimiter => d.expand(name, alias)
    case SimpSeqArg(_, d, _) => d
    case LabelSeqArg(_,d,_,_) => d
  }
  val firstDelimLength = firstDelim.map(_.text.length).getOrElse(-1)
}

/** a set of parsing rules with the same precedence, see [[NotationOrder]] */
case class ParsingRuleGroup(precedence: Precedence, rules: List[ParsingRule]) {
  /** the group with additional rules */
  def add(rs: List[ParsingRule]) = copy(rules = (rules ::: rs).distinct)
}
/** a set of parsing rule groups, ordered by increasing precedence,
 *  used by [[NotationBasedParser]] to apply notations in precedence-order
 */
case class ParsingRuleTable(groups: List[ParsingRuleGroup]) {
  /** this without the first group */
  def tail = ParsingRuleTable(groups.tail)
  /** merges two tables, preserving ordering */
  def add(t: ParsingRuleTable) = {
    var nw: List[ParsingRuleGroup] = Nil
    var old1 = groups
    var old2 = t.groups
    while (old1.nonEmpty && old2.nonEmpty) {
      if (old1.head.precedence < old2.head.precedence) {
        nw ::= old1.head
        old1 = old1.tail
      } else if (old1.head.precedence > old2.head.precedence) {
        nw ::= old2.head
        old2 = old2.tail
      } else {
        nw ::= old1.head add old2.head.rules
        old1 = old1.tail
        old2 = old2.tail
      }
    }
    ParsingRuleTable(nw.reverse)
  }
}

/**
 * names are locally meaningful: variables bound in the contexts (to be parsed as OMV), fields of dependent OML sequences (to be parsed as OML)
 */
case class BoundName(name: LocalName, isOML: Boolean) {
  def toTerm: Term = if (isOML) OML(name) else OMV(name)
}
object BoundName {
  def getVarNames(bns: List[BoundName]) = bns collect {
    case BoundName(n,false) => n
  }
}

/**
 * A notation based parser
 */
class NotationBasedParser extends ObjectParser {
  override val logPrefix = "object-parser"

  def isApplicable(format: String): Boolean = format == "mmt"

  private lazy val prag = controller.pragmatic
  private lazy val lup = controller.globalLookup

  /** constructs a SourceError, all errors go through this method */
  private def makeError(msg: String, reg: SourceRegion)(implicit pu: ParsingUnit, errorCont: ErrorHandler): Unit = {
    val ref = SourceRef(pu.source.container, reg)
    val err = SourceError(logPrefix, ref, msg)
    errorCont(err)
  }


  /**
    * top level parsing function: initializes input state and implicit arguments, calls main parsing method, and builds ParseResult
   * @param pu the parsing unit
   */
  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): ParseResult = {
    if (pu.isKilled) {
      return DefaultObjectParser(pu)
    }
    implicit val puI = pu
    val variables = new Variables
    val tm = parse(pu, variables, errorCont)
    val (unk,free) = variables.getVariables
    ParseResult(unk,free,tm)
  }

  /** main parsing method */
  private def parse(implicit pu: ParsingUnit, variables: Variables, errorCont: ErrorHandler): Term = {
    //gathering notations and lexer extensions in scope
    val (parsing,lexing,_) = getRules(pu.context,Some(pu.iiContext))
    val notations = tableNotations(parsing)
    log("parsing: " + pu.term + " in context " + pu.context)
    log("rules:")
    logGroup {
      log("parsing")
      notations.groups.foreach(n => log(n.toString))
      log("lexing")
      lexing.foreach(r => log(r.toString + " (priority " + r.priority + ")"))
    }
    val escMan = new EscapeManager(lexing)
    val tl = TokenList(pu.term, escMan, pu.source.region.start)
    if (tl.getTokens.isEmpty) {
      makeError("no tokens found: " + pu.term, pu.source.region)
      DefaultObjectParser(pu).toTerm
    } else {
        /* scanning: the scanner initially scans with top rule, then with all notations in increasing order of precedence
         but we will actually call ul.scan only in makeTerm so that we can add local notations first
        */
        // TODO does it make sense to sort by meta-theory-level first, then by precedence?
        val ul = new UnmatchedList(tl, Some(pu), notations, controller.report)
        /* turn the syntax tree into a term
           to the outside, this term is its own block, so bound names introduced by the term are dropped
           to the inside, we are not in a block yet
         */
        val (_,tm) = logGroup {
          makeTerm(ul, Nil, false)
        }
        SourceRef.update(tm,pu.source) // weirdly necessary, apparently
        log("parse result: " + tm.toString)
        tm
    }
  }

  /**
    * builds parser's notation context based on content in the controller
    * and the scope (base path) of the parsing unit
    * returned list is sorted (in increasing order) by priority
    */
  protected def tableNotations(nots: List[ParsingRule]): ParsingRuleTable = {
    val qnotations = nots.groupBy(x => x.notation.precedence).toList.map {
      case (p, ns) => ParsingRuleGroup(p, ns)
    }
    //      log("notations in scope: " + qnotations)
    val qnotationsOrdered = qnotations.sortWith((p1, p2) => p1.precedence < p2.precedence)
    ParsingRuleTable(qnotationsOrdered)
  }

  private type RuleLists = (List[ParsingRule], List[LexerExtension], List[NotationExtension])

  private def unappNotation(e: ContentElement, altNames: List[LocalName], args: Int) = {
    val ms = Mixfix(Delim(e.name.toString) :: Range(0, args).toList.map(SimpArg(_)))
    val n = new TextNotation(ms, Precedence.infinite, None, false)
    ParsingRule(e.path, altNames, n)
  }
  
  /** auxiliary function to collect all lexing and parsing rules in a given context */
  private def getRules(context: Context, iiCO: Option[InterpretationInstructionContext]): RuleLists = {
    val support = context.getIncludes
    //TODO we might also collect notations attached to variables
    support.foreach {p =>
      controller.simplifier(p)
    }
    val iis = iiCO.map(_.getInstructions).getOrElse(Nil)
    val docRules = iis.mapPartial {
      case r: DocumentRule => r.rule
      case _ => None
    }
    var nots: List[ParsingRule] = Nil
    var les: List[LexerExtension] = docRules.collect {
      case le: LexerExtension => le
    }
    var notExts: List[NotationExtension] = Nil
    support.foreach {p => lup.forDeclarationsInScope(OMMOD(p)) {case (_,via,d) =>
      // TODO d must be translated along via first
      // TODO notations from implicit structures and realizations are already in the theory; collecting them again causes ambiguity
      d match {
        case c: Constant => // Declaration with HasNotation might collect too much here
          var names = (c.name :: c.alternativeNames).map(_.toString) //the names that can refer to this declaration
          if (c.name.last == SimpleStep("_")) names ::= c.name.init.toString
          //the unapplied notations consisting just of the name
          val unapp = names map (n => new TextNotation(Mixfix(List(Delim(n))), Precedence.infinite, None, false))
          val app = c.not.toList
          (unapp:::app).foreach {n =>
            nots ::= ParsingRule(c.path, c.alternativeNames, n)
          }
        case r: RuleConstant => r.df.foreach {
          case ne: NotationExtension =>
            notExts ::= ne
          case le: LexerExtension =>
            les ::= le
          case rt: uom.RealizedType =>
            rt.lexerExtension.foreach {les ::= _}
          case _ =>
        }
        case de: DerivedContentElement =>
          nots ::= unappNotation(de, Nil, 0)
        case nm: NestedModule =>
          val args = nm.module match {
            case t: Theory => t.parameters.length
            case v: View => 0
          }
          nots ::= unappNotation(nm.module, Nil, args)
        case _ =>
      }
    }}
    les = les.sortBy(- _.priority)
    (nots.distinct, les.distinct, notExts.distinct)
  }

  /* like getRules but for a theory expression (currently only called for local notations) */
  private def getRules(thy: Term): RuleLists = {
    // this used to simplify thy first, but that may be dangerous and/or inefficient
    thy match {
      case OMPMOD(mp,_) => getRules(Context(mp), None)
      case AnonymousTheoryCombinator(at) =>
        //we could also collect all notations in decls, but we do not have parsing rules for OML's
        at.mt.map(m => getRules(Context(m), None)).getOrElse((Nil,Nil,Nil))
      case _ => (Nil,Nil,Nil) // TODO only named theories are implemented so far
    }
  }


  /*
   * declarations of the unknown variables (implicit arguments, missing types, etc.
   *
   * the variable names are irrelevant as long as they are unique within each call to the parser
   * Moreover, we make sure the names are chosen the same way every time to support change management.
   */
  private class Variables {
    // due to the recursive processing, variables in children are always found first
    // so adding to the front yields the right order
    /** unknowns */
    private var unknowns: List[VarDecl] = Nil
    private var counter = 0

    /** free variables (whose types may depend on the unknowns) */
    private var freevars: List[LocalName] = Nil
    /** @return the free variables detected so far */
    def getFreeVars = freevars

    /** @return returns the unknown and free variables at the end, free vars have unknown types */
    def getVariables(implicit pu: ParsingUnit): (Context,Context) = {
      val fvDecls = freevars map {n =>
        val tp = newUnknown(newType(n), Nil) // types of free variables must be closed; alternative: allow any other free variable
        VarDecl(n,tp)
      }
      (unknowns, fvDecls)
    }
    private def next = {
      val s = counter.toString
      counter += 1
      s
    }
    /** name of an omitted implicit argument */
    def newArgument = ParseResult.VariablePrefixes.implicitArg / next
    /** name of the omitted type of a variable */
    def newType(name: LocalName) = LocalName("") / name / next
    /** name of an explicitly omitted argument */
    def newExplicitUnknown = ParseResult.VariablePrefixes.explicitUnknown / next

    /** generates a new unknown variable, constructed by applying a fresh name to all bound variables */
    def newUnknown(name: LocalName, boundNames: List[BoundName])(implicit pu: ParsingUnit) = {
      unknowns ::= VarDecl(name)
      // handling of shadowing: we assume that an unknown cannot depend on a shadowed variable
      // so we remove shadowed (i.e., earlier) occurrences of bound variables
      // There are reasonable cases, where the unknown does depend on a shadowed variable, e.g., in [x: type, c: x, x: type] c = c.
      // The behavior in still unspecified in these cases.
      val boundNamesD = boundNames.reverse.distinct.reverse
      //apply meta-variable to all bound variables in whose scope it occurs
      // this line determines whether implicit arguments may depend on OML names
      val mayUse = boundNames.map(_.toTerm)
      checking.Solver.makeUnknown(name, mayUse)
    }

    /** generates a new unknown variable for the index that chooses from a list of options in an ambiguity
      *  always an integer, thus independent of bound variables
      */
    def newAmbiguity = {
      val name = LocalName("") / "A" / next
      unknowns ::= VarDecl(name)
      OMV(name)
    }

    /** generates a new free variable that is meant to be bound on the outside */
    def newFreeVariable(n: String) = {
      val name = LocalName(n)
      freevars ::= name
      OMV(name)
    }
  }

  /** true if n may be a name introduced inside a term */
  private def mayBeName(n: String) = {
    n != "" && TokenList.isLetter(n(0)) && n.forall(c => TokenList.isLetterOrNumber(c) || TokenList.isConnector(c))
  }

  /** true if left-over token n may be interpreted as a free variable, see [[ParseResult]]
   *  making this always true hides source errors in case of typos
   *  making this never true precludes using free variables
   */
  private def mayBeFree(n: String) = {
     mayBeName(n) && n(0).isUpper
  }
  private type MadeTerm = (List[BoundName],Term)
  private type MadeDecl = (List[BoundName],OML)
  /**
   * recursively transforms a TokenListElem (usually obtained from a [[Scanner]]) to an MMT Term
   * @param te the element to transform
   * @param boundNames the names bound in this term (excluding the variables of the context of the parsing unit)
   * @param pu the original ParsingUnit (constant during recursion)
   * @param attrib the resulting term should be an attributed declaration of a fresh name
   * @return the term and the names it binds that can be used in future terms
   */
  private def makeTerm(te: TokenListElem, boundNames: List[BoundName], inBlock: Boolean, attrib: Boolean = false)
                      (implicit pu: ParsingUnit, variables: Variables, errorCont: ErrorHandler): MadeTerm = {
    // cases may return multiple options in case of ambiguity
    val mt: MadeTerm = te match {
      case te @ Token(word, _, _, _) =>
        lazy val unparsed = OMSemiFormal(objects.Text("unknown", word)) // fallback option
        val name = LocalName.parse(word)
        val isBound = boundNames.reverse.find(_.name == name)
        val term = if (attrib) {
          // we need the name in a declaration
          if (isBound.isDefined) {
            // TODO can a variable shadow another bound name? for now: generate error but parse it as usual
            makeError(s"variable $word shadows a bound name", te.region)
          }
          if (mayBeName(word)) {
            OMV(word) // unbound variable, to be turned into VarDecl by caller
          } else {
            makeError("fresh variable name expected; found: " + word,te.region)
            unparsed
          }
        } else if (isBound.isDefined) {
          // single Tokens may be bound names ...
          if (isBound.get.isOML) OML(name) else OMV(name)
        } else if (variables.getFreeVars.contains(name) || pu.context.exists(_.name == name)) {
          // ... or free variables ...
          OMV(name)
        } else if (word == "_") {
          // unbound _ is a fresh unknown variable
          variables.newUnknown(variables.newExplicitUnknown,boundNames)
        } else if (word.count(c => c == '?' || c == '/') > 0) {
          // ... or qualified identifiers
          makeIdentifier(te).map(OMID).getOrElse(unparsed)
        } else if (mayBeFree(word)) {
          variables.newFreeVariable(word)
        } else {
          //in all other cases, we don't know
          makeError("unbound token: " + word,te.region)
          unparsed
        }
        (Nil,term)
      case e: ExternalToken =>
        val eti = new ExternalTokenParsingInput(pu, this, errorCont) {
          def callbackParse(reg: SourceRegion, s: String) = {
            val boundVars = BoundName.getVarNames(boundNames)
            val cont = pu.context ++ Context(boundVars.map(VarDecl(_,None,None,None,None)): _*)
            val ref = pu.source.copy(region = reg)
            val innerpu = ParsingUnit(ref,cont,s,pu.iiContext)
            parse(innerpu, variables, errorCont)
          }
        }
        (Nil,e.parse(eti))
      case ml: MatchedList =>
        makeTermFromMatchedList(ml, boundNames, inBlock, attrib)
      case ul: UnmatchedList =>
        /* scanning is delayed until here in order to allow for modifying the scanning based on what has been parsed already
           - makeTermFromMatchedList below may have previously added notations to ul
           - we will now check for parsing names
         */
        if (attrib && ul.tl.isSingleWord.isDefined) {
          /* if we know we need a name, by-pass the usual methods and return it
             this slightly hacky case allows handling fresh names or names that cannot be statically resolved
           */
          val t = ul.tl.isSingleWord.get
          (Nil, OMV(LocalName.parse(t.word)))
        } else {
          ul.scan()
          if (ul.tl.length == 1) {
          // process the single TokenListElement
            makeTerm(ul.tl(0), boundNames, inBlock, attrib)
          } else {
            /* This case arises if
            - the Term is ill-formed
            - the matching TextNotation is missing
            - a subterm has no delimiters (e.g., as in LF applications)
            - a semi-formal subterm consists of multiple text Tokens
            Consequently, it is not obvious how to proceed.
            By using defaultApplication, the behavior is somewhat configurable.
            */
            val (bns,terms) = ul.tl.getTokens.map(makeTerm(_,boundNames,inBlock)).unzip
            val t = prag.defaultApplication(Some(pu.getLevel),terms.head,terms.tail)
            (Nil,t)
          }
        }
    }
    SourceRef.update(mt._2, pu.source.copy(region = te.region))
    mt
  }

  /** auxiliary method of makeTerm
   *  parses qualified identifiers, possibly heuristically
   */
  private def makeIdentifier(te: Token)(implicit pu: ParsingUnit, errorCont: ErrorHandler): Option[ContentPath] = {
    var word = te.word
    val segments = utils.stringToList(word, "\\?")
    // recognizing identifiers ?THY?SYM is awkward because it would require always lexing initial ? as identifiers
    // but we cannot always prepend ? because the identifier could also be NS?THY
    // Therefore, we turn word into ?word using a heuristic
    segments match {
      case only :: Nil =>
        val ln = LocalName.parse(word)
        val options = lup.resolveName(pu.context.getIncludes, ln)
        val name = options match {
          case Nil =>
            makeError("ill-formed constant reference " + ln, te.region)
            None
          case hd :: Nil =>
            Some(hd)
          case _ => 
            makeError("ambiguous constant reference " + ln, te.region)
            None
        }
        return name
      case fst :: _ :: Nil if !fst.contains(':') && fst != "" && Character.isUpperCase(fst.charAt(0)) =>
        word = "?" + word
      case _ =>
    }
    // recognizing prefix:REST is awkward because : is often used in notations
    // therefore, we turn prefix/REST into prefix:/REST if prefix is a known namespace prefix
    // this introduces the (less awkward problem) that relative paths may not start with a namespace prefix
    val beforeFirstSlash = segments.headOption.getOrElse(word).takeWhile(_ != '/')
    if (!beforeFirstSlash.contains(':') && pu.nsMap.get(beforeFirstSlash).isDefined) {
      word = beforeFirstSlash + ":" + word.substring(beforeFirstSlash.length)
    }
    try {
      Path.parse(word, pu.nsMap) match {
        case p: ContentPath => Some(p)
        case p =>
          makeError("content path expected: " + p, te.region)
          None
      }
    } catch {
      case ParseError(msg) =>
        // TODO recover by trying to resolve the identifier
        makeError(msg, te.region)
        None
    }
  }


  /** auxiliary method of makeTerm
    * @param boundNames visible variables and OMLs that were declared in the surrounding term
    * @param outerBlock if true, the surrounding term is a block, i.e., new OML declarations must be tracked and returned
    * @param attrib if true, this is to be become an attributed fresh name
    */
  private def makeTermFromMatchedList(ml: MatchedList, boundNames: List[BoundName], outerBlock: Boolean, attrib: Boolean)
                                     (implicit pu: ParsingUnit, variables: Variables, errorCont: ErrorHandler): (List[BoundName],Term) = {
    val notation = ml.an.rules.head.notation // all notations must agree
    val arity = notation.arity
    val innerBlock = notation.block
    val isBlock = outerBlock || innerBlock
    val firstVar = arity.firstVarNumberIfAny
    /* DELETE after testing 2020-07-07
    //log("constructing term for notation: " + ml.an)
    val found = ml.an.getFound
    // compute the names of all bound variables, in abstract syntax order
    val newBVars: List[(Var, LocalName)] = found.flatMap {
      case fv: FoundVar => fv.getVars map {
        case SingleFoundVar(_, name, _) =>
          val ln = LocalName.parse(name.word)
          (fv.marker, ln)
      }
      case _ => Nil
    }.sortBy(_._1.number)
    val newBVarNames = newBVars.map(x => BoundName(x._2, false))

    /** the bound variables occurring in a variable: the prefix of newBVarNames just before a certain variable identified by its Var marker and
        within that FoundVar's sequence of variables by its name */
    def boundNamesInVar(vm: Var, name: LocalName): List[BoundName] = {
      val pos = newBVars.indexWhere(_ ==(vm, name))
      boundNames ::: newBVarNames.take(pos)
    }
    /** the bound variables in argument, distinguishing arguments before and after the variables */
    def boundNamesInArg(n: Int): List[BoundName] = if (n < firstVar) boundNames else boundNames ::: newBVarNames
    */

    // 3 lists for the children, in concrete syntax order, together with their position in the abstract syntax
    // in abstract syntax order
    // the arguments before the variables
    var subs: List[(Int, Term)] = Nil
    // the variable declaration list
    var vars: List[(Var, OML)] = Nil
    // the arguments behind the variables
    var args: List[(Int, Term)] = Nil
    // the bound names that the arguments processed so far have introduced, to be used in later (in abstract syntax order) arguments
    // contains all bound variables, and if isBlock also all OMLs
    var newBoundNamesSoFar: List[BoundName] = Nil
    def boundNamesSoFar = boundNames ::: newBoundNamesSoFar

    // We walk through found and fill subs, vars, and args by
    // recursively processing the respective TokenListElem in ml.tokens
    /** adds terms to either subs or args, depending on n */
    def addTerm(n: Int, mt:MadeTerm): Unit = {
      val (bns,tm) = mt
      val ntm = (n,tm)
      if (n < firstVar)
        subs = subs ::: List(ntm)
      else
        args = args ::: List(ntm)
      if (isBlock)
         newBoundNamesSoFar = newBoundNamesSoFar ::: bns
    }
    def addVar(v: Var, md: MadeDecl): Unit = {
      val oml = md._2
      val name = oml.name
      val newBound = List(BoundName(name, false))
      vars = vars ::: List((v,oml))
      // bound variables are always available in later arguments; the declaration is its own block, so any names introduced in it are dropped
      newBoundNamesSoFar = newBoundNamesSoFar ::: newBound
    }
    // will be called below (in abstract syntax order) on every FoundContent
    def doFoundContent(fc: FoundContent, toks: List[UnmatchedList]): Unit = {fc match {
      // argument before the variables
      case FoundSimp(_, m: SimpArg) =>
        if (attrib)
          true
        val attribArg = attrib && m.number == 1 // for nesting attributions: the first argument is to be attributed
        val r = makeTerm(toks.head,boundNamesSoFar,isBlock,attribArg)
        addTerm(m.number,r)
      // label arguments: as the cases above but with makeOML instead of makeTerm
      case FoundSimp(_, m: LabelArg) =>
        val r = makeOML(toks.head,boundNamesSoFar,m.info)
        addTerm(m.number, r)
      // variable binding: similar to OML
      case FoundSimp(_, m: Var) =>
        val r = makeOML(toks.head,boundNamesSoFar,m.info)
        addVar(m,r)
      // sequence arguments: as the cases above, but as many TokenListElement as the sequence has elements
      case FoundSeq(m: SimpSeqArg, _) =>
        toks.foreach {t =>
          val r = makeTerm(t,boundNamesSoFar,isBlock)
          addTerm(m.number,r)
        }
      // label sequence arguments: as above
      case FoundSeq(m: LabelSeqArg, _) =>
        // names encountered in the sequence are available for later arguments
        toks.foreach {tok =>
          val r = makeOML(tok,boundNamesSoFar,m.info)
          addTerm(m.number, r)
        }
      // sequence variables: as above
      case FoundSeq(m: Var, _) =>
        toks.foreach {tok =>
          val r = makeOML(tok,boundNamesSoFar,m.info)
          addVar(m,r)
        }
      case _ => throw ImplementationError("unexpected found object")
      /* DELETE after testing 2020-07-07
      case fv: FoundVar =>
        var toksLeft = toks
        fv.getVars foreach {case SingleFoundVar(pos, nameToken, tpOpt) =>
          val name = LocalName(nameToken.word)
          // a variable declaration, so take one TokenListElement for the type
          val tp = tpOpt map {_ =>
            val (_,t) = makeTerm(toksLeft.head, boundNamesSoFar, isBlock)
            toksLeft = toksLeft.tail
            t
          }
          val newBound = List(BoundName(name, false))
          vars = vars ::: List((fv.marker, nameToken.region, name, tp))
          // bound variables are always available in later arguments; the declaration is its own block, so  any names
           introduced in it are dropped
          newBoundNamesSoFar = newBoundNamesSoFar ::: newBound
        }
      // now toksLeft.empty
    */
    }}
    // we process all found content in abstract syntax order, inserting implicit arguments where needed
    var processed: List[FoundContent] = Nil
    arity.components.foreach {
      case ia: ImplicitArg =>
        val t = variables.newUnknown(variables.newArgument, boundNamesSoFar)
        addTerm(ia.number, (Nil,t))
      case arg =>
        val tokens = ml.tokens.filter(_._1.number == arg.number)
        // TODO optional tokens could be handled here
        tokens.foreach {case (fc,uls) =>
          // this is essentially just a call to doFoundContent
          // but first we handle local notations
          arg match {
            case a: ArgumentMarker =>
              // local notations
              a.properties.localNotations.foreach {case lni =>
                // find the previous argument that lni references, obtain additional notations from it, add them to uls
                (subs ::: args).find(_._1 == lni.argument) foreach {case (_,e) =>
                  val tO = lni.role match {
                    case LocalNotationInfo.Theory =>
                      Some(e)
                    case LocalNotationInfo.Domain =>
                      Morph.domain(e)(lup)
                    case LocalNotationInfo.Codomain =>
                      Morph.codomain(e)(lup)
                  }
                  //calling this on a non-type-checked t may or may not find all relevant notations
                  val localNotations = tO match {
                    case Some(t) =>
                      tableNotations(getRules(t)._1)
                    case None =>
                      makeError("cannot determine theory for local notations",ml.region)
                      ParsingRuleTable(Nil)
                  }
                  uls.foreach {ul =>
                    ul.addRules(localNotations,lni.replace)
                  }
                }
              }
            case _ =>
          }
          // now the main call
          processed ::= fc
          doFoundContent(fc,uls)
        }
    }
    // sanity check: have we processed every token
    if (processed.length != ml.tokens.length) {
      throw ImplementationError("unprocessed tokens")
    }

    /* DELETE after testing 2020-07-07

     // all tokens of ml enriched with the respective local notation info
     val tokensWithLocalNotationInfo: List[(FoundContent, Option[LocalNotationInfo], List[UnmatchedList])] = ml.tokens map {case (fc, uls) =>
        val arg = arity.components.find(_.number == fc.number)
        val localNotInfo = arg flatMap {
          case a: ArgumentMarker => a.properties.localNotations
          case _ => None
        }
        (fc, localNotInfo, uls)
     }
     // first process all tokens that do not have local notations
     tokensWithLocalNotationInfo.foreach {case (fc,lni,uls) =>
       lni match {
         case None =>
           doFoundContent(fc, uls)
         case _ =>
       }
     }
     // now process the other tokens, using the previous results to resolve the local notations
     tokensWithLocalNotationInfo.foreach {case (fc,lni,uls) =>
       lni match {
         case None =>
         case Some(lni) =>
           (subs:::args).find(_._1 == lni.argument) foreach {case (_,e) =>
             val tO = lni.role match {
               case LocalNotationInfo.Theory =>
                 Some(e)
               case LocalNotationInfo.Domain =>
                 Morph.domain(e)(lup)
               case LocalNotationInfo.Codomain =>
                 Morph.codomain(e)(lup)
             }
             //calling this on a non-type-checked t may or may not find all relevant notations
             val localNotations = tO match {
               case Some(t) =>
                 tableNotations(getRules(t)._1)
               case None =>
                 makeError("cannot determine theory for local notations", ml.region)
                 ParsingRuleTable(Nil)
             }
             uls.foreach {ul =>
               ul.addRules(localNotations, lni.replace)
             }
           }
           doFoundContent(fc, uls)
       }
     }
     */

     // the list of constants of the used notation
     // basically, cons = mlCons, but we drop every constant that is defined to be equal to one we already have
     // such cases can happen with structures, where the generated constants are essentially aliases that do not require ambiguity resolution 
     var consVar: List[ContentPath] = Nil
     val mlCons = ml.an.rules.map(_.name)
     mlCons foreach {
       case p: GlobalName =>
         val pA = lup.quasiAliasFor(p)
         if (utils.disjoint(mlCons, pA))
           consVar ::= p
       case p =>
         consVar ::= p
     }
     val cons = consVar.reverse

     // hard-coded special case for a bracketed subterm
     if (cons == List(utils.mmt.brackets) || cons == List(utils.mmt.andrewsDot) || cons == List(utils.mmt.andrewsDotRight)) {
       //TODO add metadata for keeping track of brackets
       // source ref of the returned term will be overridden with the source ref of the bracketed term
       return (boundNamesSoFar, args.head._2)
     }

    val finalSub = Substitution(subs.map(a => Sub(OMV.anonymous, a._2)): _*)
    val finalVars = vars.map {case (vm, oml) => oml.vd}
    val finalArgs = args.map(_._2)

    /* delete after testing 2020-09-01
    // process subs, vars, and args, which are needed to build the term
     // this includes sorting args and vars according to the abstract syntax
     // add implicit arguments before the variables
     val finalSubs: List[Term] = arity.subargs.flatMap {
        case ImplicitArg(_, _) =>
          List(variables.newUnknown(variables.newArgument, boundNamesSoFar))
        case LabelArg(n,_,_) =>
          val a = subs.find(_._1 == n).get
          List(a._2)
        case SimpArg(n, _) =>
          val a = subs.find(_._1 == n).get // must exist if notation matched
          List(a._2)
        case LabelSeqArg(n,_,_,_) =>
          val as = subs.filter(_._1 == n)
          as.map(_._2)
        case SimpSeqArg(n, _, _) =>
          val as = subs.filter(_._1 == n)
          as.map(_._2)
     }
     val finalSub = Substitution(finalSubs.map(a => Sub(OMV.anonymous, a)): _*)

    // compute the variables
    val finalVars = vars.map {
      case (vm, oml) => oml.vd
    }

     // add implicit arguments behind the variables (same as above except for using newBVarNames)
     val finalArgs: List[Term] = arity.arguments.flatMap {
        case ImplicitArg(_, _) =>
          List(variables.newUnknown(variables.newArgument, boundNamesSoFar))
        case LabelArg(n, _,_) =>
          val a = args.find(_._1 == n).get // must exist if notation matched
          List(a._2)
        case SimpArg(n, _) =>
          val a = args.find(_._1 == n).get // must exist if notation matched
          List(a._2)
        case LabelSeqArg(n, _, _,_) =>
          val as = args.filter(_._1 == n)
          as.map(_._2)
        case SimpSeqArg(n, _, _) =>
          val as = args.filter(_._1 == n)
          as.map(_._2)
     }
     */

     /* construct each possible alternative term
        all alternatives must use the same notation; therefore, they will ask for the same unknown variables
        we cache those in UnknownCacher to make sure we only generate one set of unknown variables
      */
     /**
      * used to
      * on the first run, delegates to NotationBasedParser.newUnknown and caches the results
      *
      * on subsequent runs, uses the cached results
      */
     object UnknownCacher {
        /** true iff this is the first run */
        private var firstRun = true
        /** the list of unknowns generated during the first run */
        private var cachedUnknowns : List[Term] = Nil
        /** the remaining list of unknowns to be used during the current subsequent run */
        private var nextUnknowns: List[Term] = Nil
        /** initializes before every subsequent run */
        def prepareNextRun: Unit = {
           firstRun = false
           nextUnknowns = cachedUnknowns
        }
        /** gets the next unknown variable */
        def getNext : Term = {
           if (firstRun) {
              val u = variables.newUnknown(variables.newArgument, boundNames) // these unknown occur at the beginning of the current term, so boundNames instead of boundNamesSoFar
              cachedUnknowns :::= List(u)
              u
           } else {
              val u = nextUnknowns.head
              nextUnknowns = nextUnknowns.tail
              u
           }
        }
     }
     /** constructs one alternative term */
     def makeAlternative(con: ContentPath, adaptedFinalArgs: List[Term]): Term = {
        if (arity.isConstant && subs.isEmpty && args.isEmpty && vars.isEmpty) {
          //no args, vars, scopes --> OMID
          return OMID(con)
        }
        // the level of the notation: if not provided, default to the meta-theory of the constant
        val level = notation.meta orElse {
           lup.getO(con.module) match {
             case Some(t: Theory) => t.meta
             case _ => None
           }
        }
        con match {
           case con: MPath =>
             if (finalSub.nonEmpty)
               makeError("no substitution allowed in module application", ml.region)
             if (finalVars.isEmpty)
               OMPMOD(con, adaptedFinalArgs)
             else
               OMBINDC(OMMOD(con), finalVars, adaptedFinalArgs)
           case con: GlobalName =>
             prag.makeStrict(level, con, finalSub, finalVars, adaptedFinalArgs, notation)(() => UnknownCacher.getNext)
        }
     }
     // construct the alternative terms
     val altTms = if (cons.length > 1 && finalSub.isEmpty && finalVars.isEmpty && finalArgs.nonEmpty) {
        // to avoid duplicating the arguments in each alternative (which would in particular cause them to be checked multiple times),
        // we bind argument a_i as variable /AP/i=a_i outside of the ambiguous term 
        val (argCont,argNames) = finalArgs.zipWithIndex.map {case (a,i) =>
          val n = LocalName("") / "AP" / i.toString
          (VarDecl(n, df = a), OMV(n))
        }.unzip
        val av = variables.newAmbiguity
        val alternatives = cons map {con =>
           val a = makeAlternative(con, argNames)
           UnknownCacher.prepareNextRun
           a
        }
        OMBINDC(OMS(ObjectParser.oneOf), argCont, av::alternatives)
     } else {
       val alternatives = cons map {con =>
          val a = makeAlternative(con, finalArgs)
          UnknownCacher.prepareNextRun
          a
       }
       alternatives match {
          case hd :: Nil =>
             hd
          case l =>
             val av = variables.newAmbiguity
             ObjectParser.oneOf(av::l)
       }
     }
    // OMLs are exported to the surrounding block (if any) unless this terms introduces its own block; variables are always invisible from the outside
    val newNamesForSurroundingBlock = if (outerBlock && !innerBlock) newBoundNamesSoFar.filter(_.isOML) else Nil
    (newNamesForSurroundingBlock, altTms)
  }

  /** like makeTerm but interprets OMA(:,OMA(=,v)) as an OML */
  private def makeOML(te: TokenListElem, boundNames: List[BoundName], info: LabelInfo)
                      (implicit pu: ParsingUnit, variables: Variables, errorCont: ErrorHandler): MadeDecl = {
    val mt = makeTerm(te,boundNames, false, attrib = true) // OML is its own block, cannot export names other than itself
    mt match {
      case (_, OMLTypeDefNot(name, tpOpt, dfOpt, ntOpt) ) =>
        var unknown = false // true if no unknown generated
        val tp = tpOpt orElse {
          if (info.typed) {
            unknown = true
            Some(variables.newUnknown(variables.newType(name), boundNames))
          } else
            None
        }
        val df = dfOpt orElse {
          if (info.defined) {
            unknown = true
            Some(variables.newUnknown(variables.newExplicitUnknown, boundNames))
          } else
            None
        } // ever being able to infer a definiens is unlikely
        val l = OML(name,tp,df,ntOpt).from(mt._2)
        if (unknown) {
          metadata.TagInferredType.set(l)
        }
        /* an OML is its own block - type and definiens do not export names into the environment
           so any names introduced inside are dropped, and the single name of the OML is exported
        */
        val newBound = List(BoundName(name, true))
        (newBound, l)
      case _ =>
        makeError("expected declaration, found other term: " + mt._2, te.region)
        (mt._1, OML(OMV.anonymous, None, Some(mt._2)))
    }
  }

  /** matches v[:T][=D][#N] */
  private object OMLTypeDefNot {
    /* DELETE after testing 2020-07-07
    private object Name {
      def unapply(t : Term) : Option[LocalName] = t match {
        case OMV(n) => Some(n)
        case OMS(p) => Some(p.name)
        case OMA(OMS(ObjectParser.oneOf),ls)=>
          val rs = ls collect {case OMS(p) => p.name}
          rs.headOption match {
            case Some(name) if rs.forall(_ == name) => Some(name)
            case _ => None
          }
        case l : OML => Some(l.name)
        case OMSemiFormal(List(Text(_,s))) =>
          Some(LocalName.parse(s))
        case _ =>
          None
      }
    }*/
    def unapply(t : Term) : Option[(LocalName,Option[Term],Option[Term],Option[TextNotation])] = {
      var tp: Option[Term] = None
      var df: Option[Term] = None
      var nt: Option[TextNotation] = None
      // returns the name of this OML and stores the found attributions (any order, no duplicates) in the above variables
      def matchTDN(left: Term): Option[LocalName] = left match {
        case OMLtype(k, t) =>
          if (tp.isDefined) {
            None
          } else {
            tp = Some(t)
            matchTDN(k)
          }
        case OMLdef(k, t) =>
          if (df.isDefined) {
            None
          } else {
            df = Some(t)
            matchTDN(k)
          }
        case OMLnotation(k, NotationRealizedType(not)) =>
          if (nt.isDefined) {
            None
          } else {
            nt = Some(not)
            matchTDN(k)
          }
        case OMV(n) => Some(n)
        case _ => None
      }
      matchTDN(t).map {n => (n, tp, df, nt)}
    }
  }
  /** matches v c T where c is a constant with role rl */
  private case class OMLAnnotation(rl: String) {
    def unapply(t : Term) : Option[(Term,Term)] = controller.pragmatic.mostPragmatic(t) match {
      case OMA(OMS(f),List(a,b)) =>
        controller.get(f) match {
          case c:Constant if c.rl contains rl => Some((a,b))
          case _ => None
        }
      case _ => None
    }
  }
  private object OMLtype extends OMLAnnotation("Type")
  private object OMLdef extends OMLAnnotation("Def")
  private object OMLnotation extends OMLAnnotation("Notation")
}