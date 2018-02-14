package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

/** couples an identifier with its notation */
case class ParsingRule(name: ContentPath, alias: List[LocalName], notation: TextNotation) {
  /** the first delimiter of this notation, which triggers the rule */
  def firstDelimString: Option[String] = notation.parsingMarkers collectFirst {
    case d: Delimiter => d.expand(name, alias).text
    case SimpSeqArg(_, Delim(s), _) => s
    case LabelSeqArg(_,Delim(s),_,_) => s
  }
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
 * names are locally meaningful: variables bound in the contexts (to be parsed as OMV), fields of dependnet OML sequences (to be parsed as OML)
 */
case class BoundName(name: LocalName, isOML: Boolean)

object BoundName {
  /** the names of bound variables */
  def getVars(names: List[BoundName]): List[LocalName] = {
    names collect {
      case BoundName(n,false) => n
    }
  }
}

/**
 * A notation based parser
 */
class NotationBasedParser extends ObjectParser {
  override val logPrefix = "object-parser"

  def isApplicable(format: String): Boolean = format == "mmt"

  private lazy val prag = controller.pragmatic

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

  /** constructs a SourceError, all errors go through this method */
  private def makeError(msg: String, reg: SourceRegion)(implicit pu: ParsingUnit, errorCont: ErrorHandler) {
    val ref = SourceRef(pu.source.container, reg)
    val err = SourceError(logPrefix, ref, msg)
    errorCont(err)
  }

  /*
   * declarations of the unknown variables (implicit arguments, missing types, etc.
   *
   * the variable names are irrelevant as long as they are unique within each call to the parser
   * Moreover, we make sure the names are chosen the same way every time to support change management.
   */
  private object Variables {
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

     def reset() {
       unknowns = Nil
       counter = 0
       freevars = Nil
     }

     /** name of an omitted implicit argument */
     def newArgument =
       LocalName("") / "I" / next

     /** name of the omitted type of a variable */
     def newType(name: LocalName) =
       LocalName("") / name / next

     /** name of an explicitly omitted argument */
     def newExplicitUnknown =
       LocalName("") / "_" / next

     /** generates a new unknown variable, constructed by applying a fresh name to all bound variables */
     def newUnknown(name: LocalName, boundNames: List[BoundName])(implicit pu: ParsingUnit) = {
       unknowns ::= VarDecl(name)
       val bvars = BoundName.getVars(boundNames)
       if (bvars.isEmpty)
         OMV(name)
       else {
         //TODO in case of shadowing (duplicates in bvars), one variable must be renamed
         //apply meta-variable to all bound variables in whose scope it occurs
         prag.defaultApplication(Some(pu.context.getIncludes.last), OMV(name), bvars.map(OMV(_)))
       }
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

     /** removes a free variable if it is later found out that it can be interpreted otherwise, used for OMLs */
     def removeFreeVariable(n: LocalName) {
       freevars = freevars diff List(n)
     }
  }
  import Variables._


  /**
   * @param pu the parsing unit
   */
  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): ParseResult = {
    if (pu.isKilled) {
      return DefaultObjectParser(pu)
    }
    implicit val puI = pu
    //gathering notations and lexer extensions in scope
    val (parsing, lexing, _) = getRules(pu.context)
    val notations = tableNotations(parsing)
    Variables.reset()
    log("parsing: " + pu.term)
    log("rules:")
    logGroup {
      log("parsing")
      notations.groups.foreach(n => log(n.toString))
      log("lexing")
      lexing.foreach(r => log(r.toString + " (priority " + r.priority + ")"))
    }
    val escMan = new EscapeManager(lexing)
    val tl = TokenList(pu.term, escMan, pu.source.region.start)
    val result = if (tl.getTokens.isEmpty) {
      makeError("no tokens found: " + pu.term, pu.source.region)
      DefaultObjectParser(pu)
    } else {
        //scanning
        val ul = new UnmatchedList(tl)
        // TODO does it make sense to sort by meta-theory-level first, then by precedence?
        // the scanner initially scans with top rule, then with all notations in increasing order of precedence
        // but we will actually call ul.scanner.scan only in makeTerm so that we can add local notations first
        ul.scanner = new Scanner(tl, Some(pu), notations, controller.report)
        // turn the syntax tree into a term
        val tm = logGroup {
          makeTerm(ul, Nil)
        }
        log("parse result: " + tm.toString)
        val (unk, free) = getVariables
        ParseResult(unk, free, tm)
    }
    result
  }

  private type RuleLists = (List[ParsingRule], List[LexerExtension], List[NotationExtension])

  /** auxiliary function to collect all lexing and parsing rules in a given context */
  private def getRules(context: Context): RuleLists = {
    val support = context.getIncludes
    //TODO we might also collect notations attached to variables
    support.foreach {p =>
      controller.simplifier(p)
    }
    val decls = support.flatMap {p => controller.globalLookup.getDeclarationsInScope(OMMOD(p))}.distinct
    var nots: List[ParsingRule] = Nil
    var les: List[LexerExtension] = Nil
    var notExts: List[NotationExtension] = Nil
    decls.foreach {
      case c: Constant => // Declaration with HasNotation might collect too much here
        var names = (c.name :: c.alternativeNames).map(_.toString) //the names that can refer to this declaration
        if (c.name.last == SimpleStep("_")) names ::= c.name.init.toString
        //the unapplied notations consisting just of the name
        val unapp = names map (n => new TextNotation(Mixfix(List(Delim(n))), Precedence.infinite, None))
        val app = c.not.toList
        (app ::: unapp).foreach {n =>
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
      case nm: NestedModule =>
        val args = nm.module match {
          case t: Theory => t.parameters.length
          case v: View => 0
        }
        val ms = Mixfix(Delim(nm.name.toString) :: Range(0, args).toList.map(SimpArg(_)))
        val tn = new TextNotation(ms, Precedence.infinite, None)
        nots ::= ParsingRule(nm.module.path, Nil, tn)
      case _ =>
    }
    les = les.sortBy(- _.priority)
    (nots, les, notExts)
  }

  /* like getRules but for a theory expression (currently only called for local notations) */
  private def getRules(thy: Term): RuleLists = {
    // this used to simplify thy first, but that may be dangerous and/or inefficient
    thy match {
      case OMPMOD(mp,_) => getRules(Context(mp))
      case AnonymousTheory(metaO, decls) =>
        //we could also collect all notations in decls, but we do not have parsing rules for OML's
        metaO.map(m => getRules(Context(m))).getOrElse((Nil,Nil,Nil))
      case _ => (Nil,Nil,Nil) // TODO only named theories are implemented so far
    }
  }

  /** true if n may be the name of a free variable, see [[ParseResult]]
   *
   *  making this always true hides source errors
   *  making this never true requires awkwardly binding all variables
   */
  private def mayBeFree(n: String) = {
     n != "" && n(0).isLetter &&
     n.forall(c => c.isLetter || c.isDigit)
  }

 /**
   * recursively transforms a TokenListElem (usually obtained from a [[Scanner]]) to an MMT Term
   * @param te the element to transform
   * @param boundVars the variable names bound in this term (excluding the variables of the context of the parsing unit)
   * @param pu the original ParsingUnit (constant during recursion)
   * @param attrib the resulting term should be a variable attribution
   */
  private def makeTerm(te: TokenListElem, boundNames: List[BoundName], attrib: Boolean = false)(implicit pu: ParsingUnit, errorCont: ErrorHandler): Term = {
    // cases may return multiple options in case of ambiguity
    val term = te match {
      case te @ Token(word, _, _, _) =>
        lazy val unparsed = OMSemiFormal(objects.Text("unknown", word)) // fallback option
        val name = LocalName.parse(word)
        val isBound = boundNames.find(_.name == name)
        val term = if (isBound.isDefined) {
          //single Tokens may be bound names ...
          if (isBound.get.isOML) OML(name) else OMV(name)
        } else if (getFreeVars.contains(name) || pu.context.exists(_.name == name)) {
          // ... or free variables ...
          OMV(name)
        } else if (word == "_") {
          // unbound _ is a fresh unknown variable
          newUnknown(newExplicitUnknown, boundNames)
        } else if (word.count(_ == '?') > 0) {
          // ... or qualified identifiers
          makeIdentifier(te).map(OMID).getOrElse(unparsed)
        } else if (mayBeFree(word)) {
           newFreeVariable(word)
        } else {
          //in all other cases, we don't know
          makeError("unbound token: " + word, te.region)
          unparsed
        }
        term
      case e: ExternalToken =>
        val eti = new ExternalTokenParsingInput(pu, boundNames, this, errorCont)
        e.parse(eti)
      case ml: MatchedList =>
        makeTermFromMatchedList(ml, boundNames, attrib)
      case ul: UnmatchedList =>
        // scanning is delayed until here in order to allow for collecting local notations first
        ul.scanner.scan()
        val term = if (ul.tl.length == 1) {
        // process the single TokenListElement
          makeTerm(ul.tl(0), boundNames)
        } else {
          /* This case arises if
          - the Term is ill-formed
          - the matching TextNotation is missing
          - a subterm has no delimiters (e.g., as in LF applications)
          - a semi-formal subterm consists of multiple text Tokens
          Consequently, it is not obvious how to proceed.
          By using defaultApplication, the behavior is somewhat configurable.
          */
          val terms = ul.tl.getTokens.map(makeTerm(_, boundNames))
          prag.defaultApplication(pu.context.getIncludes.lastOption, terms.head, terms.tail)
        }
        term
    }
    SourceRef.update(term, pu.source.copy(region = te.region))
    term
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
      case fst :: _ :: Nil if !fst.contains(':') && fst != "" && Character.isUpperCase(fst.charAt(0)) =>
        word = "?" + word
      case _ =>
    }
    // recognizing prefix:REST is awkward because : is usually used in notations
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


  /** auxiliary method of makeTerm */
  private def makeTermFromMatchedList(ml: MatchedList, boundNames: List[BoundName], attrib: Boolean)(implicit pu: ParsingUnit, errorCont: ErrorHandler): Term = {
     val notation = ml.an.rules.head.notation // all notations must agree
     val arity = notation.arity
     val firstVar = arity.firstVarNumberIfAny
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
     /** the bound variable in argument, distinguishing arguments before and after the variables */
     def boundNamesInArg(n: Int): List[BoundName] = if (n < firstVar) boundNames else boundNames ::: newBVarNames
     // 3 lists for the children, in concrete syntax order, together with their position in the abstract syntax
     // order does not matter except that sequence elements (which have the same position) must occur in sequence order
     // the arguments before the variables
     var subs: List[(Int, Term)] = Nil
     // the variable declaration list (name + type)
     var vars: List[(Var, SourceRegion, LocalName, Option[Term])] = Nil
     // the arguments behind the variables
     var args: List[(Int, Term)] = Nil
     // We walk through found and fill subs, vars, and args by
     // recursively processing the respective TokenListElem in ml.tokens
     /** adds terms to either subs or args, depending on n, and increments i for each term */
     def addTerms(n: Int, terms: List[Term]) {
        val nterms = terms.map((n,_))
        if (n < firstVar)
          subs = subs ::: nterms
        else
          args = args ::: nterms
     }
     def doFoundContent(fc: FoundContent, toks: List[UnmatchedList]) {fc match {
       // argument before the variables
       case FoundSimpArg(_, n) =>
         val r = makeTerm(toks.head, boundNamesInArg(n))
         addTerms(n, List(r))
       // label arguments: as the cases above but with makeOML instead of makeTerm
       case FoundOML(_,n,info) =>
         val r = makeOML(toks.head, boundNamesInArg(n),info)
         addTerms(n, List(r))
       // sequence arguments: as the case above, but as many TokenListElement as the sequence has elements
       case FoundSimpSeqArg(n, fas) =>
         val r = toks.map(t => makeTerm(t, boundNamesInArg(n)))
         addTerms(n, r)
       // label sequence arguments: as above
       case FoundSeqOML(n,fas,info) =>
         // the names encountered in the sequence so far
         // if the sequence is dependent, these names are bound in the remainder
         var omlNames: List[BoundName] = Nil
         val r = toks.map {tok =>
           val usableOMLNames = if (info.dependent) omlNames else Nil
           val term = makeOML(tok,boundNamesInArg(n):::usableOMLNames,info)
           if (info.dependent) {
             term match {
               case o: OML => omlNames ::= BoundName(o.name, true)
               case _ =>
             }
           }
           term
         }
         addTerms(n, r)
       // variables
       case fv: FoundVar =>
         var toksLeft = toks
         fv.getVars foreach {case SingleFoundVar(pos, nameToken, tpOpt) =>
           val name = LocalName(nameToken.word)
           // a variable declaration, so take one TokenListElement for the type
           val tp = tpOpt map {_ =>
             val stp = makeTerm(toksLeft.head, boundNamesInVar(fv.marker, name), attrib = true)
             toksLeft = toksLeft.tail
             // remove toplevel operator, e.g., :
             stp match {
               case prag.StrictTyping(stpt) => stpt
               case _ => stp // applies to unregistered type attributions
             }
           }
           vars = vars ::: List((fv.marker, nameToken.region, name, tp))
         }
         // now toksLeft.empty
     }}
     val tokensWithLocalNotationInfo: List[(FoundContent, Option[SimpArg], List[UnmatchedList])] = ml.tokens map {case (fc, uls) =>
        val arg = arity.components.find(_.number == fc.number)
        // the FoundArg of the ActiveNotation that corresponds to arg.locallyUsesNotationsFrom
        val localNotInfo = arg flatMap {
          case a: ArgumentMarker => a.locallyUsesNotationsFrom
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
         case Some(sa) =>
           (subs:::args).find(_._1 == sa.number) foreach {case (_,t) =>
             //calling this on a non-type-checked t may or may not find all relevant notations
             val localNotations = tableNotations(getRules(t)._1)
             uls.foreach {ul =>
               ul.addRules(localNotations)
             }
           }
           doFoundContent(fc, uls)
       }
     }

     val cons = ml.an.rules.map(_.name)

     // hard-coded special case for a bracketed subterm
     if (cons == List(utils.mmt.brackets) || cons == List(utils.mmt.andrewsDot)) {
       //TODO add metadata for keeping track of brackets
       // source ref of the returned term will be overridden with the source ref of the bracketed term
       return args.head._2
     }

     // process subs, vars, and args, which are needed to build the term
     // this includes sorting args and vars according to the abstract syntax
     // add implicit arguments before the variables
     val finalSubs: List[Term] = arity.subargs.flatMap {
        case ImplicitArg(_, _) =>
          List(newUnknown(newArgument, boundNames))
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

     // add implicit arguments behind the variables (same as above except for using newBVarNames)
     val finalArgs: List[Term] = arity.arguments.flatMap {
        case ImplicitArg(_, _) =>
          List(newUnknown(newArgument, boundNames ::: newBVarNames))
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

     // compute the variables
     val finalVars = vars.sortBy(_._1.number).map {
        case (vm, reg, vname, tp) =>
          val (finalTp, unknown) = if (!vm.typed)
            (None, false)
          else tp match {
            case Some(_) => (tp, false)
            case None =>
              //new unknown for the type
              //under a binder, apply the meta-variable to all governing bound variables
              //these are the boundVars and all preceding vars of the current binder
              val governingBVars = boundNamesInVar(vm, vname)
              val t = newUnknown(newType(vname), governingBVars)
              (Some(t), true)
          }
          val vd = VarDecl(vname).copy(tp = finalTp)
          if (unknown)
            metadata.TagInferredType.set(vd)
          SourceRef.update(vd, pu.source.copy(region = reg))
          vd
     }

     /* construct each possible alternative terms
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
        def prepareNextRun {
           firstRun = false
           nextUnknowns = cachedUnknowns
        }
        /** gets the next unknown variable */
        def getNext : Term = {
           if (firstRun) {
              val u = newUnknown(newArgument, boundNames)
              cachedUnknowns :::= List(u)
              u
           } else {
              val u = nextUnknowns.head
              nextUnknowns = nextUnknowns.tail
              u
           }
        }
     }
     /** constructs one alternative terms */
     def makeAlternative(con: ContentPath, adaptedFinalArgs: List[Term]): Term = {
        if (arity.isConstant && subs.isEmpty && args.isEmpty && vars.isEmpty && !attrib) {
          //no args, vars, scopes --> OMID
          return OMID(con)
        }
        // the level of the notation: if not provided, default to the meta-theory of the constant
        val level = notation.meta orElse {
           controller.globalLookup.getO(con.module) match {
             case Some(t: modules.DeclaredTheory) => t.meta
             case _ => None
           }
        }
        con match {
           case con: MPath =>
             if (finalSubs.nonEmpty)
               makeError("no substitution allowed in module application", ml.region)
             if (finalVars.isEmpty)
               OMPMOD(con, adaptedFinalArgs)
             else
               OMBINDC(OMMOD(con), finalVars, adaptedFinalArgs)
           case con: GlobalName =>
             prag.makeStrict(level, con, finalSub, finalVars, adaptedFinalArgs, attrib, notation)(
                () => UnknownCacher.getNext
             )
        }
     }
     // construct the alternative terms
     if (cons.length > 1 && finalSub.isEmpty && finalVars.isEmpty && finalArgs.nonEmpty) {
        val (argCont,argNames) = finalArgs.zipWithIndex.map {case (a,i) =>
          val n = LocalName("") / "AP" / i.toString
          (VarDecl(n, df = a), OMV(n))
        }.unzip
        val av = newAmbiguity
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
             val av = newAmbiguity
             ObjectParser.oneOf(av::l)
       }
     }
  }

  /** like makeTerm but interprets OMA(:,OMA(=,v)) as an OML */
  private def makeOML(te: TokenListElem, boundNames: List[BoundName], info: LabelInfo, attrib: Boolean = false)
                      (implicit pu: ParsingUnit, errorCont: ErrorHandler): Term = {
    // TODO evil hack to allow OML names with slashes, must be removed at next opportunity
    val filter : Error => Boolean = {
      case SourceError(_,_,msg,_,_) if msg startsWith "unbound token:" => false
      case _ => true
    }
    val t = makeTerm(te,boundNames)(pu,new FilteringErrorHandler(errorCont,filter))
    t match {
      case OMLTypeDefNot(name, tpOpt, dfOpt, ntOpt) /* if !boundVars.contains(name) && getFreeVars.contains(name) */ =>
         removeFreeVariable(name)
         val tp = tpOpt orElse {if (info.typed) Some(newUnknown(newExplicitUnknown, boundNames)) else None}
         val df = dfOpt orElse {if (info.defined) Some(newUnknown(newExplicitUnknown, boundNames)) else None}
         val l = OML(name,tp,df,ntOpt)
         l
      case _ =>
        makeError("expected label, found other term: " + t, te.region)
        t
    }
  }

  /** matches v[:T][=D][#N] */
  private object OMLTypeDefNot {
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
    }
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
        case Name(n) => Some(n)
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
  private object OMLtype extends OMLAnnotation("OMLType")
  private object OMLdef extends OMLAnnotation("OMLDef")
  private object OMLnotation extends OMLAnnotation("OMLNotation")
}
