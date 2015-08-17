package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

/** couples an identifier with its notation */
case class ParsingRule(name: ContentPath, notation: TextNotation) {
  /** the first delimiter of this notation, which triggers the rule */
  def firstDelimString: Option[String] = notation.parsingMarkers collectFirst {
    case d: Delimiter => d.expand(name).text
    case SeqArg(_, Delim(s), _) => s
  }
}

/**
 * A notation based parser
 */
class NotationBasedParser extends ObjectParser {
  override val logPrefix = "parser"

  def isApplicable(format: String): Boolean = format == "mmt"

  private lazy val prag = controller.pragmatic

  /**
   * builds parser's notation context based on content in the controller
   * and the scope (base path) of the parsing unit
   * returned list is sorted (in increasing order) by priority
   */
  protected def tableNotations(nots: List[ParsingRule]): List[(Precedence, List[ParsingRule])] = {
    val qnotations = nots.groupBy(x => x.notation.precedence).toList
    //      log("notations in scope: " + qnotations)
    qnotations.sortWith((p1, p2) => p1._1 < p2._1)
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
  // due to the recursive processing, variables in children are always found first
  // so adding to the front yields the right order
  private var vardecls: List[VarDecl] = Nil
  private var counter = 0

  private def next = {
    val s = counter.toString
    counter += 1
    s
  }

  private def resetVarGenerator() {
    vardecls = Nil
    counter = 0
  }

  /** an omitted implicit argument */
  private def newArgument =
    LocalName("") / "I" / next

  /** the omitted index type of a variable */
  private def newType(name: LocalName) =
    LocalName("") / name / next

  /** an explicitly omitted argument */
  private def newExplicitUnknown =
    LocalName("") / "_" / next

  /** a new unknown variable, constructed by applying a fresh name to all bound variables */
  private def newUnknown(name: LocalName, bvars: List[LocalName])(implicit pu: ParsingUnit) = {
    vardecls ::= VarDecl(name, None, None, None)
    if (bvars.isEmpty)
      OMV(name)
    else {
      //apply meta-variable to all bound variables in whose scope it occurs
      prag.defaultApplication(Some(pu.context.getIncludes.last), OMV(name), bvars.map(OMV(_)))
    }
  }

  /** the index choosing from a list of options in an ambiguity
   *  always an integer, thus independent of bound variables
   */
  private def newAmbiguity = {
    val name = LocalName("") / "A" / next
    vardecls ::= VarDecl(name, None, None, None)
    OMV(name)
  }
    

  /**
   * @param pu the parsing unit
   */
  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler): Term = {
    implicit val puI = pu
    //gathering notations and lexer extensions in scope
    val (parsing, lexing) = getRules(pu.context)
    val notations = tableNotations(parsing)
    resetVarGenerator()
    log("parsing: " + pu.term)
    log("notations:")
    logGroup {
      notations.foreach(o => log(o.toString()))
    }
    val escMan = new EscapeManager(controller.extman.lexerExtensions ::: lexing)
    val tl = TokenList(pu.term, escMan, pu.source.region.start)
    if (tl.getTokens.isEmpty) {
      makeError("no tokens found: " + pu.term, pu.source.region)
      DefaultObjectParser(pu)
    }
    else {
      //scanning
      val sc = new Scanner(tl, pu.top, controller.report)
      // scan once with the top notation and make sure it matches the whole input
      /*val wrongTop = pu.top match {
        case Some(n) =>
          log("scanning top notation: " + n)
          sc.scan(List(n))
          sc.tl.getTokens match {
            case hd :: Nil if hd.isInstanceOf[MatchedList] => false
            case _ =>
              makeError("top notation did not match whole input: " + n.toString, pu.source.region)
              true
          }
        case _ => false
      }
      if (wrongTop)
        DefaultObjectParser(pu)
      else {*/
        // now scan with all notations in increasing order of precedence
        notations foreach {
          case (priority, nots) => sc.scan(nots)
        }
        val scanned = sc.tl.getTokens match {
          case hd :: Nil => hd
          case _ => new UnmatchedList(sc.tl)
        }
        log("scan result: " + sc.tl.toString)
        // turn the syntax tree into a term
        val tm = logGroup {
          makeTerm(scanned, Nil)
        }
        log("parse result: " + tm.toString)
        if (vardecls.isEmpty)
          tm
        else
          OMBIND(OMID(ObjectParser.unknown), Context(vardecls: _*), tm)
      //}
    }
  }

  /** auxiliary function to collect all lexing and parsing rules in a given context */
  private def getRules(context: Context): (List[ParsingRule], List[LexerExtension]) = {
    val closer = new libraries.Closer(controller)
    val support = context.getIncludes
    //TODO we can also collect notations attached to variables
    support foreach { p => closer(p) }
    val includes = support.flatMap { p => controller.library.visible(OMMOD(p)) }.distinct
    val decls1 = includes flatMap { tm =>
      controller.localLookup.getO(tm.toMPath) match {
        case Some(d: modules.DeclaredTheory) => d.getDeclarations
        case _ => None
      }
    }
    val decls = decls1.flatMap {
      case s: DeclaredStructure => closer.getStructureDecls(s)
      case d => List(d)
    }.filter {
      case s: DeclaredStructure => s.isImplicit
      case _ => true
    }

    val nots = decls.flatMap {
      case nm: NestedModule =>
        val args = nm.module match {
          case t: Theory => t.parameters.length
          case v: View => 0
        }
        val tn = new TextNotation(Mixfix(Delim(nm.name.toString) :: Range(0, args).toList.map(Arg(_))),
          Precedence.infinite, None)
        List(ParsingRule(nm.module.path, tn))
      case c: Declaration with HasNotation =>
        var names = (c.name :: c.alternativeName.toList).map(_.toString) //the names that can refer to this declaration
        if (c.name.last == SimpleStep("_")) names ::= c.name.init.toString
        //the unapplied notations consisting just of the name
        val unapp = names map (n => new TextNotation(Mixfix(List(Delim(n))), Precedence.infinite, None))
        val app = c.not.toList
        (app ::: unapp).map(n => ParsingRule(c.path, n))
      case _ => Nil
    }
    val les = decls.flatMap {
      case r: RuleConstant => r.df match {
        case rt: uom.RealizedType => rt.lexerExtension.toList
        case _ => Nil
      }
      case _ => Nil
    }
    (nots, les)
  }

  /**
   * recursively transforms a TokenListElem (usually obtained from a [[Scanner]]) to an MMT Term
   * @param te the element to transform
   * @param boundVars the variable names bound in this term (excluding the variables of the context of the parsing unit)
   * @param pu the original ParsingUnit (constant during recursion)
   * @param attrib the resulting term should be a variable attribution
   */
  private def makeTerm(te: TokenListElem, boundVars: List[LocalName], attrib: Boolean = false)(implicit pu: ParsingUnit, errorCont: ErrorHandler): Term = {
    // cases may return multiple options in case of ambiguity 
    val alternatives: List[Term] = te match {
      case Token(word, _, _, _) =>
        lazy val unparsed = OMSemiFormal(objects.Text("unknown", word)) // fallback option
        val name = LocalName.parse(word)
        val term = if (boundVars.contains(name) || pu.context.exists(_.name == name)) {
          //single Tokens may be bound variables ...
          OMV(name)
        } else if (word == "_") {
          // unbound _ is a fresh unknown variable
          newUnknown(newExplicitUnknown, boundVars)
        } else if (word.count(_ == '?') > 0) {
          // ... or qualified identifiers
          val segments = utils.stringToList(word, "\\?")
          // recognizing identifiers ?THY?SYM is awkward because it would require always lexing initial ? as identifiers
          // but we cannot always prepend ? because the identifier could also be NS?THY
          // Therefore, we prepend ? using a heuristic
          val qid = segments match {
            case fst :: _ :: Nil if !fst.contains(':') && Character.isUpperCase(fst.charAt(0))
            => "?" + word
            case _ => word
          }
          try {
            Path.parse(qid, pu.nsMap) match {
              case p: ContentPath => OMID(p)
              case p =>
                makeError("content path expected: " + p, te.region)
                unparsed
            }
          } catch {
            case ParseError(msg) =>
              makeError(msg, te.region)
              unparsed
          }
        } else {
          //in all other cases, we don't know
          makeError("unbound token: " + word, te.region)
          unparsed
        }
        List(term)
      case e: ExternalToken =>
        List(e.parse(pu, boundVars, this))
      case ml: MatchedList =>
        makeTermFromMatchedList(ml, boundVars, attrib)
      case ul: UnmatchedList =>
        val term = if (ul.tl.length == 1)
        // process the single TokenListElement
          makeTerm(ul.tl(0), boundVars)
        else {
          /* This case arises if
          - the Term is ill-formed
          - the matching TextNotation is missing
          - a subterm has no delimiters (e.g., as in LF applications)
          - a semi-formal subterm consists of multiple text Tokens
          Consequently, it is not obvious how to proceed.
          By using defaultApplication, the behavior is somewhat configurable.
          */
          val terms = ul.tl.getTokens.map(makeTerm(_, boundVars))
          prag.defaultApplication(pu.context.getIncludes.lastOption, terms.head, terms.tail)
        }
        List(term)
    }
    alternatives foreach {term => SourceRef.update(term, pu.source.copy(region = te.region))}
    // if more than one alternative, wrap them in oneOf
    alternatives match {
        case hd :: Nil =>
           hd
        case l =>
           val av = newAmbiguity
           ObjectParser.oneOf(av::l)
     }
  }
  
  /** auxiliary method of makeTerm */
  private def makeTermFromMatchedList(ml: MatchedList, boundVars: List[LocalName], attrib: Boolean)(implicit pu: ParsingUnit, errorCont: ErrorHandler): List[Term] = {
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
     val newBVarNames = newBVars.map(_._2)
     // the prefix of newBVarNames just before a certain variable identified by its Var marker and
     // within that FoundVar's sequence of variables by its name
     def newBVarNamesBefore(vm: Var, name: LocalName): List[LocalName] = {
       val pos = newBVars.indexWhere(_ ==(vm, name))
       newBVarNames.take(pos)
     }
     // 3 lists for the children, in concrete syntax order, together with their position in the abstract syntax
     // the arguments before the variables
     var subs: List[(Int, Term)] = Nil
     // the variable declaration list (name + type)
     var vars: List[(Var, SourceRegion, LocalName, Option[Term])] = Nil
     // the arguments behind the variables
     var args: List[(Int, Term)] = Nil
     // We walk through found and fill subs, vars, and args by
     // recursively processing the respective TokenListElem in ml.tokens
     var i = 0 //the position of the next TokenListElem in ml.tokens
     found foreach {
       case _: FoundDelim =>
       case FoundArg(_, n) if n < firstVar =>
         // argument before the variables
         subs ::=(n, makeTerm(ml.tokens(i), boundVars))
         i += 1
       case FoundArg(_, n) =>
         // argument behind the variables: as above but all newBVarNames are added to the context
         args ::=(n, makeTerm(ml.tokens(i), boundVars ::: newBVarNames))
         i += 1
       // sequence arguments: as the two cases above, but as many TokenListElement as the sequence has elements
       case FoundSeqArg(n, fas) if n < firstVar =>
         val toks = ml.tokens.slice(i, i + fas.length)
         subs = subs ::: toks.map(t => (n, makeTerm(t, boundVars)))
         i += fas.length
       case FoundSeqArg(n, fas) =>
         val toks = ml.tokens.slice(i, i + fas.length)
         args = args ::: toks.map(t => (n, makeTerm(t, boundVars ::: newBVarNames)))
         i += fas.length
       case fv: FoundVar =>
         fv.getVars foreach { case SingleFoundVar(pos, nameToken, tpOpt) =>
           val name = LocalName(nameToken.word)
           // a variable declaration, so take one TokenListElement for the type
           val tp = tpOpt map { _ =>
             i += 1
             val stp = makeTerm(ml.tokens(i - 1), boundVars ::: newBVarNamesBefore(fv.marker, name), attrib = true)
             // remove toplevel operator, e.g., :
             stp match {
               case prag.StrictTyping(stpt) => stpt
               case _ => stp // applies to unregistered type attributions
             }
           }
           vars = vars ::: List((fv.marker, nameToken.region, name, tp))
         }
     }
     val cons = ml.an.rules.map(_.name)
     // hard-coded special case for a bracketed subterm
     if (cons == List(utils.mmt.brackets) || cons == List(utils.mmt.andrewsDot)) {
       //TODO add metadata for keeping track of brackets
       // source ref of the returned term will be overridden with the source ref of the bracketed term
       return List(args.head._2)
     }
     // process subs, vars, and args, which are needed to build the term
     // this includes sorting args and vars according to the abstract syntax
     // add implicit arguments before the variables
     val finalSubs: List[Term] = arity.subargs.flatMap {
        case ImplicitArg(_, _) =>
          List(newUnknown(newArgument, boundVars))
        case Arg(n, _) =>
          val a = subs.find(_._1 == n).get // must exist if notation matched
          List(a._2)
        case SeqArg(n, _, _) =>
          val as = subs.filter(_._1 == n)
          as.map(_._2)
     }
     val finalSub = Substitution(finalSubs.map(a => Sub(OMV.anonymous, a)): _*)
     // add implicit arguments behind the variables (same as above except for using newBVarNames)
     val finalArgs: List[Term] = arity.arguments.flatMap {
        case ImplicitArg(_, _) =>
          List(newUnknown(newArgument, boundVars ::: newBVarNames))
        case Arg(n, _) =>
          val a = args.find(_._1 == n).get // must exist if notation matched
          List(a._2)
        case SeqArg(n, _, _) =>
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
              //new meta-variable for unknown type
              //under a binder, apply the meta-variable to all governing bound variables
              //these are the boundVars and all preceding vars of the current binder
              val governingBVars = boundVars ::: newBVarNamesBefore(vm, vname)
              val t = newUnknown(newType(vname), governingBVars)
              (Some(t), true)
          }
          // using metadata to signal that the type was unknown
          // TODO: clean up together with Twelf output
          val vd = VarDecl(vname, finalTp, None, None)
          if (unknown)
            metadata.Generated.set(vd)
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
      * 
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
              val u = newUnknown(newArgument, boundVars)
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
     def makeAlternative(con: ContentPath): Term = {
        if (arity.isConstant && args == Nil && vars == Nil && !attrib) {
          //no args, vars, scopes --> OMID
          return OMID(con)
        }
        // the level of the notation: if not provided, default to the meta-theory of the constant
        val level = notation.meta orElse {
           controller.globalLookup.getO(con.module.toMPath) match {
             case Some(t: modules.DeclaredTheory) => t.meta
             case _ => None
           }
        }
        con match {
           case con: MPath =>
             if (finalSubs.nonEmpty || finalVars.nonEmpty)
               makeError("no context or substitution allowed in module application", ml.region)
             OMPMOD(con, finalArgs)
           case con: GlobalName =>
             prag.makeStrict(level, con, finalSub, Context(finalVars: _*), finalArgs, attrib, notation)(
                () => UnknownCacher.getNext
             )
        }
     }
     // construct the alternative terms
     cons map {con =>
        val a = makeAlternative(con)
        UnknownCacher.prepareNextRun
        a
     }
  }
}
