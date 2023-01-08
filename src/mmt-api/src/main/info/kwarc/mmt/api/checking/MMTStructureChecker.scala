package info.kwarc.mmt.api.checking


import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import objects._
import ontology._
import parser._
import frontend._
import info.kwarc.mmt.api.utils.MMT_TODO
import opaque._
import objects.Conversions._

/** variant of CheckingEnvironment that carries around more structure */
case class ExtendedCheckingEnvironment(ce: CheckingEnvironment, objectChecker: ObjectChecker, rules: RuleSet, current: Path, var timeout: Int = 0) {
  def pCont(q: Path): Unit = {
    ce.reCont(RefersTo(q, current))
  }

  def errorCont(e: Error): Unit = {
    ce.errorCont(e)
  }

  def extSimpEnv = uom.ExtendedSimplificationEnvironment(ce.simpEnv, ce.simplifier.objectLevel, rules)
}

/** auxiliary class for the [[MMTStructureChecker]] to store expectations about a constant */
case class Expectation(tp: Option[Term], df: Option[Term])

/** A StructureChecker traverses structural elements and checks them, calling the object checker as needed.
  * 
  * After checking an element, it is immediately elaborated.
  *
  * Deriving classes may override unitCont and reCont to customize the behavior.
  */
class MMTStructureChecker(objectChecker: ObjectChecker) extends Checker(objectChecker) {
  val id = "mmt"
  private lazy val extman = controller.extman
  private implicit lazy val content = controller.globalLookup
  override val logPrefix = "structure-checker"

  /* interaction with simplifier
   * 
   * All container elements are streamed in to the simplifier even if they are not streamed into the checker.
   * simplifier.applyElementBegin and applyElementEnd are called (as well as calls for every child).
   * These calls happen at the end of checkElementBegin as well as of elementChecked, which is called by both check and checkElementEnd.
   * 
   * For non-container elements simplifier.applyChecked is called in elementChecked. 
   */
  
  
  /* the entry points
   * invariant for container elements: applyElementBegin + apply on every child + applyElementEnd <=> apply 
   */
  def apply(e: StructuralElement)(implicit ce: CheckingEnvironment): Unit = {
    applyWithTimeout(e, None)
  }
  /** generalization of apply to allow for a timeout for generated type checking tasks */
  def applyWithTimeout(e: StructuralElement, t : Option[Int])(implicit ce: CheckingEnvironment): Unit = {
    if (! e.isGenerated) {
      val (context,env) = prepareCheck(e)
      t.foreach {env.timeout = _}
      check(context, e, streamed = false)(env)
    }
  }

  def applyElementBegin(e: StructuralElement)(implicit ce: CheckingEnvironment): Unit = {
    if (! e.isGenerated) {
      val (context,env) = prepareCheck(e)
      e match {
        case ce: ContainerElement[_] => checkElementBegin(context, ce)(env)
        case ne: NestedModule => applyElementBegin(ne.module)
        case _ => check(context, e, streamed = true)(env)
      }
    }
  }

  def applyElementEnd(e: ContainerElement[_])(implicit ce: CheckingEnvironment): Unit = {
    if (! e.isGenerated) {
      val (context,env) = prepareCheck(e)
      checkElementEnd(context, e)(env)
    }
  }

  /** called after checking an element */
  private def elementChecked(e: StructuralElement)(implicit env: ExtendedCheckingEnvironment): Unit = {
    if (!env.ce.task.isKilled) {
      e match {
        case e: ContainerElement[_] =>
          env.ce.simplifier.applyElementEnd(e)(env.ce.simpEnv)
        case _ =>
          env.ce.simplifier.applyChecked(e)(env.ce.simpEnv)
      }
    }
    UncheckedElement.erase(e)
    env.ce.task.reportProgress(Checked(e))
    new Notify(controller.extman.get(classOf[ChangeListener]), report).onCheck(e)
  }

  @deprecated("MMT_TODO: unclear what happens here", since="forever")
  def elabContext(th : Theory)(implicit ce: CheckingEnvironment): Context = {
    //val con = getContext(th)
    val rules = RuleSet.collectRules(controller,Context.empty)
    implicit val env = new ExtendedCheckingEnvironment(ce, objectChecker, rules, th.path)
    implicit val task = ce.task
    val c = controller.getExtraInnerContext(th)
    checkContextTop(Context.empty, c)
  }

  private def prepareCheck(e: StructuralElement)(implicit ce: CheckingEnvironment): (Context, ExtendedCheckingEnvironment) = {
    val context = controller.getContext(e)
    val rules = RuleSet.collectRules(controller, context)
    val env = new ExtendedCheckingEnvironment(ce, objectChecker, rules, e.path)
    (context, env)
  }

  private def prepareCheckExtendContext(context: Context, env: ExtendedCheckingEnvironment, additionalContext: Context): (Context, ExtendedCheckingEnvironment) = {
     val contextI = context ++ additionalContext
     val rulesI = RuleSet.collectAdditionalRules(controller, Some(env.rules), additionalContext)
     val envI = env.copy(rules = rulesI)
     (contextI, envI)
  }

  /**
    * @param context all variables and theories that may be used in e (including the theory in which is declared)
    * @param e       the element to check
    * @param streamed true if a later call of checkElementEnd on this element or its parent is expected; all checks performed in checkElementEnd are skipped
    */
  // e will be marked as checked (independent of whether there are errors)
  private def check(context: Context, e: StructuralElement, streamed: Boolean)(implicit env: ExtendedCheckingEnvironment): Unit = {
    implicit val ce = env.ce
    val path = e.path
    log("checking " + path)//+ " using the following rules: " + env.rules.toString)
    UncheckedElement.set(e)

    // Global switch-case on what to check
    // ==================================
    e match {
      case c: ContainerElement[_] =>
        checkElementBegin(context, c)
        // mark all children as unchecked
        val tDecls = c.getPrimitiveDeclarations
        tDecls foreach {d =>
          UncheckedElement.set(d)
        }
        // check children
        val additionalContext = checkContextTop(context, controller.getExtraInnerContext(c))
        val (contextI, envI) = prepareCheckExtendContext(context, env, additionalContext)
        logGroup {
          tDecls foreach {d =>
            check(contextI, d, false)(envI.copy(current = d.path))
          }
        }
        if (!streamed)
           checkElementEnd(context, c)
      case oe: OpaqueElement =>
        // when streamed, opaque elements are checked in the checkElementEnd method of their parent (to allow for forward-references)
        if (!streamed) {
          controller.extman.get(classOf[OpaqueChecker], oe.format) match {
            case None =>
              env.errorCont(InvalidElement(oe, "no checker found for format " + oe.format))
            case Some(oc) =>
              oc.check(objectChecker, context, env.rules, oe)
          }
        }
        //TODO check all declarations, components? currently done in each OpaqueChecker
      case r: NRef =>
        val target = controller.getO(r.target).getOrElse {
          env.errorCont(InvalidElement(r, "reference to unknown element"))
        }
        //TODO decide what to do here; usually, checking is redundant and may even fail here if the context is not fully elaborated and loaded
        //apply(target)
      case ii: InterpretationInstruction =>
        // nothing to do; but we could carry a namespace map around        
      case nm: NestedModule =>
        check(context, nm.module, streamed)
      case rc: RuleConstant =>
        rc.tp foreach {tp =>
          val (tpR,_) = checkTermTop(context, tp)
          rc.tpC.analyzed = tpR
          if (rc.df.isEmpty) {
            if (ParseResult.fromTerm(tpR).isPlainTerm) {
              try {
                new RuleConstantInterpreter(controller).createRule(rc)
              } catch {
                case e: Error =>
                  env.errorCont(e)
              }
            } else {
              env.errorCont(InvalidElement(rc, "type of rule constant not fully checked"))
            }
          }
        }

      case c: Constant =>
        // determine the expected type and definiens (if any) based on the parent element
        /* TODO if computation of the expected type fails due to a missing assignments, we could generate a fresh unknown and try to infer
           the assignment during the later checks
         */
        val parent = content.getParent(c)
        lazy val defaultExp = Expectation(None, None)

        // Step 1: compute typing expectation from constant based on parent
        // ----------------------------------------------------------------
        val expectation = parent match {
          case thy: Theory =>
            c.name.steps.head match {
              case ComplexStep(r) =>
                val reals = thy.getRealizees
                reals.find(_.from == r) match {
                  case None =>
                    env.errorCont(InvalidElement(c, "cannot find realized constant"))
                    defaultExp
                  case Some(real) =>
                    content.getO(r ? c.name.tail) match {
                      case Some(cOrg: Constant) =>
                        val mor = real.asMorphism
                        def tr(t: Term) = content.ApplyMorphs(t, real.asMorphism)
                        Expectation(cOrg.tp map tr, cOrg.df map tr)
                      case _ =>
                        env.errorCont(InvalidElement(c, "cannot find realized constant"))
                        defaultExp
                    }
                }
              case _ =>
                defaultExp
            }

          // Constants living in a link
          //
          // Here, the constant c really encodes an assignment `domainSymbol of type c.tp := c.df`.
          // Hence, generate the expectation `|- v(c.tp) : v(c.df)` where `v` is the homomorphic extension
          // of the link
          case link: Link =>
            val rc = try {content.get(link.from, c.name)}
                     catch {case g: GetError => throw GetError(g.path, "cannot find realized constant").setCausedBy(g)}
            rc match {
              case cOrg: Constant =>
                def tr(t: Term) = content.ApplyMorphs(t, link.toTerm)
                Expectation(cOrg.tp map tr, cOrg.df map tr)
              case _ =>
                env.errorCont(InvalidElement(c, "cannot find realized constant"))
                Expectation(None, None)
            }
          case dd: DerivedModule =>
            val sfOpt = extman.getOrAddExtension(classOf[ModuleLevelFeature], dd.feature)
            sfOpt match {
              case None => defaultExp
              case Some(sf) =>
                val expTpO = sf.expectedType(dd, c)
                Expectation(expTpO, None)
            }
          case dd: DerivedDeclaration =>
            val sfOpt = extman.getOrAddExtension(classOf[StructuralFeature], dd.feature)
            sfOpt match {
              case None => defaultExp
              case Some(sf) =>
                val expTpO = sf.expectedType(dd, c)
                Expectation(expTpO, None)
            }
          case _ =>
            defaultExp
        }

        // Step 2: do actual checking
        // -------------------------------------------------------

        /* auxiliary function for structural checking of a term:
         * @return unknown variables, structural reconstruction, boolean result of latter
         */
        def prepareTerm(t: Term): (ParseResult, Boolean) = {
          val pr = ParseResult.fromTerm(t)
          val (tR, valid) = checkTermTop(context ++ pr.unknown ++ pr.free, pr.term)
          (ParseResult(pr.unknown, pr.free, tR), valid)
        }
        /* shared code for checking a type */
        def checkInhabitable(pr: ParseResult): Unit = {
            val j = Inhabitable(Stack(pr.free), pr.term)
            val cu = CheckingUnit(Some(c.path $ TypeComponent), context, pr.unknown, j).diesWith(env.ce.task)
            if (env.timeout != 0)
               cu.setTimeout(env.timeout)(() => log("Timed out!"))
            objectChecker(cu, env.rules)
          
        }

        // Step 2.1: checking if type of c is inhabited (if given)
        // ----------------------------------------------------------
        getTermToCheck(c.tpC, "type") foreach {t =>
          val (pr, valid) = prepareTerm(t)
          if (valid) {
            checkInhabitable(pr)
          }
        }

        // Step 2.2: checking the typing
        //           (both from given type/definiens and from expectation)
        // ----------------------------------------------------------------

        // Step 2.2a: check expected type
        // ------------------------------------
        expectation.tp foreach {expTp =>
          c.tp match {
            case Some(tp) =>
            // c already has a type: keep it, but check compatibility
            //val j = Subtype(Stack(scope, parR), tp, expType)
            //objectChecker(ValidationUnit(c.path $ TypeComponent, Context(), j))
            case None =>
              // c has no type: copy over the expected type
              val tr = env.ce.simplifier.objectLevel.toTranslator(env.rules, false)
              val expTpS = tr.applyPlain(context, expTp)
              c.tpC.analyzed = expTpS
          }
        }

        // Step 2.2b: check that definiens of c (if given) type-checks
        //            against the type of c (if given or expected)
        // -------------------------------------------------------------
        val tpVar = CheckingUnit.unknownType
        getTermToCheck(c.dfC, "definiens") foreach {d =>
          val (pr, valid) = prepareTerm(d)
          if (valid) {
            val cp = c.path $ DefComponent
            val (unknowns, expTp, inferType) = c.tp match {
              case Some(t) =>
                (pr.unknown, t, false)
              case None =>
                // try to guess the type of d by inferring without checking
                val dIO = Solver.infer(controller, context ++ pr.free ++ pr.unknown, pr.term, Some(env.rules))//,pr.unknown)
                dIO match {
                  // we can only use the inferred type if no extra variables are left in it
                  case Some(dI) if dI.freeVars.forall(x => (context++pr.unknown).isDeclared(x))  =>
                    // dI was computed by trusting d, so we need to check it as well; also this call sets c.tp
                    //checkInhabitable(ParseResult(pr.unknown.filter(v => dI.freeVars.contains(v.name)),pr.free, dI))
                    (pr.unknown ++ VarDecl(tpVar,None,None,Some(dI),None), dI, true)
                  case _ =>
                    (pr.unknown ++ VarDecl(tpVar, None, None, None, None), OMV(tpVar), true)
                }
            }
            val j = Typing(Stack(pr.free), pr.term, expTp, None)
            val cu = CheckingUnit(Some(cp), context, unknowns, j).diesWith(env.ce.task)
            if (env.timeout != 0)
              cu.setTimeout(env.timeout)(() => log("Timed out!"))
            val cr = objectChecker(cu, env.rules)
            if (inferType && cr.solved) {
              // if no expected type was known but the type could be inferred, add it
              cr.solution.foreach { sol =>
                val tp = sol(tpVar).df
                c.tpC.analyzed = tp
              }
            }
          }
        }

        // Step 2.2c: additional treatment if there is an expected definiens
        // ------------------------------------------------------------------
        expectation.df foreach {expDef =>
          c.df match {
            case Some(df) =>
            // c already has a definiens: keep it, but check equality
            //val j = Equality(Stack(scope, parR), df, expDef, None)
            //objectChecker(ValidationUnit(c.path $ DefComponent, Context(), j))
            case None =>
              // c has no definiens: copy over the expected definiens
              c.dfC.analyzed = expDef
          }
        }
      case _ =>
        //succeed for everything else but signal error
        logError("unchecked " + path)
    }
    e match {
      case _:ContainerElement[_] if streamed =>
        // streamed container elements are finalized in checkElementEnd
      case _ =>
        elementChecked(e)
    }
  }

  /** determines which dimension of a term (parsed, analyzed, or neither) is checked */
  private def getTermToCheck(tc: TermContainer, dim: String) = {
    if (tc.parsed.isDefined && tc.analyzed.isDefined) {
      if (tc.checkNeeded) {
        log(s"re-checking dirty or ill-formed $dim")
        tc.parsed
      } else {
        log(s"skipping $dim (uptodate and well-formed)")
        None
      }
    } else {
      val t = tc.get
      if (t.isDefined)
        log(s"checking $dim")
      else
        log(s"no $dim given")
      t
    }
  }

  // ***** ContainerElements *****

  /** auxiliary method of check */
  private def checkElementBegin(context : Context, e : ContainerElement[_<: StructuralElement])(implicit env: ExtendedCheckingEnvironment): Unit = {
    UncheckedElement.set(e)
    e.getPrimitiveDeclarations foreach {d => UncheckedElement.set(d)}
    val path = e.path
    log("checking begin of " + path )
    val rules = env.rules
    implicit val ce = env.ce
    e match {
      case d: Document =>
      case t: Theory =>
        var contextMeta = context
        t.meta foreach { mt =>
          checkTheory(Some(CPath(t.path, TypeComponent)), context, OMMOD(mt))
          contextMeta = contextMeta ++ mt
        }
        checkContextTop(contextMeta, t.parameters)
        t.df map {d => checkTheory(Some(CPath(t.path, DefComponent)), contextMeta++t.parameters, d)}
        // this is redundant on a clean check because e is empty then;
      case v: View =>
        checkTheory(CPath(v.path, DomComponent), v, context, v.fromC.get)
        checkTheory(CPath(v.path, CodComponent), v, context, v.toC.get)
        // check definiens, use it to update domain/codomain
        v.df foreach {df =>
          val (dfR, fromR, toR) = checkMorphism(context, df, v.fromC.get, v.toC.get)
          v.fromC.analyzed = fromR
          v.toC.analyzed = toR
          v.dfC.analyzed = dfR
        }
      case s: Structure =>
        s.fromC.get foreach {t => checkTheory(Some(CPath(s.path, TypeComponent)), context, t)}
        // check definiens, use it to update/infer domain
        val (thy, linkOpt) = content.getDomain(s)
        linkOpt foreach {l =>
          if (s.isImplicit && linkOpt.isDefined) {
            throw InvalidElement(s,s.feature + " in a " + l.feature + " may not be implicit")
          }
        }
        s.df.foreach {df =>
          val (expectedDomain, expectedCodomain) = linkOpt match {
            case None =>
              (s.fromC.get, Some(thy match {
                case _ : Theory => thy.toTerm
                case at : AbstractTheory => OMMOD(at.modulePath)
              }))
            case Some(link) =>
              val sOrg = content.getStructure(thy.modulePath ? s.name)
              val sOrgFrom = sOrg.from
              s.fromC.get match {
                case None =>
                  // if no domain is given, copy it over from thy
                  s.fromC.analyzed = sOrgFrom
                case Some(f) =>
                  // otherwise, make sure they're equal 
                  if (!TheoryExp.equal(sOrgFrom, f))
                    env.errorCont(InvalidElement(s, "import-assignment has bad domain: found " + f + " expected " + sOrgFrom))
              }
              (Some(s.from), Some(link.to))
          }
          val (dfR, fromR, _ ) = checkMorphism(context, df, expectedDomain, expectedCodomain)
          s.fromC.analyzed = fromR
          s.dfC.analyzed = dfR
        }
        if (s.fromC.get.isEmpty)
          throw InvalidElement(s, "could not infer domain of structure")
      case dm: DerivedModule =>
        var contextMeta = context
        dm.meta foreach {mt =>
          checkTheory(Some(CPath(dm.path, TypeComponent)), context, OMMOD(mt))
          contextMeta = contextMeta ++ mt
        }
        val sfOpt = extman.getOrAddExtension(classOf[ModuleLevelFeature], dm.feature)
        sfOpt match {
          case None =>
            env.errorCont(InvalidElement(dm, s"structural feature '${dm.feature}' not registered"))
          case Some(sf) =>
            // TODO check header of derived module properly
            dm.df foreach {df =>
              // this is one possible check, but for DiagramDefinitions, it cannot handle the dynamic extension of the context
              // val (dfR,_) = checkTermTop(contextMeta, df)
              dm.dfC.analyzed = df
            }
        }
      case dd: DerivedDeclaration =>
        val sfOpt = extman.get(classOf[StructuralFeature], dd.feature)
        sfOpt match {
          case None =>
            env.errorCont(InvalidElement(dd, s"structural feature '${dd.feature}' not registered"))
          case Some(sf) =>
            dd.tpC.get foreach {tp =>
              val tpR = checkTermTop(context, tp)
              // not using tpR here because source references are gone
            }
            dd.dfC.get foreach {df =>
              val dfR = checkTermTop(context, df)
              // not using dfR here because source references are gone
            }
        }
      case _ =>
        //succeed for everything else but signal error
        logError("unchecked " + path)
    }
    if (!env.ce.task.isKilled) {
      ce.simplifier.applyElementBegin(e)(ce.simpEnv)
    }
  }

  /** auxiliary method of check */
  private def checkElementEnd(context: Context, e: ContainerElement[_])(implicit env: ExtendedCheckingEnvironment): Unit = {
    log("checking end of " + e.path )
    val ce = env.ce
    e match {
      case d: Document =>
      case t: Theory =>
        val reals = t.getRealizees
        reals foreach {r =>
          r.df match {
            case None =>
              checkTotalTop(t, OMMOD(r.from))
            case Some(df) =>
              checkMorphism(context, df, Some(OMMOD(r.from)), Some(t.toTerm))
          }
        }
      case v: View =>
        checkTotalTop(v, v.from)
      case Include(_) =>
      case s: Structure =>
        if (s.isTotal) {
          checkTotalTop(s, s.from)
        }
      case dm: DerivedModule =>
        val sfOpt = extman.get(classOf[ModuleLevelFeature], dm.feature)
        // error for sfOpt.isEmpty is raised in checkElementegin already
        sfOpt foreach {sf =>
          sf.check(dm)
        }
        
      case dd: DerivedDeclaration =>
        val sfOpt = extman.get(classOf[StructuralFeature], dd.feature)
        // error for sfOpt.isEmpty is raised in checkElementegin already
        sfOpt foreach {sf =>
          sf.check(dd)
        }
      case _ =>
        //succeed for everything else but signal error
        logError("unchecked " + e.path)
    }
    // check all the narrative structure (at the end to allow forward references)
    val (contextI, envI) = prepareCheckExtendContext(context, env, controller.getExtraInnerContext(e))
    def doDoc(ne: NarrativeElement): Unit = {
      ne match {
        case doc: Document => doc.getDeclarations foreach doDoc
        case r: NRef =>
        case ii: InterpretationInstruction =>
        case oe: OpaqueElement =>
          check(contextI, oe, false)(envI)
      }
    }
    e match {
      case d: Document =>
        doDoc(d)
      case b: ModuleOrLink =>
        doDoc(b.asDocument)
      case _ =>
    }
    elementChecked(e)
  }
  
  private def checkTotalTop(mod: ModuleOrLink, thyTerm: Term)(implicit env: ExtendedCheckingEnvironment): Unit = {
    val unmappedNames = checkTotal(mod, thyTerm)
    val total = unmappedNames.isEmpty
    if (!total) {
      val ie = new InvalidElement(mod, mod.feature + " is not total") {
        override val excuse = Some(Level.Gap)
        override def extraMessage = unmappedNames.mkString("\n")
      }
      env.errorCont(ie)
    }
  }
  /** checks if a modules (theory, view, or structure) is total for a given theory
    * @param mod the module
    * @param thyTerm the theory
    * @return all names for which an assignment is missing
    * pre: thyTerm has been simplified already
    */
  private def checkTotal(mod: ModuleOrLink, thyTerm: Term)(implicit env: ExtendedCheckingEnvironment): List[GlobalName] = {
    val thyPath = thyTerm match {
      case OMPMOD(p,_) => p
      case _ => return Nil // TODO make sure this does not happen
    }
    val thy = content.getAs(classOf[Theory], thyPath)
    // convenience for the two kinds of results
    def mapped = Nil
    def unmapped(n: LocalName) = List(thyPath ? n)
    val unm = thy.getDeclarations flatMap {
      case c: Constant =>
        if (c.df.isDefined) mapped else {
          val names = c.name.prefixes
          val isMapped = names.exists {n => mod.getO(LocalName(thyPath) / c.name) match {
            case Some(nA: Constant) => nA.df.isDefined
            case Some(nS: Structure) => nS.df.isDefined
            case _ => false
          }}
          if (isMapped) mapped
          else unmapped(c.name)
        }
      case s: Structure =>
        if (s.df.isDefined) mapped else s match {
          case Include(id) if id.isRealization =>
            mapped
          case Include(id) =>
            mod.getO(LocalName(id.from)) match {
              case Some(nS: Structure) =>
                // no need to look for assignments to theories that subsume id.from because thy is flattened
                mapped // in theories, id is allowed to be undefined; in views, definedness of id is checked elsewhere anyway 
              case _ =>
                // recurse into includes
                checkTotal(mod, s.from)
            }
          case _ =>
            // we assume a structure is mapped if its elaboration is
            mapped
        }
      case nm: NestedModule => Nil
      case dd: DerivedDeclaration =>
        if (dd.df.isDefined) mapped else {
          // TODO treatment of derived declarations in links not specified yet
          // for now we assume it is mapped if its elaboration is
          mapped
        }
      case d =>
        // TODO add more cases if needed
        unmapped(d.name)
    }
    unm.distinct
  }

  // *****

  /** auxiliary function for setting the analyzed dimension after checking a module expression */
  private def setAnalyzed(cpath: CPath, t: Term): Unit = {
    controller.globalLookup.getComponent(cpath) match {
      case tc: TermContainer => tc.analyzed = t
      case _ =>
    }
  }
  
  /** checks whether a theory object is well-formed
    *
    * @param cpath the component (if any) that is this theory, to be updated by the object checker
    * @param context the context relative to which m is checked
    * @param t       the theory
    * @return the reconstructed theory
    */
  private def checkTheory(cpath: Option[CPath], context: Context, t: Term)(implicit env: ExtendedCheckingEnvironment): Term = t match {
    // TODO these cases should become a rule of the object checker
    case OMPMOD(p, args) =>
      val dec = try {
        controller.globalLookup.get(p)
      } catch {
        case e: Error =>
          env.errorCont(InvalidObject(t, "unknown identifier: " + p.toPath).setCausedBy(e))
          return t
      }
      val thy = dec match {
        case t: Module => t
        case nm: NestedModule => nm.module
        case dd: DerivedDeclaration if (
          controller.extman.get(classOf[StructuralFeature], dd.feature).getOrElse(throw GeneralError("Structural feature "+dd.feature+" not found."))
            match {case sf : ParametricTheoryLike => true
          case _ => false
          }) => dd
        case _ =>
          env.errorCont(InvalidObject(t, "not a theory: " + controller.presenter.asString(t)))
          dec
      }
      val tR: Term = thy match {
        case thy: Theory =>
          // check instantiation
          val pars = thy.parameters
          if (pars.nonEmpty && args.nonEmpty) {
            env.pCont(p)
            val subs = (pars / args).getOrElse {
              env.errorCont(InvalidObject(t, "bad number of arguments, expected " + pars.length))
              context.id
            }
            checkSubstitution(context, subs, pars, Context.empty, false)
          }
          // check visibility of thy
          thy.superModule match {
            case None => t // root modules are always visible
            case Some(parent) =>
              controller.globalLookup.getImplicit(OMMOD(parent), ComplexTheory(context)) match {
                case None =>
                  env.errorCont(InvalidObject(t, "theory not visible in current context"))
                  t
                case Some(m) =>
                  if (!Morph.isInclude(m)) {
                     // note: to allow the pushout, we have to at least consider the note in Library.getInLink 
                     env.errorCont(InvalidObject(t, "theory is visible via morphism " + m + " but pushout is not implemented yet"))
                  }
                  t
              }
          }
        case dd: DerivedDeclaration if (
          controller.extman.get(classOf[StructuralFeature], dd.feature).getOrElse(throw GeneralError("Structural feature "+dd.feature+" not found."))
          match {case sf : ParametricTheoryLike => true
          case _ => false
          }) => t
        case _ =>
          env.errorCont(InvalidObject(t, "not a theory identifier: " + p.toPath))
          t
      }
      cpath foreach {cp => setAnalyzed(cp, tR)} 
      tR
    case ComplexTheory(body) =>
      val bodyR = checkContextTop(context, body)
      val tR = ComplexTheory(bodyR)
      cpath foreach {cp => setAnalyzed(cp, tR)}
      tR
    case _ =>
      val prt = ParseResult.fromTerm(t)
      val j = Typing(Stack(prt.free), prt.term, OMS(ModExp.theorytype))
      val cu = CheckingUnit(cpath, context, prt.unknown, j)
      val result = objectChecker(cu, env.rules)(env.ce)
      result.term
  }
  /** like checkTheory but reports an error if the theory is absent */
  private def checkTheory(cpath: CPath, elem: StructuralElement, context: Context, tO: Option[Term])(implicit env: ExtendedCheckingEnvironment): Unit = {tO match {
    case Some(t) =>
      checkTheory(Some(cpath), context, t)
    case None =>
      env.errorCont(InvalidElement(elem, "missing " + cpath.component))
  }}

  /** checks whether a morphism object is well-formed relative to a domain and a codomain
    * domain and codomain are (tried to be) infered if omitted
    *
    * @param context the context relative to which m is checked
    * @param m       the morphism
    * @param domOpt  the domain, None if it is to be inferred
    * @param codOpt  the codomain
    * @return the reconstructed morphism (with implicit morphisms inserted), its domain, and codomain
    */
  private def checkMorphism(context: Context, m: Term, domOpt: Option[Term], codOpt: Option[Term])(implicit env: ExtendedCheckingEnvironment): (Term, Term, Term) = {
    val dom = domOpt orElse Morph.domain(m)(content) getOrElse {
      throw InvalidObject(m, "cannot infer domain of morphism")
    }
    val cod = codOpt orElse Morph.codomain(m)(content) getOrElse {
      throw InvalidObject(m, "cannot infer codomain of morphism")
    }
    lazy val domC = ComplexTheory.unapply(dom) getOrElse {
      throw InvalidObject(m, "domain is not a theory")
    }
    lazy val codC = ComplexTheory.unapply(cod) getOrElse {
      throw InvalidObject(m, "codomain is not a theory")
    }
    val (mR, domI, codI) = m match {
      case OMMOD(p) =>
        val l = content.getAs(classOf[Link], p)
        env.pCont(p)
        (m, l.from, l.to)
      case OMS(p) =>
        checkTheory(None, context, OMMOD(p.module))
        content.get(p) match {
          case l: Structure =>
            env.pCont(l.path)
            (m, l.from, l.to)
          case _ =>
            throw InvalidObject(m, "invalid morphism")
        }
      case OMIDENT(t) =>
        checkTheory(None, context, t)
        (m, t, t)
      case OMCOMP(ms) => ms.filter(_ != OMCOMP()) match {
        case Nil => (m, dom, cod)
        case ms =>
          val (initR, r, s1) = checkMorphism(Context.empty, OMCOMP(ms.init), Some(dom), None)
          val (lastR, s2, t) = checkMorphism(context, ms.last, Some(s1), Some(cod))
          // implicit morphism s1 -> s2 is inserted into lastR by recursive call
          (OMCOMP(initR :: List(lastR)), r, t)
      }
      case ComplexMorphism(body) =>
        // get domain and codomain as contexts
        val bodyR = checkSubstitution(context, body, domC, codC, false)
        (ComplexMorphism(bodyR), dom, cod)
      case OMStructuralInclude(f,t) =>
        val tThy = content.getAs(classOf[Theory], t)
        tThy.getRealizees.find(_.from == f) match {
          case None =>
            throw InvalidObject(m, "no such realization declared")
          case Some(id) =>
        }
        (m, OMMOD(f), OMMOD(t))
      case _ =>
        throw InvalidObject(m, "unknown morphism")
    }
    val implDom = content.getImplicit(dom, domI)
    val implCod = content.getImplicit(codI, ComplexTheory(context ++ codC))
    val mRR = (implDom, implCod) match {
      case (Some(l0), Some(l1)) => OMCOMP(l0, mR, l1)
      case _ =>
        // content.getImplicit(codI, ComplexTheory(context ++ codC)) // helpful for debugging if the error below occurs
        env.errorCont(InvalidObject(m, "ill-formed morphism: expected " + dom + " -> " + cod + ", found " + domI + " -> " + codI))
        m
    }
    (mRR, dom, cod)
  }

  /**
    * special case of checkMorphism, where the morphism is checked relative to its codomain
    */
  private def checkRealization(context: Context, r: Term, dom: Term)(implicit env: ExtendedCheckingEnvironment) =
    checkMorphism(context, r, Some(dom), Some(TheoryExp.empty))

  /**
    * Checks structural well-formedness of a closed term relative to a home theory.
    *
    * @param context the context
    * @param t       the term
    * @return the reconstructed term and a flag to signal if there were errors
    */
  private def checkTermTop(context: Context, t: Term)(implicit env: ExtendedCheckingEnvironment): (Term, Boolean) = {
    // wrap the error handler in a tracker to see if this term introduced errors
    val envTracking = env.copy(ce = env.ce.copy(errorCont = new TrackingHandler(env.ce.errorCont)))
    val tR = checkTerm(context, t)(envTracking,t)
    (tR, !envTracking.ce.errorCont.hasNewErrors)
  }

  /**
    * Checks structural well-formedness of a term in context relative to a home theory.
    *
    * @param context the context
    * @param s       the term
    * @param rootObj the root term on which the method was called
    * @return the reconstructed term
    */
  //TODO make more reusable (maybe by moving to RuleBasedChecker?)
  private def checkTerm(context: Context, s: Term)(implicit env: ExtendedCheckingEnvironment, rootObj: Obj): Term = {
    s match {
      case OMMOD(p) =>
        if (p.doc.uri.scheme contains "scala") {
          // TODO Scala classes/objects will be loaded in different ways depending on function
        } else {
          val mOpt = content.getO(p)
          if (mOpt.isEmpty) {
            env.errorCont(InvalidObject(s, "ill-formed module reference"))
          }
          mOpt match {
            case Some(m: Module) =>
              //TODO check for visibility of a module
            case _ =>
          }
        }
        env.pCont(p)
        s
      case OMS(path) =>
        val ceOpt = content.getO(path) orElse {
          // heuristically try to resolve an unknown path
          val options = content.resolveName(List(path.module), path.name)
          options match {
            case Nil =>
              env.errorCont(InvalidObject(s, "ill-formed constant reference " + path))
              None
            case hd :: Nil =>
              content.getO(hd)
            case _ => 
              env.errorCont(InvalidObject(s, "ambiguous constant reference " + path))
              None
          }
        }
        ceOpt match {
          case Some(d: Declaration) =>
            val pathR = d.path
            if (!content.hasImplicit(d.home, ComplexTheory(context))) {
              env.errorCont(InvalidObject(s, "constant " + d.path + " is not imported into current context " + context))
            }
            if (UncheckedElement.is(d))
              env.errorCont(InvalidObject(s, "constant " + d.path + " is used before being declared " + context))
            env.pCont(pathR)
            //TODO wrap in implicit morphism?
            OMS(pathR).from(s)
          case Some(_) =>
            env.errorCont(InvalidObject(s, path.toString + " does not refer to constant"))
            s
          case None =>
            // error was thrown above already 
            s
        }
      case OML(name, tp, df,_,_) => OML(name, tp.map(checkTerm(context, _)), df.map(checkTerm(context, _)))
      case OMV(name) =>
        if (!context.isDeclared(name))
          env.errorCont(InvalidObject(rootObj, s"variable $name is not declared"))
        s
      case ComplexTerm(c, subs, bound, args) =>
        val subsR = subs map { case Sub(v, t) => Sub(v, checkTerm(context, t)) }
        val boundR = checkContextTop(context, bound)
        val argsR = args map { a => checkTerm(context ++ bound, a) }
        env.pCont(c)
        ComplexTerm(c, subsR, boundR, argsR).from(s)
      case OMA(f, args) =>
        val fR = checkTerm(context, f)
        val argsR = args map { a => checkTerm(context, a) }
        OMA(fR, argsR).from(s)
      case OMBINDC(bin, con, args) =>
        val binR = checkTerm(context, bin)
        val conR = checkContextTop(context, con)
        val contextCon = context ++ conR
        val argsR = args map {a => checkTerm(contextCon, a)}
        OMBINDC(binR, conR, argsR).from(s)
      case OMATTR(arg, key, value) =>
        val argR = checkTerm(context, arg)
        checkTerm(context, key)
        val valueR = checkTerm(context, value)
        OMATTR(argR, key, valueR).from(s)
      case OMM(arg, morph) =>
        val (morphR, ComplexTheory(from), _) = checkMorphism(context, morph, None, Some(TheoryExp.empty))
        val argR = checkTerm(context ++ from, arg)
        OMM(argR, morphR).from(s)
      case l: OMLIT =>
        val synType = l.rt.synType
        checkTerm(context, synType)
        val rts = env.rules.get(classOf[uom.RealizedType])
        if (!rts.exists(_ == l.rt))
          env.errorCont(InvalidObject(l, "literal not in scope: " + l.toString + " (of type: " + l.synType + ", realized as: " + l.rt.semType + ")"))
        l
      // resolve type and parse unknown literal, return OMLIT
      case u @ UnknownOMLIT(v, synType) =>
        checkTerm(context, synType)
        controller.recognizeLiteral(env.rules, u).getOrElse {
          env.errorCont(InvalidObject(s, "unknown literal type " + synType))
          u
        }
      case OMFOREIGN(node) => s //TODO
      case OMSemiFormal(t) => s //TODO
      //case _ => s
    }
  }

  private def checkContextTop(context: Context, con: Context)(implicit env: ExtendedCheckingEnvironment): Context = {
    checkContext(context, con)(env, con)
  }
  /** Checks structural well-formedness of a context relative to a context.
    *
    * @param context the context of con
    * @param con     the context to check
    * @return the reconstructed context
    *         if context is valid, then this succeeds iff context ++ con is valid
    */
  private def checkContext(context: Context, con: Context)(implicit env: ExtendedCheckingEnvironment, rootObj: Obj): Context = {
    var ret = Context.empty
    con.foreach {vd =>
      val currentContext = context ++ ret
      val r = vd.feature match {
        case Some(StructureVarDecl.feature) =>
          // a variable that imports another theory
          // type must be a theory
          val tpR = vd.tp match {
            case Some(tp) => Some(checkTheory(None, currentContext, tp))
            case None =>
              env.errorCont(InvalidObject(vd, "type of structure variable must be given"))
              None
          }
          // definiens must be a partial substitution
          val dfR = (vd.tp, vd.df) match {
            case (Some(ComplexTheory(tpCon)), Some(ComplexMorphism(dfSubs))) =>
              val subsR = checkSubstitution(currentContext, dfSubs, tpCon, Context.empty, true)
              Some(ComplexMorphism(subsR))
            case _ =>
              env.errorCont(InvalidObject(vd, "definiens of theory-level variable must be a partial substitution, found " + vd.df))
              vd.df
          }
          List(vd.copy(tp = tpR, df = dfR))
        case Some(feat) =>
          //TODO DM elaboration at this point breaks overall design
          val sfOpt = extman.get(classOf[StructuralFeature], feat)
          if (sfOpt.isDefined) {
            sfOpt.get.checkInContext(currentContext,vd)
            vd :: sfOpt.get.elaborateInContext(currentContext,vd)
          } else List(vd)
        case None =>
          val vdR = vd map {x => checkTerm(currentContext, x)}
          List(vdR)
      }
      ret = ret ++ r
    }
    ret
  }

  /** Checks structural well-formedness of a substitution relative to a home theory.
    *
    * @param context      the current home context
    * @param subs         the substitution
    * @param from         the context declaring the variables to be substituted
    * @param to           the context in which the substituting terms live
    * @param allowPartial if true, do not require totality of subs
    * @return the reconstructed substitution
    */
  private def checkSubstitution(context: Context, subs: Substitution, from: Context, to: Context, allowPartial: Boolean)
                               (implicit env: ExtendedCheckingEnvironment): Substitution = {
    // collect all names to be mapped
    var fromDomain = from.getDomain
    var subsDomain = subs.asContext.getDomain
    /* intuitively: subs is partial/total substitution if
     *    fromDomain without defined elemnts subsumes/is equal to subsDomain
     * but we also allow names in fromDf or subsDf that are more specific than names in fromTp
     *    in that case, we replace the name in fromTp with its subdomain
     */
    /* invariants:
     *   fromDomain contains the declaring DomainElements that must still be mapped
     *   subsDomain contains the mapping DomainElements that have not been considered yet
     */
    while (subsDomain.nonEmpty) {
      val DomainElement(n, _, _) = subsDomain.head
      subsDomain = subsDomain.tail
      val matchingDomElems = fromDomain.filter { case de => n.dropPrefix(de.name).isDefined }.toList
      matchingDomElems match {
        case List(de@DomainElement(p, defined, subdomainOpt)) =>
          if (defined)
            env.errorCont(InvalidObject(subs, "found map for " + n + ", but domain already has definition for " + p))
          else {
            // n maps p: remove de from fromDomain
            if (p == n) {
              fromDomain = fromDomain.filter(_ != de)
            } else {
              subdomainOpt match {
                case Some((t, defs)) =>
                  // n maps a part of p, which imports tpath: replace de with domain elements for tpath
                  val tpathDomElems = TheoryExp.getDomain(t).map { d =>
                    d.copy(name = p / d.name)
                  }
                  fromDomain = fromDomain.filter(_ != de) ::: tpathDomElems
                  // the definitions of de act like the elements of subsDomain: add them
                  //TODO this does not cover the case where a Structure(VarDecl) has an assignment using a non-total ComplexMorhism
                  // for that, a DomainElement would need the whole substitution domain, not just the List[LocalName]
                  subsDomain :::= defs.map(ln => DomainElement(p / ln, true, None))
                case None =>
                  env.errorCont(InvalidObject(subs, "found map for " + n + ", but domain declares basic name " + p))
              }
            }
          }
        case Nil =>
          env.errorCont(InvalidObject(subs, "found map for " + n + ", but domain declares no matching name (may also happen if a substitution declares multiple maps for the same name)"))
        case ns =>
          env.errorCont(InvalidObject(subs, "found map for " + n + ", but domain declares " + ns.mkString(", ")))
      }
    }
    // subs is total if all names in fromDomain have been removed or are defined to begin with
    if (!allowPartial) {
      val left = fromDomain.filterNot(_.defined)
      if (left.nonEmpty) {
        val e = new InvalidObject(subs, "not total, missing cases for " + left.map(_.name).mkString(", ")) {
          override val excuse = Some(Level.Gap)
        }
        env.errorCont(e)
      }
    }
    // finally, check the individual maps in subs
    subs.map {
      case Sub(n, t) =>
        val tR = checkTerm(context ++ to, t)(env, subs)
        Sub(n, tR)
    }
  }

  /** special case of checkSubstitution for total substitutions into the empty context */
  private def checkSubstitutionRealization(context: Context, subs: Substitution, from: Context)(implicit env: ExtendedCheckingEnvironment) =
    checkSubstitution(context, subs, from, Context(), false)
}


/** if set, the element appears to be in scope but has not been checked yet */
object UncheckedElement extends BooleanClientProperty[StructuralElement](utils.mmt.baseURI / "clientProperties" / "controller" / "checked")
