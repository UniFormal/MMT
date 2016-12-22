package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._

import documents._
import modules._
import symbols._
import objects._
import ontology._
import parser._
import frontend._
import opaque._

import objects.Conversions._

/** variant of CheckingEnvironment that carries around more structure */
class ExtendedCheckingEnvironment(val ce: CheckingEnvironment, val objectChecker: ObjectChecker, val rules: RuleSet, p: Path) {
  def pCont(q: Path) {
    ce.reCont(RefersTo(q, p))
  }

  def errorCont(e: Error) {
    ce.errorCont(e)
  }
}


/** A StructureChecker traverses structural elements and checks them structurally.
  *
  * Deriving classes may override unitCont and reCont to customize the behavior.
  */
class MMTStructureChecker(objectChecker: ObjectChecker) extends Checker(objectChecker) {
  val id = "mmt"
  private lazy val extman = controller.extman
  private implicit lazy val content = controller.globalLookup
  override val logPrefix = "structure-checker"

  /**
    * checks a StructuralElement
    *
    * @param e the element to check
    */
  def apply(e: StructuralElement)(implicit ce: CheckingEnvironment) {
    val cont = getContext(e)
    check(cont, e)
  }

  /** computes the context in which an element must be checked
   *  during top-down traversal, this method allows checking to start in the middle
   */
  private def getContext(e: StructuralElement): Context = {
    lazy val parent = controller.get(e.parent)
    e match {
      case d: NarrativeElement => d.parentOpt match {
        case None => d match {
          case d: Document => d.contentAncestor match {
            case None => Context.empty
            case Some(m) => getInnerContext(m)
          }
          case _ => Context.empty
        }
        case Some(p) => getContext(parent)
      }
      case m: Module => m.superModule match {
        case None => Context.empty
        case Some(smP) => controller.get(smP) match {
          case sm: DeclaredModule => getInnerContext(sm)
          case sm: DefinedModule => getContext(m) // should never occur
        }
      }
      case d: Declaration => parent match {
        case m: DeclaredModule => getInnerContext(m)
        case m: DefinedModule => getContext(m) // should never occur
      }
    }
  }
  /** @return the context in which the body of ContainerElement is checked */
  private def getInnerContext(e: ContainerElement[_]) = makeInnerContext(getContext(e), e)
  
  /**
    * @param context all variables and theories that may be used in e (including the theory in which is declared)
    * @param e       the element to check
    */
  private def check(context: Context, e: StructuralElement)(implicit ce: CheckingEnvironment) {
    val rules = RuleSet.collectRules(controller, context)
    implicit val env = new ExtendedCheckingEnvironment(ce, objectChecker, rules, e.path)
    val path = e.path
    log("checking " + path + " using the following rules: " + rules.toString)
    e match {
      //TODO merge this case with the generic case for ContainerElements
      //the extended treatment is probably needed for all ContainerElements anyway
      case t: DeclaredTheory =>
        checkElementBegin(t,context)
        val contextI = checkContext(Context.empty,makeInnerContext(context, t))
        // content structure
        val tDecls = t.getPrimitiveDeclarations
        // mark all children as unchecked
        tDecls foreach { d =>
          UncheckedElement.set(d)
        }
        // check each child and mark it as checked (independent of whether there are errors)
        logGroup {
          tDecls foreach { d =>
            check(contextI, d)
            UncheckedElement.erase(d)
          }
        }
        checkElementEnd(t,context)
      case c: ContainerElement[_] =>
        checkElementBegin(c, context)
        val contextI = makeInnerContext(context, c)
        logGroup {
          c.getPrimitiveDeclarations foreach {
            i => check(contextI, i)
          }
        }
        checkElementEnd(c, context)
      case oe: OpaqueElement =>
        controller.extman.get(classOf[OpaqueChecker], oe.format) match {
          case None => env.errorCont(InvalidElement(oe, "no checker found for format " + oe.format))
          case Some(oc) =>
            oc.check(objectChecker, context, rules, oe)
        }
      //TODO check all declarations, components? currently done in each OpaqueChecker
      case r: NRef =>
        check(context, controller.get(r.target))
      case dd: DerivedDeclaration =>
        val sfOpt = extman.get(classOf[StructuralFeature], dd.feature)
        sfOpt match {
          case None =>
            env.errorCont(InvalidElement(dd, s"structural feature '${dd.feature}' not registered"))
          case Some(sf) =>
            dd.tpC.get foreach {tp =>
              val tpR = checkTerm(context, tp)
              // not using tpR here because source references are gone
            }
            val conInner = sf.getInnerContext(dd)
            check(context ++ conInner, dd.module)
            sf.check(dd)
        }
      case nm: NestedModule =>
        check(context, nm.module)
      case t: DefinedTheory =>
        val dfR = checkTheory(context, t.df)
        t.dfC.analyzed = dfR
      case v: DefinedView =>
        checkTheory(context, v.from)
        checkTheory(context, v.to)
        val (dfR, _, _) = checkMorphism(context, v.df, Some(v.from), Some(v.to))
        v.dfC.analyzed = dfR
      case s: DefinedStructure =>
        val (thy, linkOpt) = content.getDomain(s)
        linkOpt match {
          case None =>
            // declaration in a theory
            checkTheory(context, s.from)
            checkRealization(context, s.df, s.from)
          case Some(link) =>
            // assignment in a link
            if (s.isInclude) {
              val (dfR, dom, _) = checkMorphism(context, s.df, Some(s.from), Some(link.to))
              dom match {
                case OMMOD(p) =>
                  //TODO check if p is included into link.from
                  s.dfC.analyzed = dfR
                case _ =>
                  env.errorCont(InvalidElement(s, "cannot infer atomic domain of assignment"))
              }
            } else {
              val sOrg = content.getStructure(thy.path ? s.name)
              // infer and set s.from if not provided
              if (s.tpC.isDefined) {
                if (!TheoryExp.equal(sOrg.from, s.from))
                  env.errorCont(InvalidElement(s, "import-assignment has bad domain: found " + s.from + " expected " + sOrg.from))
              } else {
                s.tpC.analyzed = s.from
              }
              checkRealization(context, s.df, s.from)
            }
        }
      case rc: RuleConstant =>
        val _ = checkTerm(context, rc.tp)
        if (rc.df.isEmpty) {
          if (ParseResult.fromTerm(rc.tp).isPlainTerm) {
             new RuleConstantInterpreter(controller).createRule(rc)
          } else {
             env.errorCont(InvalidElement(rc, "type of rule constant not fully checked"))
          }
        }
      case c: Constant =>
        // determine whether we are in a theory or a link
        //   (thy,None): c is constant in theory thy
        //   (thy, Some(l)): c is assignment in link l from thy
        val (thy, linkOpt) = content.getDomain(c)
        // the home theory of the components of c
        val scope = linkOpt match {
          case None => OMMOD(thy.path)
          case Some(link) => link.to
        }
        // if link: the source constant and its translated components
        val linkInfo = linkOpt map { link =>
          val cOrg = content.getConstant(thy.path ? c.name)
          val expTypeOpt = cOrg.tp map { t => content.ApplyMorphs(t, link.toTerm) }
          val expDefOpt = cOrg.df map { t => content.ApplyMorphs(t, link.toTerm) }
          (cOrg, expTypeOpt, expDefOpt)
        }
        /* auxiliary function for structural checking of a term:
         * @return unknown variables, structural reconstruction, boolean result of latter
         */
        def prepareTerm(t: Term): (ParseResult, Boolean) = {
          val pr = ParseResult.fromTerm(t)
          val (tR, valid) = checkTermTop(context ++ pr.unknown ++ pr.free, pr.term)
          (ParseResult(pr.unknown, pr.free, tR), valid)
        }
        // = checking the type =
        // check that the type of c (if given) is in a universe
        getTermToCheck(c.tpC, "type") foreach { t =>
          val (pr, valid) = prepareTerm(t)
          if (valid) {
            val j = Inhabitable(Stack(pr.free), pr.term)
            objectChecker(CheckingUnit(Some(c.path $ TypeComponent), context, pr.unknown, j), env.rules)
          }
        }
        // == additional check in a link ==
        // translate the type of cOrg
        linkInfo foreach {
          case (_, Some(expType), _) => c.tp match {
            case Some(tp) =>
            // c already has a type: keep it, but check compatibility
            //val j = Subtype(Stack(scope, parR), tp, expType)
            //objectChecker(ValidationUnit(c.path $ TypeComponent, Context(), j))
            case None =>
              // c has no type: use the translated type
              c.tpC.analyzed = expType
          }
          case _ => // cOrg has no type, nothing to do
        }
        // = checking the definiens =
        // check that the definiens of c (if given) type-checks against the type of c (if given)
        val tpVar = CheckingUnit.unknownType
        getTermToCheck(c.dfC, "definiens") foreach { d =>
          val (pr, valid) = prepareTerm(d)
          if (valid) {
            val cp = c.path $ DefComponent
            var performCheck = true
            val (unknowns, expTp, inferType) = c.tp match {
              case Some(t) =>
                (pr.unknown, t, false)
              case None =>
                if (d.isInstanceOf[OMID])
                // no need to check atomic definiens without expected type
                // slightly hacky trick to allow atomic definitions in the absence of a type system
                   performCheck = false
                (pr.unknown ++ VarDecl(tpVar, None, None, None), OMV(tpVar), true)
            }
            val j = Typing(Stack(pr.free), pr.term, expTp, None)
            if (performCheck) {
              val cr = objectChecker(CheckingUnit(Some(cp), context, unknowns, j), env.rules)
              if (inferType && cr.solved) {
                // if no expected type was known but the type could be inferred, add it
                cr.solution.foreach { sol =>
                  val tp = sol(tpVar).df
                  c.tpC.analyzed = tp
                }
              }
            }
          }
        }
        // == additional check in a link ==
        // translate the definiens of cOrg
        linkInfo foreach {
          case (_, _, Some(expDef)) => c.df match {
            case Some(df) =>
            // c already has a definiens: keep it, but check equality
            //val j = Equality(Stack(scope, parR), df, expDef, None)
            //objectChecker(ValidationUnit(c.path $ DefComponent, Context(), j))
            case None =>
              // c has no definiens: use the translated definiens
              c.dfC.analyzed = expDef
          }
          case _ => // cOrg has no definiens, nothing to do
        }
      case _ =>
        //succeed for everything else but signal error
        logError("unchecked " + path)
    }
    new Notify(controller.extman.get(classOf[ChangeListener]), report).onCheck(e)
  }

  /** determines which dimension of a term (parsed, analyzed, or neither) is checked */
  private def getTermToCheck(tc: TermContainer, dim: String) = {
    if (tc.parsed.isDefined && tc.analyzed.isDefined) {
      if (tc.isAnalyzedDirty) {
        log(s"re-checking dirty $dim")
        tc.parsed
      } else {
        log(s"skipping $dim (not dirty)")
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
  
  /** computes the context in which the body of ContainerElement is checked */
  private def makeInnerContext(context: Context, e: ContainerElement[_]) = e match {
    case d: Document => context
    case m: DeclaredModule => context ++ m.getInnerContext
    case s: DeclaredStructure => context
  }
  
  /** auxiliary method of check */
  private def checkElementBegin(e : ContainerElement[_], context : Context)(implicit env: ExtendedCheckingEnvironment) {
    val rules = env.rules
    implicit val ce = env.ce
    val path = e.path
    e match {
      case d: Document =>
      case t: DeclaredTheory =>
        var contextMeta = context
        t.meta foreach { mt =>
          checkTheory(context, OMMOD(mt))
          contextMeta = contextMeta ++ mt
        }
        checkContext(contextMeta, t.parameters)
      case v: DeclaredView =>
        checkTheory(context, v.from)
        checkTheory(context, v.to)
      case s: DeclaredStructure =>
        checkTheory(context, s.from)
      case _ =>
        //succeed for everything else but signal error
        logError("unchecked " + path)
    }
  }

  /** auxiliary method of check */
  private def checkElementEnd(e: ContainerElement[_], context: Context)(implicit env: ExtendedCheckingEnvironment) {
    val rules = env.rules
    implicit val ce = env.ce
    val path = e.path
    e match {
      case d: Document =>
      case t: DeclaredTheory =>
        val contextI = makeInnerContext(context, t)
        // check all the narrative structure (at the end to allow forward references)
        //TODO currently this is called on a NestedModule before the rest of the parent is checked
        def doDoc(ne: NarrativeElement) {
          ne match {
            case doc: Document => doc.getDeclarations foreach doDoc
            case r: NRef =>
            case oe: OpaqueElement => check(contextI, oe)
          }
        }
        doDoc(t.asDocument)
      case v: DeclaredView =>
      case s: DeclaredStructure =>
      case _ =>
        //succeed for everything else but signal error
        logError("unchecked " + path)
    }
  }

  // *****
  
  /** checks whether a theory object is well-formed
    *
    * @param context the context relative to which m is checked
    * @param t       the theory
    * @return the reconstructed theory
    */
  private def checkTheory(context: Context, t: Term)(implicit env: ExtendedCheckingEnvironment): Term = {
    t match {
      case OMPMOD(p, args) =>
        val thy = try {
          controller.globalLookup.get(p)
        } catch {
          case e: Error =>
            env.errorCont(InvalidObject(t, "unknown identifier: " + p.toPath).setCausedBy(e))
            return t
        }
        thy match {
          case thy: Theory =>
            val pars = thy.parameters
            env.pCont(p)
            val subs = (pars / args).getOrElse {
              env.errorCont(InvalidObject(t, "bad number of arguments, expected " + pars.length))
              context.id
            }
            checkSubstitution(context, subs, pars, Context(), false)
          case _ =>
            env.errorCont(InvalidObject(t, "not a theory identifier" + p.toPath))
        }
        t
      case ComplexTheory(body) =>
        val bodyR = checkContext(context, body)
        ComplexTheory(bodyR)
      case _ =>
        env.errorCont(InvalidObject(t, "not a valid theory"))
        t
    }
  }

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
        val l = controller.globalLookup.getLink(p)
        env.pCont(p)
        (m, l.from, l.to)
      case OMS(p) =>
        checkTheory(context, OMMOD(p.module))
        content.get(p) match {
          case l: Structure =>
            env.pCont(l.path)
            (m, l.from, l.to)
          case _ =>
            throw InvalidObject(m, "invalid morphism")
        }
      case OMIDENT(t) =>
        checkTheory(context, t)
        (m, t, t)
      case OMCOMP(ms) => ms.filter(_ != OMCOMP()) match {
        case Nil => (m, dom, cod)
        case hd :: tl =>
          val (hdR, r, s1) = checkMorphism(context, hd, Some(dom), None)
          if (tl.isEmpty)
            (hdR, r, s1)
          else {
            val (tlR, s2, t) = checkMorphism(context, OMCOMP(tl), Some(s1), Some(cod))
            content.getImplicit(s1, s2) match {
              case Some(l) =>
                (OMCOMP(hdR, l, tlR), r, t)
              case None =>
                throw InvalidObject(m, "ill-formed morphism: " + hd + " cannot be composed with " + tl)
            }
          }
      }
      case ComplexMorphism(body) =>
        // get domain and codomain as contexts
        val bodyR = checkSubstitution(context, body, domC, codC, false)
        (ComplexMorphism(bodyR), dom, cod)
    }
    lazy val domIC = ComplexTheory.unapply(domI) getOrElse {
      throw InvalidObject(m, "domain is not a theory")
    }
    val implDom = content.getImplicit(dom, ComplexTheory(context ++ domIC))
    val implCod = content.getImplicit(codI, ComplexTheory(context ++ codC))
    val mRR = (implDom, implCod) match {
      case (Some(l0), Some(l1)) => OMCOMP(l0, mR, l1)
      case _ =>
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
    * @return the reconstructed term
    */
  private def checkTermTop(context: Context, t: Term)(implicit env: ExtendedCheckingEnvironment): (Term, Boolean) = {
    env.ce.errorCont.mark
    val tR = checkTerm(context, t)
    (tR, env.ce.errorCont.noErrorsAdded)
  }

  /**
    * Checks structural well-formedness of a term in context relative to a home theory.
    *
    * @param context the context
    * @param s       the term
    * @return the reconstructed term
    */
  //TODO make more reusable (maybe by moving to RuleBasedChecker?)
  private def checkTerm(context: Context, s: Term)(implicit env: ExtendedCheckingEnvironment): Term = {
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
        val ceOpt = content.getO(path)
        if (ceOpt.isEmpty) {
           env.errorCont(InvalidObject(s, "ill-formed constant reference " + path))
        }
        ceOpt match {
          case Some(d: Declaration) =>
            if (!content.hasImplicit(d.home, ComplexTheory(context)))
              env.errorCont(InvalidObject(s, "constant " + d.path + " is not imported into current context " + context))
            if (UncheckedElement.is(d))
              env.errorCont(InvalidObject(s, "constant " + d.path + " is used before being declared " + context))
          case _ =>
            env.errorCont(InvalidObject(s, path + " does not refer to constant"))
        }
        env.pCont(path)
        //wrap in implicit morphism?
        s
      case OML(name, tp, df) => OML(name, tp.map(checkTerm(context, _)), df.map(checkTerm(context, _)))
      case OMV(name) =>
        if (!context.isDeclared(name))
          env.errorCont(InvalidObject(s, "variable is not declared"))
        s
      case ComplexTerm(c, subs, bound, args) =>
        val subsR = subs map { case Sub(v, t) => Sub(v, checkTerm(context, t)) }
        val boundR = checkContext(context, bound)
        val argsR = args map { a => checkTerm(context ++ bound, a) }
        env.pCont(c)
        ComplexTerm(c, subsR, boundR, argsR).from(s)
      case OMA(f, args) =>
        val fR = checkTerm(context, f)
        val argsR = args map { a => checkTerm(context, a) }
        OMA(fR, argsR)
      case OMBINDC(bin, con, args) =>
        val binR = checkTerm(context, bin)
        val conR = checkContext(context, con)
        val argsR = args map {a => checkTerm(context, a)}
        OMBINDC(binR, conR, argsR)
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
        if (!(rts contains l.rt))
          env.errorCont(InvalidObject(s, "literal not in scope: " + l.toString))
        l
      // resolve type and parse unknown literal, return OMLIT
      case UnknownOMLIT(v, synType) =>
        checkTerm(context, synType)
        val rts = env.rules.get(classOf[uom.RealizedType]).filter(_.synType == synType).toList
        rts match {
          case rule :: Nil =>
            rule.parse(v).from(s)
          case Nil =>
            env.errorCont(InvalidObject(s, "unknown literal type " + synType))
            s
          case _ =>
            env.errorCont(InvalidObject(s, "multiple literal rules for the same type"))
            s
        }
      case OMFOREIGN(node) => s //TODO
      case OMSemiFormal(t) => s //TODO
      //case _ => s
    }
  }

  /** Checks structural well-formedness of a context relative to a context.
    *
    * @param context the context of con
    * @param con     the context to check
    * @return the reconstructed context
    *         if context is valid, then this succeeds iff context ++ con is valid
    */
  private def checkContext(context: Context, con: Context)(implicit env: ExtendedCheckingEnvironment): Context = {
    con.toList.foldLeft(Context.empty)((c, vd) => {
      val currentContext = context ++ c
      c ++ (vd match {
        case dv @ DerivedVarDecl(name,feat,_,args) =>
          val sfOpt = extman.get(classOf[StructuralFeature], feat)
          if (sfOpt.isDefined) {
            sfOpt.get.checkInContext(currentContext,dv)
            vd :: sfOpt.get.elaborateInContext(currentContext,dv)
          } else List(vd)
        case StructureVarDecl(name, tp, dfOpt) =>
          // a variable that imports another theory
          // type must be a theory
          checkTheory(currentContext, tp)
          // definiens must be a partial substitution
          val dfR = dfOpt map { df => (tp, df) match {
            case (ComplexTheory(tpCon), ComplexMorphism(dfSubs)) =>
              val subsR = checkSubstitution(currentContext, dfSubs, tpCon, Context(), true)
              ComplexMorphism(subsR)
            case _ =>
              env.errorCont(InvalidObject(vd, "definiens of theory-level variable must be a partial substitution, found " + df))
              df
          }
          }
          List(StructureVarDecl(name, tp, dfR))
        case VarDecl(name, tp, df, not) =>
          // a normal variable
          val tpR = tp.map(x => checkTerm(currentContext, x))
          val dfR = df.map(x => checkTerm(currentContext, x))
          val vdR = VarDecl(name, tpR, dfR, not)
          vdR.copyFrom(vd)
          List(vdR)
      })
    })
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
  private def checkSubstitution(context: Context, subs: Substitution, from: Context, to: Context, allowPartial: Boolean)(implicit env: ExtendedCheckingEnvironment): Substitution = {
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
      if (left.nonEmpty)
        env.errorCont(InvalidObject(subs, "not total, missing cases for " + left.map(_.name).mkString(", ")))
    }
    // finally, check the individual maps in subs
    subs.map {
      case Sub(n, t) =>
        val tR = checkTerm(context ++ to, t)
        Sub(n, tR)
    }
  }

  /** special case of checkSubstitution for total substitutions into the empty context */
  private def checkSubstitutionRealization(context: Context, subs: Substitution, from: Context)(implicit env: ExtendedCheckingEnvironment) =
    checkSubstitution(context, subs, from, Context(), false)
}


/** if set, the element appears to be in scope but has not been checked yet */
object UncheckedElement extends BooleanClientProperty[StructuralElement](utils.mmt.baseURI / "clientProperties" / "controller" / "checked")
