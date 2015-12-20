package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import documents._
import info.kwarc.mmt.api.libraries.Closer
import modules._
import symbols._
import objects._
import patterns._
import ontology._
import utils._
import moc._
import frontend._

import utils.MyList.fromList
import objects.Conversions._

/**
 * A StructureChecker traverses structural elements and checks them structurally.
 * 
 * Deriving classes may override unitCont and reCont to customize the behavior.
 */
  //Note: Even Library.addUnchecked checks that declarations can only be added to atomic declared theories/views.
  //Therefore, the home modules are not checked here.
  //Body.add checks that no two declarations of the same name exist. Therefore, names are not checked here.
  //Names that are prefixes of other names in the same body are permitted.
  //  This works well for links, for theories it is questionable. Refusing declaration with non-primitive names might be forbidden.
  //  When retrieving, more specific entries overrule the more general ones.
class MMTStructureChecker(objectChecker: ObjectChecker) extends Checker(objectChecker) {
   val id = "mmt"
   private lazy val extman = controller.extman
   private implicit lazy val content = controller.globalLookup
   override val logPrefix = "checker"

   /** variant of CheckingEnvironment that carries around more structure */
   private class Environment(val ce: CheckingEnvironment, val rules: RuleSet, p: Path) {
      def pCont(q: Path) {ce.reCont(RefersTo(q, p))}
      def errorCont(e: Error) {ce.errorCont(e)}
   }
  
   /**
    * checks a StructuralElement
    * @param e the element to check
    */
   def apply(e : StructuralElement)(implicit ce: CheckingEnvironment) {
      check(Context(), e)
   }
   /**
    * @param context all variables and theories that may be used in e (including the theory in which is declared)
    * @param e the element to check
    */
   private def check(context: Context, e : StructuralElement)(implicit ce: CheckingEnvironment) {
      val rules = RuleSet.collectRules(controller, context)
      implicit val env = new Environment(ce, rules, e.path)
      val path = e.path
      log("checking " + path)
      e match {
         case d: Document =>
            d.getDeclarations foreach {i => check(context, i)}
         case r: NRef =>
            check(context, controller.get(r.target))
         case t: DeclaredTheory =>
            var contextMeta = context
            t.meta map {mt =>
              checkTheory(context, OMMOD(mt))
              contextMeta = contextMeta ++ mt
            }
            checkContext(contextMeta, t.parameters)
            val tDecls = t.getPrimitiveDeclarations
            tDecls foreach {d => d.status = Inactive}
            logGroup {
               t.getPrimitiveDeclarations foreach {d =>
                  check(context ++ t.getInnerContext, d)
                  d.status = Active
               }
            }
         case t: DefinedTheory =>
            val dfR = checkTheory(context, t.df)
            t.dfC.analyzed = dfR
         case v: DeclaredView =>
            checkTheory(context, v.from)
            checkTheory(context, v.to)
            logGroup {
               v.getPrimitiveDeclarations foreach {d => check(context ++ v.codomainAsContext, d)}
            }
         case v: DefinedView =>
            checkTheory(context, v.from)
            checkTheory(context, v.to)
            val (dfR, _, _) = checkMorphism(context, v.df, Some(v.from), Some(v.to))
            v.dfC.analyzed = dfR
         case nm: NestedModule =>
            check(context, nm.module)
         case s: DeclaredStructure =>
            checkTheory(context, s.from)
            s.getPrimitiveDeclarations foreach {d => check(context, d)}
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
                        if (! TheoryExp.equal(sOrg.from, s.from))
                           env.errorCont(InvalidElement(s, "import-assignment has bad domain: found " + s.from + " expected " + sOrg.from))
                     } else {
                        s.tpC.analyzed = s.from
                     }
                     checkRealization(context, s.df, s.from)
                  }
            }
         case c : Constant =>
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
            val linkInfo = linkOpt map {link =>
               val cOrg = content.getConstant(thy.path ? c.name)
               val expTypeOpt = cOrg.tp map {t => content.ApplyMorphs(t, link.toTerm)}
               val expDefOpt = cOrg.df map {t => content.ApplyMorphs(t, link.toTerm)}
               (cOrg, expTypeOpt, expDefOpt)
            }
            /* auxiliary function for structural checking of a term:
             * @return unknown variables, structural reconstruction, boolean result of latter
             */
            def prepareTerm(t: Term) : (Context, Term, Boolean) = {
               val (unknowns,tU) = parser.ObjectParser.splitOffUnknowns(t)
               val (tR, valid) = checkTermTop(context ++ unknowns, tU)
               (unknowns, tR, valid)
            }
            // = checking the type =
            // check that the type of c (if given) is in a universe
            getTermToCheck(c.tpC, "type") foreach {t =>
               val (unknowns, tR, valid) = prepareTerm(t)
               if (valid) {
                  val j = Inhabitable(Stack(Context()), tR)
                  objectChecker(CheckingUnit(c.path $ TypeComponent, context, unknowns, j), env.rules)
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
            getTermToCheck(c.dfC, "definiens") foreach {d =>
              val (unk, dR, valid) = prepareTerm(d)
              if (valid) {
                val cp = c.path $ DefComponent
                var performCheck = true
                val (unknowns,expTp) = c.tp match {
                  case Some(t) =>
                     (unk, t)
                  case None =>
                     if (d.isInstanceOf[OMID])
                        // no need to check atomic definiens without expected type
                        // slightly hacky trick to allow atomic definitions in the absence of a type system
                        performCheck = false
                     val tpVar = LocalName("") / "omitted_type"
                     (unk ++ VarDecl(tpVar,None,None,None), OMV(tpVar))
                }
                val j = Typing(Stack(Context()), dR, expTp, None)
                if (performCheck) {
                   objectChecker(CheckingUnit(cp, context, unknowns, j), env.rules)
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
         case rc: RuleConstant =>
         case _ =>
            //succeed for everything else but signal error
            logError("unchecked " + path)
      }
      // call the registered change listeners
      e match {
         case ce: ContentElement =>
            new Notify(controller.extman.get(classOf[ChangeListener]), report).onCheck(ce)
            //TODO set Checked status
         case _ =>
      }
   }
   
  /** determines which dimension of a term (parsed, analyzed, or neither) is checked */
  private def getTermToCheck(tc: TermContainer, dim: String) =
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

  /** checks whether a theory object is well-formed
    *  @param context the context relative to which m is checked
    *  @param t the theory
    *  @return the reconstructed theory
    */
   private def checkTheory(context : Context, t : Term)(implicit env: Environment) : Term = {
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
    *  domain and codomain are (tried to be) infered if omitted
    *  @param context the context relative to which m is checked
    *  @param m the morphism
    *  @param domOpt the domain, None if it is to be inferred
    *  @param codOpt the codomain
    *  @return the reconstructed morphism (with implicit morphisms inserted), its domain, and codomain
    */
   private def checkMorphism(context: Context, m: Term, domOpt: Option[Term], codOpt: Option[Term])(implicit env: Environment) : (Term, Term, Term) = {
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
        case OMDL(to, name) =>
           checkTheory(context, OMMOD(to))
           content.get(to ? name) match {
              case l: Structure =>
                 env.pCont(l.path)
                 (m, l.from, l.to)
              case _ =>
                 throw InvalidObject(m,"invalid morphism")
           }
        case OMIDENT(t) =>
           checkTheory(context, t)
           (m, t, t)
        case OMCOMP(ms) => ms.filter(_ != OMCOMP()) match {
           case Nil => (m, dom, cod)
           case hd::tl =>
              val (hdR, r, s1) = checkMorphism(context, hd, Some(dom), None)
              if (tl.isEmpty)
                 (hdR, r, s1)
              else {
                 val (tlR, s2, t) = checkMorphism(context, OMCOMP(tl), Some(s1), Some(cod))
                 content.getImplicit(s1,s2) match { 
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
   private def checkRealization(context: Context, r: Term, dom: Term)(implicit env: Environment) =
      checkMorphism(context, r, Some(dom), Some(TheoryExp.empty))

   /**
    * Checks structural well-formedness of a closed term relative to a home theory.
    * @param home the home theory
    * @param s the term
    * @return the reconstructed term
    */
   private def checkTermTop(context : Context, t : Term)(implicit env: Environment) : (Term, Boolean) = {
      env.ce.errorCont.mark
      val tR = checkTerm(context, t)
      (tR, ! env.ce.errorCont.hasNewErrors)
   }
   /**
    * Checks structural well-formedness of a term in context relative to a home theory.
    * @param home the home theory
    * @param s the term
    * @return the reconstructed term
    */
   private def checkTerm(context : Context, s : Term)(implicit env: Environment) : Term = {
      s match {
//         case OMID(GlobalName(h, IncludeStep(from) / ln)) =>
//            if (! content.imports(from, h))
//               throw Invalid(from + " is not imported into " + h + " in " + s)
//            checkTerm(home, context, OMID(from % ln).from(s))
         case OMS(path) =>
            val ceOpt = try {content.getO(path)}
                    catch {case e: GetError =>
                                env.errorCont(InvalidObject(s, "ill-formed constant reference"))
                    }
            ceOpt match {
               case Some(d : Declaration) =>
                  if (! content.hasImplicit(d.home, ComplexTheory(context)))
                     env.errorCont(InvalidObject(s, "constant " + d.path + " is not imported into current context " + context))
               case _ =>
                  env.errorCont(InvalidObject(s, path + " does not refer to constant"))
            }
            env.pCont(path)
            //wrap in implicit morphism?
            s
         case _:OMID =>
            null
         case OMV(name) =>
            if (! context.isDeclared(name))
               env.errorCont(InvalidObject(s, "variable is not declared"))
            s
         case ComplexTerm(c, subs, bound, args) =>
            val subsR = subs map {case Sub(v, t) => Sub(v, checkTerm(context, t))}
            val boundR = checkContext(context, bound)
            val argsR = args map {a => checkTerm(context++bound, a)}
            env.pCont(c)
            ComplexTerm(c, subsR, boundR, argsR).from(s)
         case OMA(f,args) =>
            val fR = checkTerm(context, f)
            val argsR = args map {a => checkTerm(context, a)}
            OMA(fR, argsR)
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
            val rts = env.rules.getByHead(classOf[uom.RealizedType], l.rt.head)
            if (! (rts contains l.rt))
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
                  env.errorCont(InvalidObject(s, "unknown literal type"))
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

   /**
    * Checks structural well-formedness of a context relative to a context.
    * @param context the context of con
    * @param con the context to check
    * @return the reconstructed context
    * if context is valid, then this succeeds iff context ++ con is valid
    */
   private def checkContext(context: Context, con: Context)(implicit env: Environment) : Context = {
      con.mapVarDecls {case (c, vd) =>
           val currentContext = context ++ c
           vd match {
              case StructureVarDecl(name, tp, dfOpt) =>
                 // a variable that imports another theory
                 // type must be a theory
                 checkTheory(currentContext, tp) 
                 // definiens must be a partial substitution
                 val dfR = dfOpt map {df => (tp,df) match {
                    case (ComplexTheory(tpCon), ComplexMorphism(dfSubs)) =>
                       val subsR = checkSubstitution(currentContext, dfSubs, tpCon, Context(), true)
                       ComplexMorphism(subsR)
                    case _ =>
                       env.errorCont(InvalidObject(vd, "definiens of theory-level variable must be a partial substitution, found " + df))
                       df
                 }}
                 StructureVarDecl(name, tp, dfR)
              case VarDecl(name, tp, df, not) =>
                 // a normal variable
                 val tpR = tp.map(x => checkTerm(currentContext, x))
                 val dfR = df.map(x => checkTerm(currentContext, x))
                 val vdR = VarDecl(name, tpR, dfR, not)
                 vdR.copyFrom(vd)
                 vdR
           }
      }
   }
   
   /**
    * Checks structural well-formedness of a substitution relative to a home theory.
    * @param home the home theory
    * @param context the current home context
    * @param subs the substitution
    * @param from the context declaring the variables to be substituted
    * @param to the context in which the substituting terms live
    * @param allowPartial if true, do not require totality of subs
    * @return the reconstructed substitution
    */
   private def checkSubstitution(context: Context, subs: Substitution, from: Context, to: Context, allowPartial: Boolean)(implicit env: Environment) : Substitution = {
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
      while (! subsDomain.isEmpty) {
         val DomainElement(n, _, _) = subsDomain.head
         subsDomain = subsDomain.tail
         val matchingDomElems = fromDomain.filter {case de => n.dropPrefix(de.name).isDefined}.toList
         matchingDomElems match {
            case List(de @ DomainElement(p, defined, subdomainOpt)) =>
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
                           val tpathDomElems = TheoryExp.getDomain(t).map {d =>
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
      if (! allowPartial) {
         val left = fromDomain.filterNot(_.defined) 
         if (! left.isEmpty)
            env.errorCont(InvalidObject(subs, "not total, missing cases for " + left.map(_.name).mkString(", ")))
      }
      // finally, check the individual maps in subs
      subs.map {
         case Sub(n, t) =>
            val tR = checkTerm(context ++ to,t)
            Sub(n, tR)
      }
   }
   /** 
    *  special case of checkSubstitution for total substitutions into the empty context
    */
   private def checkSubstitutionRealization(context: Context, subs: Substitution, from: Context)(implicit env: Environment) =
      checkSubstitution(context, subs, from, Context(), false)
}
