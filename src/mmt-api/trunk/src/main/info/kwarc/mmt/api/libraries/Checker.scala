package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import documents._
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
class StructureChecker(controller: Controller) extends Logger {
   private val extman = controller.extman
   private implicit lazy val content = controller.globalLookup
   val report = controller.report
   val logPrefix = "checker"

  /** called on every validation unit, empty by default */
  def unitCont(vu: ValidationUnit) {}
  /** called on every relational element produced, empty by default */
  def reCont(re: RelationalElement) {}

  def errorCont(er: Invalid) {
     errors ::= er
  }
  private var errors: List[Invalid] = Nil
  
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

  /** checks a StructuralElement, given by its URI */
  def apply(p: Path): List[Invalid] = {
      apply(controller.get(p))
  }

   /** checks a StructuralElement
    * @param e the element to check
    */
   def apply(e : StructuralElement): List[Invalid] = {
      errors = Nil
      check(e)
      errors.reverse
   }
   private def check(p: Path) {
      check(controller.get(p))
   }
   private def check(e : StructuralElement) {
      val path = e.path
      log("checking " + path)
      implicit val pCont = (p: Path) => {reCont(RefersTo(path, p))}
      e match {
         case d: Document => d.getLocalItems foreach check
         case r: XRef => if (r.isGenerated) check(r.target)
         case t: DeclaredTheory =>
            t.meta map {mt =>
              checkTopTheory(t, OMMOD(mt))
            }
            logGroup {
               t.getPrimitiveDeclarations foreach check
            }
         case t: DefinedTheory =>
            val dfR = checkTopTheory(t, t.df)
            t.dfC.analyzed = dfR
         case v: DeclaredView =>
            checkTopTheory(v, v.from)
            checkTopTheory(v, v.to)
            logGroup {
               v.getPrimitiveDeclarations foreach check
            }
         case v: DefinedView =>
            checkTopTheory(v, v.from)
            checkTopTheory(v, v.to)
            val (dfR, _, _) = checkMorphism(v.superModule, Context(), v.df, Some(v.from), Some(v.to))
            v.dfC.analyzed = dfR
         case nm: NestedModule => check(nm.module)
         case s: DeclaredStructure =>
            checkTheory(s.home, Context(), s.from)
            s.getPrimitiveDeclarations foreach check
         case s: DefinedStructure =>
            val (thy, linkOpt) = content.getDomain(s)
            linkOpt match {
               case None =>
                  // declaration in a theory
                  checkTopTheory(thy, s.from)
                  checkRealization(s.home, Context(), s.df, s.from)
               case Some(link) =>
                  // assignment in a link
                  if (s.isAnonymous) {
                     val (dfR, dom, _) = checkMorphism(thy.toTerm, Context(), s.df, Some(s.from), Some(link.to))
                     dom match {
                        case OMMOD(p) =>
                           //TODO check if p is included into link.from
                           s.dfC.analyzed = dfR
                        case _ =>
                           errorCont(InvalidElement(s, "cannot infer atomic domain of assignment"))
                     }
                  } else {
                     val sOrg = content.getStructure(thy.path ? s.name)
                     if (sOrg.fromPath != s.from)
                        errorCont(InvalidElement(s, "import-assignment has bad domain: found " + s.from + " expected " + sOrg.from)) 
                     checkRealization(s.home, Context(), s.df, s.from)
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
               val (unknowns,tU) = parser.AbstractObjectParser.splitOffUnknowns(t)
               val (tR, valid) = checkTermTop(scope, c.parameters ++ unknowns, tU)
               (unknowns, tR, valid)
            }
            val parR = checkContext(c.home, Context(), c.parameters)
            //TODO reconstruction in parameters
            // = checking the type =
            // check that the type of c (if given) is in a universe
            getTermToCheck(c.tpC, "type") foreach {t =>
               val (unknowns, tR, valid) = prepareTerm(t)
               if (valid) {
                  val j = Inhabitable(Stack(scope, parR), tR)
                  unitCont(ValidationUnit(c.path $ TypeComponent, unknowns, j))
               }
            }
            // == additional check in a link ==
            // translate the type of cOrg
            linkInfo foreach {
               case (_, Some(expType), _) => c.tp match {
                  case Some(tp) =>
                     // c already has a type: keep it, but check compatibility
                     //val j = Subtype(Stack(scope, parR), tp, expType)
                     //unitCont(ValidationUnit(c.path $ TypeComponent, Context(), j))
                  case None =>
                     // c has no type: use the translated type
                     c.tpC.analyzed = expType
               }
               case _ => // cOrg has no type, nothing to do
            }
            // = checking the definiens =
            // check that the definiens of c (if given) type-checks against the type of c (if given)
            getTermToCheck(c.dfC, "definiens") foreach {d =>
               val (unknowns, dR, valid) = prepareTerm(d)
               if (valid) {
                  c.tp foreach {tp =>
                      val j = Typing(Stack(scope, parR), dR, tp, None)
                      unitCont(ValidationUnit(c.path $ DefComponent, unknowns, j))
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
                     //unitCont(ValidationUnit(c.path $ DefComponent, Context(), j))
                  case None =>
                     // c has no definiens: use the translated definiens
                     c.dfC.analyzed = expDef
               }
               case _ => // cOrg has no definiens, nothing to do
            }
         case p : Pattern =>
            checkContext(p.home, Context(), p.params)
            checkContext(p.home, p.params, p.body)
         case i : Instance => 
            val ptOpt = try {
               Some(content.getPattern(i.pattern))
            } catch {
               case e: GetError =>
                  errorCont(InvalidElement(i,"invalid pattern: " + i.pattern,e))
                  None
            }
            ptOpt foreach {pt =>
               checkSubstitutionRealization(i.home, Context(), pt.getSubstitution(i), pt.params)}
            //TODO mihnea's instances already refer to their elaboration in their matches
         case _ =>
            //succeed for everything else
            logError("unchecked " + path)
      }
      // call the registered change listeners
      e match {
         case ce: ContentElement =>
            controller.extman.changeListeners foreach {l => l.onCheck(ce)}
         case _ =>
      }
   }
   
  /** checks whether a theory object is well-formed
    *  @param t the theory
    *  @return the reconstructed theory
    */
   def checkTheory(home : Term, context : Context, t : Term)(implicit pCont: Path => Unit) : Term = {
     t match {
        case OMMOD(p) =>
           controller.globalLookup.getO(p) match {
              case Some(thy: Theory) =>
                 pCont(p)
              case Some(_) =>
                 errorCont(InvalidObject(t, "not a theory identifier" + p.toPath))
              case None =>
                 errorCont(InvalidObject(t, "unknown identifier: " + p.toPath))
           }
           t
        case ComplexTheory(body) =>
           val bodyR = checkContext(home, context, body)
           ComplexTheory(bodyR)
        case _ =>
           errorCont(InvalidObject(t, "not a valid theory"))
           t
      }
   }
   /** abbreviation for checkTheory where t occurs at the toplevel of module */
   private def checkTopTheory(module: Module, t: Term)(implicit pCont: Path => Unit): Term = {
      checkTheory(module.superModule, Context(), t)
   } 
   
   /** checks whether a morphism object is well-formed relative to a domain and a codomain
    *  domain and codomain are (tried to be) infered if omitted
    *  @param home the theory relative to which m is checked
    *  @param context the context relative to which m is checked
    *  @param m the morphism
    *  @param domOpt the domain, None if it is to be inferred
    *  @param codOpt the codomain
    *  @return the reconstructed morphism (with implicit morphisms inserted), its domain, and codomain
    */
   def checkMorphism(home: Term, context: Context, m: Term, domOpt: Option[Term], codOpt: Option[Term])(implicit pCont: Path => Unit) : (Term, Term, Term) = {
      val dom = domOpt orElse Morph.domain(m)(content) getOrElse {
         throw InvalidObject(m, "cannot infer domain of morphism")
      }
      val cod = codOpt orElse Morph.codomain(m)(content) getOrElse {
         throw InvalidObject(m, "cannot infer codomain of morphism")
      }
      val (mR, domI, codI) = m match {
        case OMMOD(p) =>
           val l = controller.globalLookup.getLink(p)
           pCont(p)
           (m, l.from, l.to)
        case OMDL(to, name) =>
           checkTheory(home, context, to)
           content.get(to % name) match {
              case l: Structure =>
                 pCont(l.path)
                 (m, l.from, l.to)
              case _ =>
                 throw InvalidObject(m,"invalid morphism")
           }
        case OMIDENT(t) =>
           checkTheory(home, context, t)
           (m, t, t)
        case OMCOMP(ms) => ms.filter(_ != OMCOMP()) match {
           case Nil => (m, dom, cod)
           case hd::tl =>
              val (hdR, r, s1) = checkMorphism(home, context, hd, Some(dom), None)
              if (tl.isEmpty)
                 (hdR, r, s1)
              else {
                 val (tlR, s2, t) = checkMorphism(home, context, OMCOMP(tl), Some(s1), Some(cod))
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
           val from = ComplexTheory.unapply(dom) getOrElse {
              throw InvalidObject(m, "domain is not a theory")
           }
           val to = ComplexTheory.unapply(cod) getOrElse {
              throw InvalidObject(m, "codomain is not a theory")
           }
           val bodyR = checkSubstitution(home, context, body, from, to, false)
           (ComplexMorphism(bodyR), dom, cod)
      }
      val implDom = content.getImplicit(dom, domI)
      val implCod = content.getImplicit(codI, cod)
      val mRR = (implDom, implCod) match {
         case (Some(l0), Some(l1)) => OMCOMP(l0, mR, l1)
         case _ =>
           errorCont(InvalidObject(m, "ill-formed morphism: expected " + dom + " -> " + cod + ", found " + domI + " -> " + codI))
           m
      }
      (mRR, dom, cod)
   }
   
   /**
    * special case of checkMorphism, where the morphism is checked relative to its codomain
    */
   def checkRealization(home: Term, context: Context, r: Term, dom: Term)(implicit pCont: Path => Unit) =
      checkMorphism(home, context, r, Some(dom), Some(home))

   /**
    * Checks structural well-formedness of a closed term relative to a home theory.
    * @param home the home theory
    * @param s the term
    * @return the reconstructed term
    */
   def checkTermTop(home: Term, context : Context, t : Term)(implicit pCont: Path => Unit) : (Term, Boolean) = {
      val n = errors.length
      val tR = checkTerm(home, context, t)
      if (errors.length == n)
         (tR, true) // no new errors
      else
         (tR, false)      
   }
   /**
    * Checks structural well-formedness of a term in context relative to a home theory.
    * @param home the home theory
    * @param s the term
    * @return the reconstructed term
    */
   private def checkTerm(home : Term, context : Context, s : Term)(implicit pCont: Path => Unit) : Term = {
      s match {
//         case OMID(GlobalName(h, IncludeStep(from) / ln)) =>
//            if (! content.imports(from, h))
//               throw Invalid(from + " is not imported into " + h + " in " + s)
//            checkTerm(home, context, OMID(from % ln).from(s))
         case OMS(path) =>
            val ceOpt = try {content.getO(path)}
                    catch {case e: GetError =>
                                errorCont(InvalidObject(s, "ill-formed constant reference"))
                    }
            ceOpt match {
               case Some(c : Constant) =>
                  if (! content.hasImplicit(c.home, home))
                     errorCont(InvalidObject(s, "constant " + c.path + " is not imported into home theory " + home))
               case _ =>
                  errorCont(InvalidObject(s, path + " does not refer to constant"))
            }
            pCont(path)
            s
         case OMV(name) =>
            if (! context.isDeclared(name))
               errorCont(InvalidObject(s, "variable is not declared"))
            s
         case OMA(fun, args) =>
            val funR = checkTerm(home, context, fun)
            val argsR = args map {a => checkTerm(home, context, a)}
            OMA(funR, argsR).from(s)
         case OMBINDC(binder, bound, scopes) =>
            val binderR = checkTerm(home, context, binder)
            val boundR = checkContext(home, context, bound)
            val scopesR = scopes map {c => checkTerm(home, context ++ bound, c)}
            OMBINDC(binderR, boundR, scopesR).from(s)
         case OMATTR(arg, key, value) =>
            val argR = checkTerm(home, context, arg)
            checkTerm(home, context, key)
            val valueR = checkTerm(home, context, value)
            OMATTR(argR, key, valueR).from(s)
         case OMM(arg, morph) =>
            val (morphR, from, to) = checkMorphism(home, context, morph, None, Some(home))
            val argR = checkTerm(from, Context(), arg) // assuming arg to be closed is rather strict, but what else can be done?
            OMM(argR, morphR).from(s)
         case l: OMLIT =>
            content.getO(l.rt.path) match {
               case Some(rc: RealizedTypeConstant) =>
                  if (! content.hasImplicit(rc.home, home))
                     errorCont(InvalidObject(s, "realized type " + rc.path + " is not imported into home theory " + home))
                  if (rc.real != l.rt)
                     errorCont(InvalidObject(s, "illegal literal - " + l.rt.path + " is not the right realized type"))
               case Some(_) =>
                  errorCont(InvalidObject(s, "illegal literal - " + l.rt.path + " exists but is not a realized type"))
               case None =>
                  errorCont(InvalidObject(s, "illegal literal - realized type " + l.rt.path + " is unknown"))
            }
            l
         // resolve type and parse unknown literal, return OMLIT
         case UnknownOMLIT(v, tp) => 
            content.getO(tp) match {
               case Some(rc: RealizedTypeConstant) =>
                  if (! content.hasImplicit(rc.home, home))
                     errorCont(InvalidObject(s, "realized type " + rc.path + " is not imported into home theory " + home))
                  rc.real.parse(v).from(s)
               case _ =>
                  errorCont(InvalidObject(s, "unknown literal type"))
                  s
            }
         case OMFOREIGN(node) => s //TODO
         case OMSemiFormal(t) => s //TODO
         //case _ => s
      }
   }

   /**
    * Checks structural well-formedness of a context relative to a home theory and a context.
    * @param home the home theory
    * @param context the context of con
    * @param con the context to check
    * @return the reconstructed context
    * if context is valid, then this succeeds iff context ++ con is valid
    */
   def checkContext(home: Term, context: Context, con: Context)(implicit pCont: Path => Unit) : Context = {
      con.mapVarDecls {case (c, vd) =>
           val currentContext = context ++ c
           vd match {
              case StructureVarDecl(name, tp, df) =>
                 // a variable that imports another theory
                 // type must be a theory
                 val tpR = checkTheory(home, currentContext, tp) 
                 // definiens must be a partial substitution
                 val dfR = df map {
                    case ComplexMorphism(subs) =>
                       val subsR = checkSubstitution(home, currentContext, subs, TheoryExp.toContext(tpR), Context(), true)
                       ComplexMorphism(subsR)
                    case o =>
                       errorCont(InvalidObject(vd, "definiens of theory-level variable must be a partial substitution, found " + o))
                       o
                 }
                 StructureVarDecl(name, tpR, dfR)
              case VarDecl(name, tp, df) =>
                 // a normal variable
          	     val tpR = tp.map(x => checkTerm(home, currentContext, x))
          	     val dfR = df.map(x => checkTerm(home, currentContext, x))
          	     val vdR = VarDecl(name, tpR, dfR)
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
   def checkSubstitution(home: Term, context: Context, subs: Substitution, from: Context, to: Context, allowPartial: Boolean)(implicit pCont: Path => Unit) : Substitution = {
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
         val matchingDomElems = fromDomain.filter {case de => n.hasPrefix(de.name).isDefined}.toList
         matchingDomElems match {
            case List(de @ DomainElement(p, defined, subdomainOpt)) =>
               if (defined)
                  errorCont(InvalidObject(subs, "found map for " + n + ", but domain already has definition for " + p))
               else {
                  // n maps p: remove de from fromDomain
                  if (p == n) {
                     fromDomain = fromDomain.filter(_ != de)
                  } else {
                     subdomainOpt match {
                        case Some((tpath, defs)) =>
                           // n maps a part of p, which imports tpath: replace de with domain elements for tpath
                           val tpathDomElems = TheoryExp.getDomain(OMMOD(tpath)).map {d =>
                              d.copy(name = p / d.name)
                           }
                           fromDomain = fromDomain.filter(_ != de) ::: tpathDomElems
                           // the definitions of de act like the elements of subsDomain: add them
                           //TODO this does not cover the case where a Structure(VarDecl) has an assignment using a non-total ComplexMorhism
                           // for that, a DomainElement would need the whole substitution domain, not just the List[LocalName]
                           subsDomain :::= defs.map(ln => DomainElement(p / ln, true, None))
                        case None =>
                           errorCont(InvalidObject(subs, "found map for " + n + ", but domain declares basic name " + p))
                     }
                  }
               }
            case Nil =>
               errorCont(InvalidObject(subs, "found map for " + n + ", but domain declares no matching name (may also happen if a substitution declares multiple maps for the same name)"))
            case ns =>
               errorCont(InvalidObject(subs, "found map for " + n + ", but domain declares " + ns.mkString(", ")))
         }
      }
      // subs is total if all names in fromDomain have been removed or are defined to begin with
      if (! allowPartial) {
         if (fromDomain.forall(_.defined))
            errorCont(InvalidObject(subs, "not total, missing cases for " + fromDomain.map(_.name).mkString(", ")))
      }
      // finally, check the individual maps in subs
      subs.map {
         case Sub(n, t) =>
            // for each one, we look up the declaration of n and check t
            val tR = content.deepLookup(from, n) match {
               case StructureVarDecl(n, from, _) =>
                  checkMorphism(home, context, t, Some(from), Some(ComplexTheory(to)))._1
               case _ =>
                  // we could generate a typing obligation here
                  checkTerm(home,to,t)
            }
            Sub(n, tR)
      }
   }
   /** 
    *  special case of checkSubstitution for total substitutions into the empty context
    */
   def checkSubstitutionRealization(home: Term, context: Context, subs: Substitution, from: Context)(implicit pCont: Path => Unit) =
      checkSubstitution(home, context, subs, from, Context(), false)
}

/**
 * combines a [[StructureChecker]] for structural checking with a [[Validator]] for object checking.
 */
class StructureAndObjectChecker(controller: Controller) extends StructureChecker(controller) {
   private val vdt = new Validator(controller)
   /**
    * calls the Validator on the ValidationUnit,
    * registers the dependencies, and
    * (if changed) marks depending components as dirty
    */
   override def unitCont(vu: ValidationUnit) {
      val tc = controller.globalLookup.getComponent(vu.component) match {
         case tc: TermContainer => tc
         case _ => throw ImplementationError("not a TermContainer")
      }
      tc.dependsOn.clear
      val (success,analyzed) = vdt.apply(vu)(errorCont, tc.dependsOn += _)
      val changed = Some(analyzed) != tc.analyzed
      tc.analyzed = analyzed // set it even if unchanged so that dirty flag gets cleared
      if (! success) tc.setAnalyzedDirty // revisit failed declarations
      if (changed) {
         log("changed")
         controller.memory.content.notifyUpdated(vu.component) //TODO: this could be cleaner if taken care of by the onCheck method
      } else
         log("not changed")
   }
}