/**
  * Common utils functions for diagram operators.
  */

package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.moduleexpressions.diagdefinition.DiagramDefinition

object Combinators {
  val _path: MPath = ModExp._base ? "Combinators"
}

object Common {

  /** apply/unapply functions so that ExistingName(p) is the label of a module with URI p */
  object ExistingName {
    def apply(p: MPath) = LocalName(p)

    def unapply(l: LocalName): Option[MPath] = l.steps match {
      case List(ComplexStep(p)) => Some(p)
      case _ => None
    }
  }

  /**
    * A _stateful_ translator to translate [[OMS]]' to [[OML]]s to un-fully-qualify declarations and their
    * interdependent dependencies, say in a [[Theory]] or [[Link]], but generally in a [[ModuleOrLink]].
    *
    * @param ctx                          A [[Context]] object passed to the underlying [[OMSReplacer]], actually unused.
    * @param initialReferencesToUnqualify As we go through constant declaratiosn using [[apply(Constant)]],
    *                                     we keep track of the [[GlobalName]]s of these constant. Namely,
    *                                     to replace subsequent references to them by the un-fully-qualified
    *                                     references. You may here pass an initial list of such [[GlobalName]]s.
    *                                     This may be useful when processing a [[View]], namely in that case you
    *                                     want to pass all [[GlobalName]]s of the codomain.
    */
  class OMStoOML(ctx: Context, initialReferencesToUnqualify: List[GlobalName] = Nil) {
    /**
      * List of names to which references shall be un-fully-qualified.
      *
      * It is continuously expanded by new [[Constant]]s we process in [[apply(Constant)]].
      */
    private var names: List[GlobalName] = initialReferencesToUnqualify

    /**
      * Replacer which only replaces a [[GlobalName]] if it's contained in [[names]].
      */
    private val omsReplacer = OMSReplacer { p =>
      if (names contains p) Some(OML(p.name)) else None
    }

    def getGlobalNamesWhoseReferencesToUnqualify: List[GlobalName] = names

    def apply(tm: Term): Term = omsReplacer(tm, ctx)

    /**
      * Un-fully-qualifies a [[Constant]] and return an [[OML]] for that together with a boolean
      * flag determining if the name of the new OML is a duplicate within the previous constants encountered
      * using this method.
      *
      * This might happen if a theory includes two unrelated theories both declaring a symbol with the same
      * [[LocalName]], but of course with different [[GlobalName]] as they stem from two unrelated theories. In
      * that case un-fully-qualifiying yields a duplicate as signalled by this method.
      */
    def apply(c: Constant, unqualifyLater: Boolean): (OML, Boolean) = {
      val translatedConstant = OML(c.name, c.tp map apply, c.df map apply, c.not)
      val isDuplicate = names.exists(p => p.name == c.name)
      if (unqualifyLater) names ::= c.path
      (translatedConstant, isDuplicate)
    }
  }

  /**
    * Anonymize and flatten a [[ModuleOrLink]], most commonly a [[Theory]] or [[View]], to a list of [[OML]] declarations.
    *
    * Overall, this method will first flatten the [[ModuleOrLink]] and then try to translate every obtained declaration
    * to an [[OML]]. For that [[OMS]]s in declaration components will be replaced by [[OML]]s referencing
    * previous [[OML]] declarations. The method basically keeps track of all "previous [[GlobalName]]s which have
    * been seen and where references to them should be replaced by [[LocalName]] references".
    * If you want to anonymize a [[Link]], this list of "previous things" does not suffice since a [[Link]]
    * is naturally to be interpreted in the context of its codomain. Hence, you can use
    * `initialReferencesToUnqualify` to provide an initial list of "previous things".
    * More precisely, in the case of anonymizing a [[Link]], you should first anonymize the codomain, obtain
    * its list of [[GlobalName]] to unqualify (the first component of the return vaue of this method), and then
    * pass this onto the anonmyization call of the [[Link]].
    *
    * @param solver                       Solver instance used for looking up inclusions (before flattening the [[ModuleOrLink]]).
    * @param namedModuleOrLink            A [[ModuleOrLink]] to be flattened and anonymized to a list of [[OML]]s.
    * @param initialReferencesToUnqualify Read the Scala doc above.
    * @return A tuple of (global names to unqualify, list of OMLs), where
    *         - the global names to unqualify are as explained above the "list of previous things"
    *         - and the list of OMLS the actual anonymized list of declarations
    */
  def anonymizeModuleOrLink(solver: CheckingCallback, namedModuleOrLink: ModuleOrLink, initialReferencesToUnqualify: List[GlobalName] = Nil)(implicit stack: Stack, history: History): (List[GlobalName], List[OML]) = {
    val inLink = namedModuleOrLink match {
      case _: Theory => false
      case _: Link => true
    }
    // Translate all OMS' into OMLs
    val omsTranslator = new OMStoOML(stack.context,initialReferencesToUnqualify)

    val omls = namedModuleOrLink.getAllIncludesWithSelf.flatMap {id =>
      val decls = solver.lookup.getAs(classOf[Theory],id.from).getDeclarationsElaborated
      decls.flatMap {
        case c: Constant =>
          val (translatedConstant,wasDuplicate) = omsTranslator(c,inLink)
          if (wasDuplicate)
            solver.error(namedModuleOrLink.path + " has duplicate local name (" + c.name + "), hence anonymization ignored it")
          // TODO In case of [[Link]]s, the name of the domain declaration contains a complex step involving the domain's theory [[MPath]]
          //      We just overgenerously drop all complex steps for brevity here, might be wrong though
          val dfT = translatedConstant.df orElse (id.df map {df =>
            val t = solver.lookup.ApplyMorphs(OMS(c.path),df,stack.context)
            omsTranslator(t)
          })
          List(translatedConstant.copy(name = translatedConstant.name.dropComplex,df = dfT))
        case _ => Nil
      }
    }
    (initialReferencesToUnqualify ::: omsTranslator.getGlobalNamesWhoseReferencesToUnqualify, omls)
  }

  /** turns a declared theory into an anonymous one by dropping all global qualifiers (only defined if names are still unique afterwards) */
  def anonymizeTheory(solver: CheckingCallback, namedTheory: Theory)(implicit stack: Stack, history: History): AnonymousTheory = {
    AnonymousTheory(namedTheory.meta, anonymizeModuleOrLink(solver, namedTheory)._2)
    // TODO Perhaps add val real = RealizeOML(namedTheory.path, None) // the theorem that the anonymous theory realizes namedTheory?
  }

  // TODO This does not get rid of List(ComplexStep(domainMPath), SimpeleStep(domainLocalNameDecl)) in morphisms
  /** turns a declared theory into an anonymous one by dropping all global qualifiers (only defined if names are still unique afterwards) */
  def anonymizeView(solver: CheckingCallback, namedView: View)(implicit stack: Stack, history: History): AnonymousMorphism = namedView.to match {
    // We first need all [[GlobalName]]s of the codomain to replace in definienses of the view's assignments
    // Hence we first anonymize the codomain, which we here only do (out of naiveity) for a theory as a codomain
    case OMMOD(theoryPath: MPath) =>
      // TODO Recomputing the anonymization of the codomain is really unfortunate as we do this anyway
      //      in [[asAnonymousDiagram]] :( But passing this as method parameter is probably cumbersome in logic
      val codomainGlobalNames = anonymizeModuleOrLink(solver, solver.lookup.getTheory(theoryPath))._1
      AnonymousMorphism(anonymizeModuleOrLink(solver, namedView, initialReferencesToUnqualify = codomainGlobalNames)._2)

    case _ => ???
  }

  /** provides the base case of the function that elaborates a theory expression (in the form of an [[AnonymousTheory]]) */
  def asAnonymousTheory(solver: CheckingCallback, thy: Term)(implicit stack: Stack, history: History): Option[AnonymousTheory] = {
    thy match {
      // named theories
      case OMMOD(p) =>
        solver.lookup.getO(p) match {
          case Some(th: Theory) =>
            lazy val default = anonymizeTheory(solver, th)
            th.dfC.normalize(d => solver.simplify(d)) // make sure a normalization value is cached
            val at = th.dfC.normalized match {
              case Some(df) =>
                df match {
                  case AnonymousTheoryCombinator(at) => at
                  case _ => default
                }
              case None => default
            }
            Some(at)
          case Some(dm: DerivedModule) if dm.feature == DiagramDefinition.feature =>
            dm.dfC.normalized flatMap {
              case AnonymousDiagramCombinator(ad) =>
                ad.getDistNode map { n => n.theory }
              case _ => None
            }
          case Some(_) =>
            solver.error("not a theory: " + p)
            None
          case None =>
            solver.error("unknown name: " + p)
            None
        }
      // explicit anonymous theories
      case AnonymousTheoryCombinator(at) => Some(at)
      case _ => None
    }
  }

  /** like asAnonymousTheory but for morphisms */
  def asAnonymousMorphism(solver: CheckingCallback, fromTerm: Term, from: AnonymousTheory,
                          toTerm: Term, to: AnonymousTheory, mor: Term)(implicit stack: Stack, history: History): Option[AnonymousMorphism] = {
    mor match {
      case OMMOD(p) =>
        solver.lookup.getO(p) match {
          case Some(m: View) =>
            lazy val default = anonymizeView(solver, m)
            m.dfC.normalize(d => solver.simplify(d)) // make sure a normalization value is cached
            val at = m.dfC.normalized match {
              case Some(df) =>
                df match {
                  case AnonymousMorphismCombinator(at) => at
                  case _ => default
                }
              case None => default
            }
            Some(at)
          case _ => throw ImplementationError("missing view")
        }
      case AnonymousMorphismCombinator(at) => Some(at) // explicit anonymous morphisms
      case _ => None
    }
  }

  /** provides the base case of the function that elaborates a diagram expression (in the form of an [[AnonymousDiagram]]) */
  def asAnonymousDiagram(solver: CheckingCallback, diag: Term)(implicit stack: Stack, history: History): Option[AnonymousDiagram] = {
    diag match {
      // named diagrams
      case OMMOD(p) =>
        solver.lookup.getO(p) match {
          case Some(dm: DerivedModule) if dm.feature == DiagramDefinition.feature =>
            dm.dfC.normalized flatMap {
              case AnonymousDiagramCombinator(ad) =>
                Some(ad)
              case _ => None
            }
          case Some(thy: Theory) =>
            // the theory as a one-node diagram
            val anonThy = anonymizeTheory(solver, thy)
            val label = ExistingName(thy.path)
            val anonThyN = DiagramNode(label, anonThy)
            Some(AnonymousDiagram(List(anonThyN), Nil, Some(label)))
          case Some(vw: View) =>
            // the view as a one-edge diagram
            val from = asAnonymousTheory(solver, vw.from).getOrElse(return None)
            val to = asAnonymousTheory(solver, vw.to).getOrElse(return None)
            val mor = asAnonymousMorphism(solver, vw.from, from, vw.to, to, vw.toTerm).getOrElse(return None)
            val label = ExistingName(vw.path)
            // TODO this only makes sense if domain and codomain are named theories; otherwise, we should maybe copy the whole diagram
            val fromL = LocalName(vw.from.toMPath)
            val toL = LocalName(vw.to.toMPath)
            val fromN = DiagramNode(fromL, from)
            val toN = DiagramNode(toL, to)
            val arrow = DiagramArrow(label, fromL, toL, mor, vw.isImplicit)
            Some(AnonymousDiagram(List(fromN, toN), List(arrow), Some(toL)))
          case _ => None
        }
      // explicit anonymous diagrams
      case AnonymousDiagramCombinator(ad) => Some(ad)
      case _ => None
    }
  }

  def prefixLabels(ad: AnonymousDiagram, prefix: LocalName): AnonymousDiagram = {
    def f(l: LocalName) = {
      l match {
        case ExistingName(_) => l
        case _ => prefix / l
      }
    }

    ad.relabel(f)
  }

  /* Applying a substitution function to an OML */
  def applySubstitution(decls: List[OML], renames: List[(LocalName, Term)]): List[OML] =
    decls.map {
      case d@OML(label, tp, df, nt, feature) =>
        val rens = renames.filter(r => if (r._1.equals(label)) true else false)
        if (rens.isEmpty) d
        else (new OML(rens.last._2.asInstanceOf[OML].name, tp, df, nt, feature))
    }

  def asSubstitution(r: List[Term]): List[(LocalName, Term)] = r.map {
    case Rename1(OML(old, None, None, _, _), nw) => (old, nw)
    case _ => return Nil
  }
}

/* The rules below compute the results of theory combinators.
 * Each rule is applicable if the arguments have been computed already.
 *
 * The rules also throw typing errors if they encounter any.
 * Open question: Should they be required to find all errors? Maybe only all structural errors?
 */

