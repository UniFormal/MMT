package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import objects._
import uom._
import info.kwarc.mmt.lf._

object Combinators {
  val _path = ModExp._base ? "Combinators"
}

object Common {
  /** turns a declared theory into an anonymous one by dropping all global qualifiers (only defined if names are still unique afterwards) */
  def anonymize(solver: CheckingCallback, namedTheory: Theory)(implicit stack: Stack, history: History): AnonymousTheory = {
    // collect included theories
    val includes = namedTheory.getIncludesWithoutMeta.flatMap {i => solver.lookup.getO(i) match {
      case Some(dt: Theory) =>
        List(dt)
      case Some(se) =>
        solver.error("ignoring include of " + se.path)
        Nil
      case None =>
        Nil
    }}
    // code for translating OMS's to OML references
    var names: List[GlobalName] = Nil
    val trav = OMSReplacer {p =>
      if (names contains p) Some(OML(p.name)) else None
    }
    def translate(tm: Term) = trav(tm, stack.context)
    // turn all constants into OML's
    val omls = (includes:::List(namedTheory)).flatMap {th =>
      th.getDeclarationsElaborated.flatMap {
        case c: Constant =>
          val cT = OML(c.name,  c.tp map translate, c.df map translate, c.not)
          if (names.exists(p => p.name == c.name))
            solver.error("theory has duplicate name: " + c.name)
          names ::= c.path
          List(cT)
        case _ => Nil
      }
    }
    val real = RealizeOML(namedTheory.path, None) // the theorem that the anonymous theory realizes namedTheory
    new AnonymousTheory(namedTheory.meta, omls ::: List(real))
  }

  /** provides the base case of the function that elaborates a theory expression (in the form of an [[AnonymousTheory]]) */
  def asAnonymousTheory(solver: CheckingCallback, thy: Term)(implicit stack: Stack, history: History): Option[AnonymousTheory] = {
    thy match {
      // named theories
      case OMMOD(p) =>
        solver.lookup.getO(p) match {
          case Some(th: Theory) =>
            lazy val default = anonymize(solver, th)
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
    val fromPath = fromTerm match {
      case OMMOD(p) => p
      case _ => return None
    }
    val morAnon = new AnonymousMorphism(Nil)
    // replaces toTerm-OMS's in mor with to-OMLs 
    val trav = OMSReplacer {p =>
      if (to.isDeclared(p.name)) Some(OML(p.name)) else None
    }
    from.decls.foreach {oml =>
      solver.lookup.getO(mor, ComplexStep(fromPath)/oml.name) foreach {
        case c: Constant => c.df foreach {df =>
          val dfT = trav(df, Context.empty)
          morAnon add OML(oml.name, None, Some(dfT))
        }
        case se =>
          solver.error("unknown assignment " + se.path)
      }
    }
    Some(morAnon)
  }
  
  /** provides the base case of the function that elaborates a diagram expression (in the form of an [[AnonymousDiagrma]]) */
  def asAnonymousDiagram(solver: CheckingCallback, thy: Term)(implicit stack: Stack, history: History): Option[AnonymousDiagram] = {
    thy match {
      // named diagrams
      case OMMOD(p) =>
        solver.lookup.getO(p) match {
          case Some(dm: DerivedModule) if dm.feature == DiagramDefinition.feature =>
            dm.dfC.normalized flatMap {
              case AnonymousDiagramCombinator(ad) => Some(ad)
              case _ => None
            }
          case Some(thy: Theory) =>
            // the theory as a one-node diagram
            val anonThy = anonymize(solver, thy)
            val label = LocalName(thy.path)
            val anonThyN = DiagramNode(label, anonThy)
            val ad = new AnonymousDiagram(List(anonThyN), Nil, Some(label), None)
            Some(ad)
          case Some(vw: View) =>
            // the view as a one-edge diagram
            val from = asAnonymousTheory(solver, vw.from).getOrElse(return None)
            val to   = asAnonymousTheory(solver, vw.to).getOrElse(return None)
            val mor  = asAnonymousMorphism(solver, vw.from, from, vw.to, to, vw.toTerm).getOrElse(return None)
            val label = LocalName(vw.path)
            // TODO this only makes sense if domain and codomain are named theories; otherwise, we should maybe copy the whole diagram
            val fromL = LocalName(vw.from.toMPath)
            val toL   = LocalName(vw.to.toMPath)
            val fromN = DiagramNode(fromL, from)
            val toN   = DiagramNode(toL, to)
            val arrow = DiagramArrow(label, fromL, toL, mor)
            val distArrow = if (vw.isImplicit) Some(label) else None
            val ad = new AnonymousDiagram(List(fromN,toN), List(arrow), Some(toL), distArrow)
            Some(ad)
          case _ => return None
        }
       // explicit anonymous diagrams
      case AnonymousDiagramCombinator(ad) => Some(ad)
      case _ => None
    }
  }
  
}

/* The rules below compute the results of theory combinators.
 * Each rule is applicable if the arguments have been computed already.
 *
 * The rules also throw typing errors if they encounter any.
 * Open question: Should they be required to find all errors? Maybe only all structural errors?
 */

object Extends extends FlexaryConstantScala(Combinators._path, "extends"){
  /** the label of the distinguished node after extension */
  val nodeLabel = LocalName("pres")
  /** the label of the distinguished arrow after extension (from old to extended theory) */
  val arrowLabel = LocalName("extend")
}

// TODO all rules must preserve and reflect typing errors
// declaration merging must happen somewhere

object ComputeExtends extends ComputationRule(Extends.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Extends(diag,wth@_*) = tm
    val ad = Common.asAnonymousDiagram(solver, diag).getOrElse {return RecurseOnly(List(1))}
    // Getting the new declarations as List[OML]
    val extD = wth match {
      case OMLList(extDecl) => extDecl
      case _ => return RecurseOnly(List(2))
    }
    // dN : distinguished node of the input diagram
    val dN = ad.getDistNode.getOrElse {
      solver.error("distinguished node not found")
      return Simplifiability.NoRecurse
    }
    // creating the new AnonymousDiagram
    val new_decls = dN.theory.decls ::: extD
    val new_dN = DiagramNode(Extends.nodeLabel, new AnonymousTheory(dN.theory.mt,new_decls))
    val extM = new AnonymousMorphism(new_decls)
    val extA = DiagramArrow(Extends.arrowLabel, dN.label, new_dN.label, extM)
    val result = new AnonymousDiagram(ad.nodes ::: List(new_dN), ad.arrows ::: List(extA), Some(Extends.nodeLabel), Some(Extends.arrowLabel))
    Simplify(result.toTerm)
  }
}

object Combine extends FlexaryConstantScala(Combinators._path, "combine")

object ComputeCombine extends ComputationRule(Combine.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      val Combine(thys@_*) = tm
      val thysAnon = thys map {thy => Common.asAnonymousTheory(solver, thy).getOrElse(return Recurse)}
      var mts: List[MPath] = Nil
      var decls: List[OML] = Nil
      thysAnon.foreach {at =>
          at.mt.foreach {mts ::= _}
          decls = decls ::: at.decls
      }
      val declsD = decls.distinct
      val mt = mts.distinct match {
        case Nil => None
        case hd::Nil => Some(hd)
        case _ => return Recurse
      }
      Simplify(AnonymousTheoryCombinator(mt, declsD))
  }
}

object Rename extends FlexaryConstantScala(Combinators._path, "rename") {
  /** the label of the renamed theory */
  val nodeLabel = LocalName("pres")
  /** the label of the renaming morphism (from old to renamed) */
  val arrowLabel = LocalName("extend")
}

object ComputeRename extends ComputationRule(Rename.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Rename(diag,rens@_*) = tm
    val ad = Common.asAnonymousDiagram(solver, diag).getOrElse {return RecurseOnly(List(1))}
    // perform the renaming
    val oldNew = rens.toList.flatMap {
      case OML(nw, None, Some(OML(old, None,None,_,_)),_,_) =>
        List((old,nw))
      case OML(nw,None,Some(OMS(old)),_,_) =>
        List((old.name,nw))
      case r =>
        solver.error("not a renaming " + r)
        Nil
    }
    val dN = ad.getDistNode.getOrElse {
      solver.error("distinguished node not found")
      return Simplifiability.NoRecurse
    }
    val atR = dN.theory.copy
    atR.rename(oldNew:_*)
    val dNR = DiagramNode(Rename.nodeLabel, atR)
    val renM = new AnonymousMorphism(oldNew map {case (o,n) => OML(o, None, Some(OML(o)))})
    val renA = DiagramArrow(Rename.arrowLabel, dN.label, dNR.label, renM)
    val result = new AnonymousDiagram(ad.nodes ::: List(dNR), ad.arrows ::: List(renA), Some(Rename.nodeLabel), Some(Rename.arrowLabel)) 
    // remove all invalidated realizations, i.e., all that realized a theory one of whose symbols was renamed
    // TODO more generally, we could keep track of the renaming necessary for this realization, but then realizations cannot be implicit anymore
    /*val removeReals = thyAnon.decls.flatMap {
      case oml @ RealizeOML(p, _) =>
        solver.lookup.getO(p) match {
          case Some(dt: Theory) =>
            if (oldNew.exists {case (old,nw) => dt.declares(old)})
              List(oml)
            else
              Nil
          case _ => Nil
        }
      case _ => Nil
    }
    thyAnon.decls = thyAnon.decls diff removeReals */
    Simplify(result.toTerm)
  }
}

/**
 * Translate(m,T) and Expand(m,T) form a pushout along an inclusion as follows:
 *
 * m : A -> B
 * inclusion from A to T
 * Expand(m,T): T -> Translate(m,T)
 * inclusion from B to Translate(m,T)
 */
object Translate extends BinaryConstantScala(Combinators._path, "translate")

object ComputeTranslate extends ComputationRule(Translate.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Translate(mor, thy) = tm
    val dom = Morph.domain(mor)(solver.lookup).getOrElse{return Recurse}
    val cod = Morph.codomain(mor)(solver.lookup).getOrElse{return Recurse}
    val List(thyAnon,domAnon,codAnon) = List(thy,dom,cod).map {t => Common.asAnonymousTheory(solver, t).getOrElse(return Recurse)}
    val morAnon = Common.asAnonymousMorphism(solver, dom, domAnon, cod, codAnon, mor).getOrElse(return Recurse)
    // translate all declarations of thy that are not from dom via mor and add them to cod
    val morAsSub = morAnon.decls.flatMap {oml => oml.df.toList.map {d => Sub(oml.name, d)}}
    val translator = OMLReplacer(morAsSub)
    val pushout = codAnon
    thyAnon.decls.foreach {
      case RealizeOML(p,_) =>
        // these may also be translatable, but they are optional anyway
      case oml =>
        if (! domAnon.isDeclared(oml.name)) {
          if (codAnon.isDeclared(oml.name)) {
            solver.error("pushout not defined because of name clash: " + oml.name)
            return Recurse
          }
          val omlT = translator(oml, stack.context).asInstanceOf[OML]
          pushout.add(omlT)
        }
    }
    Simplify(pushout.toTerm)
  }
}

// TODO better name
/** see [[Translate]] */
object Expand extends BinaryConstantScala(Combinators._path, "expand")

// TODO does not work yet
object ComputeExpand extends ComputationRule(Expand.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
      val Expand(mor, thy) = tm
      thy match {
        case AnonymousTheoryCombinator(at) =>
          val res = new AnonymousTheory(at.mt, Nil) //TODO this should be an AnonymousMorphism; same as AnonymousTheory but no dependency
          // add include of mor
          at.decls.foreach {case OML(n,t,d,_,_) =>
            // skip all includes of theories that are already include in domain of mor
            val ass = OML(n,None,Some(OML(n,None,None)))
            res.add(ass)
          }
          Simplify(res.toTerm)
        case _ => Recurse
      }
   }
}
