package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import uom._
import symbols._
import checking._
import info.kwarc.mmt.api.modules.Theory
import objects._
import objects.Conversions._
import info.kwarc.mmt.lf._

/**
 * THY : Inhabitable
 */
object TheoryTypeInhabitable extends InhabitableRule(ModExp.theorytype) {
   def apply(solver: Solver)(tp: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
      tp match {
         case TheoryType(params) =>
            if (params.isEmpty)
               Some(true)
            else
               Some(solver.check(IsTheory(stack, ComplexTheory(params))))
      }
   }
}

/**
 * THY : Universe
 */
object TheoryTypeUniverse extends UniverseRule(ModExp.theorytype) {
   def apply(solver: Solver)(tp: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
      tp match {
         case TheoryType(params) =>
            if (params.isEmpty)
               Some(true)
            else
               Some(solver.check(IsTheory(stack, ComplexTheory(params))))
      }
   }
}

/**
 * MOR A B : Inhabitable
 */
object MorphTypeInhabitable extends InhabitableRule(ModExp.morphtype) {
   def apply(solver: Solver)(tp: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
      val MorphType(from,to) = tp
      Some(solver.check(IsTheory(stack, from)) && solver.check(IsTheory(stack, to)))
   }
}

/**
 * C Context  --->  {{C}} : theory
 */
// TODO get rid of LF dependency here? (OfType)
object ComplexTheoryInfer extends InferenceRule(ModExp.complextheory, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case ComplexTheory(con) =>
         if (covered) return Some(TheoryType(Nil))
         con.mapVarDecls {case (c, vd) =>
           val currentStack = stack ++ c
           val mayhold = vd match {
              // an import from another theory
              case StructureVarDecl(name, tp, df) =>
                 // type must be a theory
                 solver.check(IsTheory(currentStack, tp)) &&
                 // if given, definiens must be a morphism
                 (df match {
                    case Some(d) =>
                       solver.check(IsRealization(currentStack, d, tp))
                    case None => true
                 })
              case VarDecl(n,None,tpOpt,dfOpt,_) =>
                 tpOpt match {
                    case Some(tp) =>
                       solver.check(Inhabitable(currentStack, tp))(history + ("type of " + n + " must be inhabitable"))
                       dfOpt match {
                          case Some(df) =>
                             solver.check(Typing(currentStack, tp, df))(history + ("definiens of " + n + " must type check"))
                          case None => true
                       }
                    case None =>
                       dfOpt match {
                          case Some(df) =>
                             solver.error("type may not be omitted")
                          case None => true
                       }
                 }
           }
           if (!mayhold)
              return None
         }
         Some(TheoryType(Nil))
      case AnonymousTheoryCombinator(_) => Some(TheoryType(Nil)) // TODO
      case _ =>
         solver.error("illegal use of " + ModExp.complextheory)
         None
   }
}

/**
  * DIAG : Inhabitable
  */
object DiagramTypeInhabitable extends InhabitableRule(ModExp.diagramtype) {
  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = term match {
    case OMS(this.head) => Some(true)
    case _ => Some(false)
  }
}

object AnonymousDiagramInfer extends InferenceRule(ModExp.anonymousdiagram, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
    case AnonymousDiagramCombinator(_) =>
      Some(DiagramType())
    case _ =>
      None
  }
}

object DiagramCheck extends TypingRule(ModExp.anonymousdiagram) {
  override def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val simplificationUnit = SimplificationUnit(stack.context, expandConDefs=true,expandVarDefs = true, fullRecursion = true, solverO = Some(solver))

    solver.controller.simplifier(tm, simplificationUnit) match {
      case AnonymousDiagramCombinator(_) =>
        Some(true)

      case _ =>
        Some(false)
    }
  }
}

object AnonymousTheoryInfer extends InferenceRule(ModExp.anonymoustheory, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case AnonymousTheoryCombinator(_) =>
        Some(TheoryType(Nil)) // TODO
      case _ =>
         solver.error("illegal use of " + ModExp.anonymoustheory)
         None
   }
}

/**
 * m : MOR A B
 */
object MorphCheck extends TypingRule(ModExp.morphtype) {
   def check(solver: Solver)(subs: Substitution, from: Term, to: Term, allowPartial: Boolean)
                                   (implicit stack: Stack, history: History) : Boolean = {
      implicit val lup = solver.controller.globalLookup
      var fromDomain = TheoryExp.getDomain(from)
      var subsDomain = subs.asContext.getDomain
      /* intuitively: subs is partial/total substitution only if
       *    fromDomain without defined elements subsumes/is equal to subsDomain
       * but we also allow instantiations in fromContext.map(_.df) or subsDf that are more specific than names in fromContext.map(_.tp)
       *    in that case, we replace the name in fromDomain with its subdomain
       */
      // (1) clash analysis: for each elements of subsDomain, we remove the corresponding element of fromDomain
      //       We flatten fromDomain as much as needed.
      /* invariants:
       *   fromDomain contains the declaring DomainElements that must still be mapped
       *   subsDomain contains the mapping DomainElements that have not been considered yet
       */
      while (! subsDomain.isEmpty) {
         val currentSub = subsDomain.head
         val currentSubName = currentSub.name
         // all domain elements of from to which current applies
         val matchingDomElems = fromDomain.filter {case de => currentSubName.dropPrefix(de.name).isDefined}.toList
         matchingDomElems match {
            case List(de @ DomainElement(p, defined, subdomainOpt)) =>
               if (defined)
                  // no map allowed if currentSubName already defined in from
                  solver.error("found map for " + currentSubName + ", but domain already has definition for " + p)
               else {
                  // currentSub maps de: remove currentSub from subsDomain and de from fromDomain
                  if (p == currentSubName) {
                     fromDomain = fromDomain.filter(_ != de)
                     subsDomain = subsDomain.tail
                  } else {
                     // currentSub maps a part of de (currentSubName has prefix p)
                     subdomainOpt match {
                        // de must be an import from tpath
                        case Some((dom, defs)) =>
                           // flatten de: replace it with the domain elements of tpath (prefixing p to the name)
                           fromDomain = fromDomain.filter(_ != de)
                           TheoryExp.getDomain(dom).foreach {d =>
                              fromDomain ::= d.copy(name = p / d.name)
                           }
                           // the definitions of de are like fixed cases of subsDomain
                           // they are assumed well-typed but must participate in flattening and clash+totality analysis
                           subsDomain :::= defs.map(ln => DomainElement(p / ln, true, None))
                           // TODO the above assumes all elements of defs to be fully defined;
                           // this does not cover the case where an import is mapped to a partial morphism
                        case None =>
                           solver.error("found map for " + currentSubName + ", but domain declares non-import name " + p)
                     }
                  }
               }
            case Nil =>
               solver.error("found map for " + currentSubName + ", but domain declares no matching name (may also happen if a substitution declares multiple maps for the same name)")
            case ns =>
               solver.error("found map for " + currentSubName + ", but domain declares " + ns.mkString(", "))
         }
      }
      // (2) totality analysis
      // subs is total if all names in fromDomain have been removed in (1) or were defined to begin with
      if (! allowPartial) {
         val missingCases = fromDomain.filter(! _.defined)
         if (! missingCases.isEmpty)
            solver.error("not total, missing cases for " + missingCases.map(_.name).mkString(", "))
      }
      // (3) type-checking the individual maps in subs
      // invariant subsChecked ::: subsToCheck == subs
      var subsChecked = Substitution()
      var subsToCheck = subs
      while (! subsToCheck.isEmpty) {
         val current = subsToCheck.subs.head
         val Sub(n, t) = current
         // for each one, we look up the declaration of n and check t
         val d = lup.getO(from, n).get
         val historyN =  history + ("checking map of " + n)
         val mayhold = d match {
            case s: Structure =>
               solver.check(IsRealization(stack, t, s.from))(historyN)
            case c: Constant =>
               c.tp foreach {tp =>
                  val tpT = OMM(tp, ComplexMorphism(subsChecked))
                  solver.check(Typing(stack, t, tpT))(historyN)
               }
               true
            case _ =>
               solver.error(n + " does not refer to mappable declaration in " + from)
         }
         if (! mayhold)
            return false
         subsChecked = subsChecked ::: List(current)
         subsToCheck = subsToCheck.tail
      }
      true
   }

   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
      val MorphType(from,to) = tp
      tm match {
         // all cases where the domain/codomain cannot be inferred
         case ComplexMorphism(subs) =>
            // subs must be total morphism from => to
            Some(check(solver)(subs, from, to, false))
         case OMCOMP(Nil) =>
            Some(solver.check(Equality(stack,from,to,None)))
         case OMCOMP(m::ms) =>
            // there could be a ComplexMorphism in the last position, so we chain the expected type through
            Some(solver.inferTypeAndThen(m)(stack, history) {
               case MorphType(f,t) =>
                  solver.check(Equality(stack, from, f, None)) &&
                  solver.check(IsMorphism(stack, OMCOMP(ms), t,to))
               case _ =>
                  solver.error("")
            })
         case _ => throw TypingRule.SwitchToInference
      }
   }
}

/**
 * T: theory  --->  id_T : T=>T
 */
object IdentityInfer extends InferenceRule(ModExp.identity, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
        case OMIDENT(t) =>
           if (!covered) solver.check(IsTheory(stack, t))
           Some(MorphType(t,t))
      }
   }
}

/**
 * m1: a1 => b1 and m2: a2 => b2 and b1 <= a2  --->  m1;m2: a1 => b2
 *
 * cannot infer type of empty composition
 */
object CompositionInfer extends InferenceRule(ModExp.composition, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      val OMCOMP(ms) = tm
      ms match {
        case Nil => None
        case List(m) => solver.inferType(m)
        case hd::tl =>
           (solver.inferType(hd), solver.inferType(OMCOMP(tl))) match {
              case (Some(MorphType(a1,b1)), Some(MorphType(a2,b2))) =>
                 if (!covered) solver.check(Equality(stack, b1,a2, Some(TheoryType(Nil))))
                 Some(MorphType(a1,b2))
              case _ => None
           }
      }
   }
}

/**
 * typing is preserved along morphisms
 * 
 * m: MOR mDom mCod  and   mDom |- t : a  --->  mCod |- t APPLY m : a APPLY m
 */
object MorphismApplicationTerm extends InferenceRule(ModExp.morphismapplication, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      tm match {
        case OMM(t,m) =>
          if (t.freeVars.nonEmpty) {
            solver.error("cannot apply morphism to term with free variables yet")
          }
          val mI = solver.inferType(m, covered)(stack, history + ("inferring type of morphism"))
          val (mDom,mCod) = mI match {
            case Some(MorphType(d,c)) => (d,c)
            case _ => return None
          }
          val impl = solver.lookup.getImplicit(mCod, ComplexTheory(solver.constantContext)).getOrElse {
            solver.error("morphism does not translate into the current theory")
            return None
          }
          val mDomContext = mDom match {
            case OMPMOD(p,as) => Context(IncludeVarDecl(p,as))
            case _ =>
              history += "domain of morphism is not an instance of a named theory"
              return None
          }
          // TODO does not work because local context is ignored when looking up constants; also the wrong rules are applied
          val tI = solver.inferType(t, covered)(Stack(mDomContext), history + ("inferring term over theory "))
          tI map {tp => OMM(tp,OMCOMP(m,impl))}
        case _ => None
      }
   }
}

/**
 * apply a morphism
 * 
 * t APPLY m  ----> m(t)
 */
object MorphismApplicationCompute extends ComputationRule(ModExp.morphismapplication) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      tm match {
        case OMM(t,m) =>
          val mI = solver.inferType(m, covered)(stack, history + ("inferring type of morphism"))
          val (mDom,mCod) = mI match {
            case Some(MorphType(d,c)) => (d,c)
            case _ => return Recurse
          }
          val impl = solver.lookup.getImplicit(mCod, ComplexTheory(solver.outerContext)).getOrElse {
            solver.error("morphism does not translate into the current theory")
            return Recurse
          }
          if (t.freeVars.nonEmpty) {
            solver.error("cannot apply morphism to term with free variables yet")
          }
          val tT = solver.lookup.ApplyMorphs(t, OMCOMP(m,impl))
          Simplify(tT)
        case _ => Simplifiability.NoRecurse // may happen if in binder of OMBIND
      }
   }
}
