package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import proving._

/** the proof step ?:Pi x:A.B ----> lambda x:A.(?:B)
 *
 * This rule works for any universe U
 */
object PiIntroduction extends BackwardInvertible {
   val head = Pi.path
   def apply(prover: Searcher, goal: Goal) = goal.conc match {
      case Pi(n,a,b) =>
         onApply("Pi introduction") {
            val n2 = if (n == OMV.anonymous) LocalName("p") else n
            val (x,sub) = Context.pickFresh(goal.fullContext, n2)
            val sg = new Goal(x%a, b ^? sub)
            Alternative(List(sg), () => Lambda(x,a,sg.proof))
         }
      case _ => None
   }
}

/** the proof step ?:A ----> e(?,...?)  for e:Pi x1:A1,...,xn:An.A' where A' ^ s = A for some substitution s
 *
 * This rule works for any universe U and the case n=0.
 * This rule replace ?'s in the result with their terms if they can be inferred through unification.
 */
object BackwardPiElimination extends BackwardSearch {
   val head = Pi.path
   override val priority = 3
   
   private def log(s: => String): Unit = {} // = println(s)

   private object UnnamedArgument {
      def unapply(vd: VarDecl) = vd match {
         case VarDecl(OMV.anonymous, _, Some(t),_,_) => Some(t)
         case _ => None
      }
   }
   private object SolvedParameter {
      def unapply(vd: VarDecl) = vd match {
         case VarDecl(x, _, _,Some(d),_) => Some((x,d))
         case _ => None
      }
   }
   private object UnsolvedParameter {
      def unapply(vd: VarDecl) = vd match {
         case VarDecl(x, _, Some(tp), None,_) if x != OMV.anonymous => Some((x,tp))
         case _ => None
      }
   }

   /**
    * @param context current context
    * @param goal term relative to context
    * @param fact closed (function) type
    * @return the argument types of fact such that applying a function of type fact yields a result of type goal
    */
   private def makeSubgoals(prover: Searcher, context: Context, goal: Term, fact: Term): Option[Context] = {
      // tp must be of the form Pi bindings.scope
      val (bindings, scope) = FunType.unapply(fact).get
      val (paramList, subgoalList) = bindings.span(_._1.isDefined)
      // we do not allow named arguments after unnamed ones
      if (subgoalList.exists(_._1.isDefined))
         return None
      // we do not allow shadowed parameters
      val paramNames = paramList.map(_._1)
      if (paramNames.distinct.length != paramNames.length)
         return None
      // the free variables of scope (we drop the types because matching does not need them)
      val params = FunType.argsAsContext(paramList)
      // fact may contain free variables from stack.context, so make sure there are no name clashes
      // sub is a renaming from unknowns to unknownsFresh
      val (paramsFresh, rename) = Context.makeFresh(params, context.map(_.name))
      val scopeFresh = scope ^? rename
      // match goal against scope, trying to solve for scope's free variables
      // TODO using a first-order matcher is too naive in general - for the general case, we need to use the Solver
      val matcher = prover.makeMatcher
      val matchFound = matcher(context, goal, paramsFresh, scopeFresh)
      val solution = matchFound match {
        case MatchSuccess(subs,_) => subs
        case MatchFail => return None
      }
      // now scope ^ rename ^ solution == goal
      var result = Context()
      bindings foreach {b =>
         // named bound variables that are substituted by solution can be filled in
         // others are holes representing subgoals
         val renameResult = rename ^ result.toPartialSubstitution // maps unsolved variables to their renaming
         b match {
            case (Some(x), xtp) =>
               val xFresh = (x ^ rename).asInstanceOf[OMV].name // rename is a renaming
               result ++= VarDecl(xFresh, None, Some(xtp ^? renameResult), solution(xFresh), None)
            case (None, anontp) =>
               result ++= VarDecl(OMV.anonymous, anontp ^? renameResult)
         }
       }
       Some(result)
   }
   private def makeAlternative(g: Goal, tm: Term, argDecls: Context): Alternative = {
        var sgs : List[Goal] = Nil
        var args: List[() => Term] = Nil
        argDecls foreach {
           case SolvedParameter(_,a) =>
              args ::= {() => a}
           case UnnamedArgument(t) =>
              val sg = new Goal(Nil, t)
              sgs ::= sg
              args ::= {() => sg.proof}
        }
        Alternative(sgs.reverse, () => ApplyGeneral(tm, args.reverseMap(a => a())))
   }
   def apply(prover: Searcher, g: Goal): List[ApplicableTactic] = {
      log("backward Pi elimination on goal " + prover.presentObj(g.conc))
      (g.fullVarAtoms ::: prover.facts.getConstantAtoms).flatMap {case Atom(tm,tp,_) =>
         log("  trying atom " + prover.presentObj(tm))
         // match return type of tp against g.conc
         val sgsOpt = makeSubgoals(prover, g.fullContext, g.conc, tp)
         sgsOpt match {
            case None =>
               // no match
               Nil
            case Some(argDecls) =>
              // matched; check if all named arguments of tp were instantiated
              val unsolvedParameters = argDecls.collect {
                 case u @ UnsolvedParameter(_,_) => u
              }
              if (unsolvedParameters.isEmpty) {
                 // yes: make an alternative using the unnamed arguments of tp as subgoals
                 log("    applicable, making alternative")
                 onApply("Pi elimination backward using " + tm) {makeAlternative(g, tm, argDecls)}.toList
              } else {
                 // no: try to match the first unnamed argument against a known fact to instantiate the remaining ones
                 // now find the first argument that mentions all remaining unsolved parameters
                 val uAOpt = argDecls.find {
                    case UnnamedArgument(t) =>
                       val tvars = t.freeVars
                       unsolvedParameters.forall {p => tvars contains p.name}
                    case _ => false
                 }
                 val appTacs = uAOpt match {
                    case None =>
                       // no progress possible (we might try matching other arguments though)
                       Nil
                    case Some(uA) =>
                       val UnnamedArgument(uAtp) = uA
                       // match uAtp against known facts
                       prover.facts.termsOfTypeAtGoal(g, unsolvedParameters, uAtp) flatMap {case (subs, p) =>
                          // update the argument declarations with the new information
                          val argDecls2 = argDecls.map {
                             case vd if vd == uA =>
                                // the matched goal is already solved by p
                                VarDecl(OMV.anonymous, df = p)
                             case vd @ UnsolvedParameter(x,_) =>
                                // the remaining parameters can now be solved
                                vd.copy(df = subs(x))
                             case vd @ SolvedParameter(_,_) =>
                                // previously solved parameters remain unchanged
                                vd
                             case vd @ UnnamedArgument(t) =>
                                // substitute the new solutions in the remaining unnamed arguments
                                vd.copy(tp = Some(t ^ subs))
                          }
                          log("    applicable with fact " + prover.presentObj(p) + " of type " + prover.presentObj(uAtp ^ subs) + ", making alternative")
                          onApply("Pi elimination backward using " + tm + " and " + prover.presentObj(p) + " : " + prover.presentObj(uAtp ^ subs)) {
                             makeAlternative(g, tm, argDecls2)
                          }.toList
                       }
                 }
                 if (!appTacs.isEmpty)
                    appTacs
                 else {
                    // if no progress, we try enumerating values for the missing parameters
                    // but only if there is only a single missing parameter
                    unsolvedParameters match {
                       case List(uP @ UnsolvedParameter(x,xtp)) =>
                          prover.facts.termsOfTypeAtGoal(g, Context(), xtp) flatMap {case (_, p) =>
                             val argDecls2 = argDecls.map {
                                case vd if vd == uP =>
                                   VarDecl(x, df = p)
                                case vd @ SolvedParameter(_,_) =>
                                   vd
                                case vd @ UnnamedArgument(t) =>
                                   vd.copy(tp = Some(t ^? x/p))
                             }
                             onApply("Pi elimination backward using " + tm + " and " + prover.presentObj(p) + " : " + prover.presentObj(xtp)) {
                                makeAlternative(g, tm, argDecls2)
                             }.toList
                          }
                       case _ => Nil
                    }
                 }
              }
         }
      }
   }
}

object ForwardPiElimination extends ForwardSearch {
   val head = Pi.path
   def generate(prover: Searcher, interactive: Boolean): Unit = {
      // apply all symbols
      prover.facts.getConstantAtoms foreach {case a =>
         if (interactive || a.rl == Some("ForwardRule"))
            applyAtom(prover.goal, a, prover.facts)
      }
      // apply all variables of each goal
      applyVarAtoms(prover.goal, prover.facts)
   }
   /** recursively applies all variables to create new facts */
   private def applyVarAtoms(g: Goal, facts: Facts): Unit = {
      g.varAtoms.foreach {case a => applyAtom(g, a, facts)}
      g.getAlternatives.foreach {_.subgoals.foreach {sg => applyVarAtoms(sg, facts)}}
   }
   /** applies one atom (symbol or variable) to all known facts */
   private def applyAtom(g: Goal, atom: Atom, facts: Facts): Unit = {
      val (bindings, scope) = FunType.unapply(atom.tp).get
      // params: leading named arguments
      val (paramlist, neededArgs) = bindings.span(_._1.isDefined)
      val parameters = FunType.argsAsContext(paramlist)
      // all the actual logic is implemented separately
      new ArgumentFinder(facts, atom.tm, scope).apply(g, parameters, Nil, neededArgs)
   }

   /**
    * @param facts the facts to draw arguments from
    * @param fun the function to apply (once arguments are found)
    * @param scope the return type of fun
    *
    * the parameter types (leading named arguments) and the remaining input types of fun are supplied in the apply method
    */
   private class ArgumentFinder(facts: Facts, fun: Term, scope: Term) {
      /**
       * looks for unnamed arguments and solves parameters along the way
       *
       * each iteration handles the next unnamed argument type and
       * recurses for each found argument term
       *
       * @param g the goal closest to the root at which the new fact will be in scope
       * @param parameters the parameters, definitions are added during recursion
       * @param foundArgs the arguments already found (initially empty)
       * @param neededArgs the arguments still needed (initially all arguments other than parameters)
       */
      def apply(g: Goal, parameters: Context, foundArgs: List[(Option[LocalName],Term)],
                                             neededArgs: List[(Option[LocalName],Term)]): Unit = {
         // the values of the found named arguments as a substitution
         // TODO DM says this rule causes GC-related resource limit overruns
         val foundSubs = foundArgs.collect {case (Some(x),a) => x/a}
         neededArgs match {
            case Nil =>
               parameters.toSubstitution match {
                  case Some(sub) =>
                     // if all parameters were instantiated,
                     // we apply fun to all parameters and found arguments
                     // and add the new fact, whose type is obtained by substituting all parameters and named arguments in scope
                     val f = Fact(g, ApplyGeneral(fun, sub.map(_.target) ::: foundArgs.map(_._2)), scope ^? (sub ++ foundSubs))
                     facts.add(f)
                  case None =>
                     // otherwise, we cannot create a new fact
               }
            case (nOpt, tp) :: rest =>
               // substitute solved parameters and found named arguments in tp
               // tpS stills contain free variables for the so-far-unsolved parameters
               val tpS = tp ^? (parameters.toPartialSubstitution ++ foundSubs)
               // get all possible arguments for tpS
               val args = facts.termsOfTypeBelowGoal(g, parameters, tpS)
               args foreach {case (sub, arg, hOpt) =>
                  // sub instantiates additional parameters that still occurred in tpS
                  // we add these solutions to parameters
                  val newParameters = parameters map {vd =>
                     sub(vd.name) match {
                        case None => vd
                        case Some(s) => vd.copy(df = Some(s))
                     }
                  }
                  // arg is the next argument of type tpS
                  // we append it to foundArgs
                  val newFoundArgs = foundArgs ::: List((nOpt,arg))
                  // hOpt may be a goal below g
                  // in that case, we replace g with h
                  val newGoal = hOpt.getOrElse(g)
                  // we recurse to find the remaining arguments
                  apply(newGoal, newParameters, newFoundArgs, rest)
               }
         }
      }
   }
}
