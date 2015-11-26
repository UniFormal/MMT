package info.kwarc.mmt.leo.AgentSystem.MMTSystem


import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.lf._


/** the proof step ?:Pi x:A.B ----> lambda x:A.(?:B)
 *
 * This rule works for any universe U
 */
object PiIntroduction extends BackwardInvertible {
   override val head = Pi.path
   val priority = 5
   def apply(blackboard: MMTBlackboard, goal: Goal) = goal.conc match {
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
   override val head = Pi.path
   val priority = 3

   private object UnnamedArgument {
      def unapply(vd: VarDecl) = vd match {
         case VarDecl(OMV.anonymous, Some(t),_,_) => Some(t)
         case _ => None
      }
   }
   private object SolvedParameter {
      def unapply(vd: VarDecl) = vd match {
         case VarDecl(x, _,Some(d),_) => Some((x,d))
         case _ => None
      }
   }
   private object UnsolvedParameter {
      def unapply(vd: VarDecl) = vd match {
         case VarDecl(x, Some(tp), None,_) if x != OMV.anonymous => Some((x,tp))
         case _ => None
      }
   }

   /**
    * @param context current context
    * @param goal term relative to context
    * @param fact closed (function) type
    * @return the argument types of fact such that applying a function of type fact yields a result of type goal
    */
   private def makeSubgoals(blackboard: MMTBlackboard, context: Context, goal: Term, fact: Term): Option[Context] = {
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
      val matcher = blackboard.makeMatcher(context, paramsFresh)
      val matchFound = matcher(goal, scopeFresh)
      if (!matchFound) return None
      val solution = matcher.getSolution
      // now scope ^ rename ^ solution == goal
      var result = Context()
      bindings foreach {b =>
         // named bound variables that are substituted by solution can be filled in
         // others are holes representing subgoals
         val renameResult = rename ^ result.toPartialSubstitution // maps unsolved variables to their renaming
         b match {
            case (Some(x), xtp) =>
               val xFresh = (x ^ rename).asInstanceOf[OMV].name // rename is a renaming
               result ++= VarDecl(xFresh, Some(xtp ^? renameResult), solution(xFresh), None)
            case (None, anontp) =>
               result ++= VarDecl(OMV.anonymous, Some(anontp ^? renameResult), None, None)
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
   def apply(blackboard: MMTBlackboard, g: Goal): List[ApplicableTactic] = {
      val out = (g.fullVarAtoms ::: blackboard.facts.getConstantAtoms).flatMap {case Atom(tm,tp,_) =>
         // match return type of tp against g.conc
         val sgsOpt = makeSubgoals(blackboard, g.fullContext, g.conc, tp)
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
                       blackboard.facts.termsOfTypeAtGoal(g, unsolvedParameters, uAtp) flatMap {case (subs, p) =>
                          // update the argument declarations with the new information
                          val argDecls2 = argDecls.map {
                             case vd if vd == uA =>
                                // the matched goal is already solved by p
                                VarDecl(OMV.anonymous, None, Some(p), None)
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
                          onApply("Pi elimination backward using " + tm + " and " + blackboard.presentObj(p) + " : " + blackboard.presentObj(uAtp ^ subs)) {
                             makeAlternative(g, tm, argDecls2)
                          }.toList
                       }
                 }
                 if (appTacs.nonEmpty)
                    appTacs
                 else {
                    // if no progress, we try enumerating values for the missing parameters
                    // but only if there is a single missing parameter
                    unsolvedParameters match {
                       case List(uP @ UnsolvedParameter(x,xtp)) =>
                          blackboard.facts.termsOfTypeAtGoal(g, Context(), xtp) flatMap {case (_, p) =>
                             val argDecls2 = argDecls.map {
                                case vd if vd == uP =>
                                   VarDecl(x, None, Some(p), None)
                                case vd @ SolvedParameter(_,_) =>
                                   vd
                                case vd @ UnnamedArgument(t) =>
                                   vd.copy(tp = Some(t ^? x/p))
                             }
                             onApply("Pi elimination backward using " + tm + " and " + blackboard.presentObj(p) + " : " + blackboard.presentObj(xtp)) {
                                makeAlternative(g, tm, argDecls2)
                             }.toList
                          }
                       case _ => Nil
                    }
                 }
              }
         }
      }
     out
   }

}

object ForwardPiElimination extends ForwardSearch {
   override val head = Pi.path
   val priority = 0

   private var factSection:FactSection= null



   def generate(blackboard: MMTBlackboard, interactive: Boolean) {
      this.factSection=blackboard.factSection
      // apply all symbols
      blackboard.facts.getConstantAtoms foreach { case a =>
         if (interactive || a.rl.contains("ForwardRule"))
            applyAtom(blackboard.goal, a, blackboard.facts)
      }
      // apply all variables of each goal
      applyVarAtoms(blackboard.goal, blackboard.facts)
   }

   /** recursively applies all variables to create new facts */
   private def applyVarAtoms(g: Goal, facts: Facts) {
      g.varAtoms.foreach { case a => applyAtom(g, a, facts) }
      g.getAlternatives.foreach {
         _.subgoals.foreach { sg => applyVarAtoms(sg, facts) }
      }
   }

   /** applies one atom (symbol or variable) to all known facts */
   private def applyAtom(g: Goal, atom: Atom, facts: Facts) {
      val (bindings, scope) = FunType.unapply(atom.tp).get
      // params: leading named arguments
      val (paramlist, neededArgs) = bindings.span(_._1.isDefined) //If paramlist and needed arglist are empty its atomic type
      val parameters = FunType.argsAsContext(paramlist)
      // all the actual logic is implemented separately
      new ArgumentFinder(facts, atom.tm, scope).apply(g, parameters, Nil, neededArgs)
   }

   /**
    * @param facts the facts to draw arguments from
    * @param fun the function to apply (once arguments are found)
    * @param scope the return type of fun
    *
    *              the parameter types (leading named arguments) and the remaining input types of fun are supplied in the apply method
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
      def apply(g: Goal, parameters: Context, foundArgs: List[(Option[LocalName], Term)],
                neededArgs: List[(Option[LocalName], Term)]) {
         // the values of the found named arguments as a substitution
         val foundSubs = foundArgs.collect { case (Some(x), a) => x / a }
         neededArgs match {
            case Nil =>
               parameters.toSubstitution match {
                  case Some(sub) =>
                     // if all parameters were instantiated,
                     // we apply fun to all parameters and found arguments
                     // and add the new fact, whose type is obtained by substituting all parameters and named arguments in scope
                     val f = Fact(g, ApplyGeneral(fun, sub.map(_.target) ::: foundArgs.map(_._2)), scope ^? (sub ++ foundSubs))
                     facts.add(f,Some(factSection))
                  case None =>
                  // otherwise, we cannot create a new fact
               }
            case (nOpt, tp) :: rest =>
               // substitute solved parameters and found named arguments in tp
               // tpS stills contain free variables for the so-far-unsolved parameters
               val tpS = tp ^? (parameters.toPartialSubstitution ++ foundSubs)
               // get all possible arguments for tpS
               val args = facts.termsOfTypeBelowGoal(g, parameters, tpS)
               args foreach { case (sub, arg, hOpt) =>
                  // sub instantiates additional parameters that still occurred in tpS
                  // we add these solutions to parameters
                  val newParameters = parameters map { vd =>
                     sub(vd.name) match {
                        case None => vd
                        case Some(s) => vd.copy(df = Some(s))
                     }
                  }
                  // arg is the next argument of type tpS
                  // we append it to foundArgs
                  val newFoundArgs = foundArgs ::: List((nOpt, arg))
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

object TermGeneration extends ForwardSearch {
   override val head = Pi.path
   val priority = 0

   //def respond()

   def getFunctionalFacts(b:MMTBlackboard):List[Fact] = ??? //TODO fill in this step

   def generate(blackboard: MMTBlackboard, interactive: Boolean) {
      getFunctionalFacts(blackboard).foreach(applyFunctionalFact(blackboard,_))
   }


   /** applies one atom (symbol or variable) to all known terms and adds the result to the term database*/
   private def applyFunctionalFact(blackboard:MMTBlackboard,f: Fact) {
      val terms = blackboard.terms
      val (bindings, scope) = FunType.unapply(f.tp).get
      // params: leading named arguments
      val (paramList, otherArgs) = bindings.span(_._1.isDefined) //TODO ask about this step
      //TODO check that everything in other args has an undefined first component
      val parameters = FunType.argsAsContext(paramList)

      val currentReturnType = scope
      val currentFact = f
      //TODO make sure the goal is the correct goal
      findNextParamArg(f.goal, parameters, otherArgs.map(_._2), Nil)


      def findNextParamArg(g: Goal, paramArgs: Context, otherArgs: List[Term], foundParams: Substitution): Unit = {
         if (paramArgs.nonEmpty) {
            val newParam = paramArgs.variables.head
            val newTerms = terms.getTermsOfTypeBelowGoal(newParam.tp.getOrElse{return} ^? foundParams, g)
            //foreach term t of type newParam ^? foundParams above g
            newTerms.foreach {case (t,hOpt) =>
               val newParamArgs = paramArgs.tail
               val newFoundParams = foundParams ++ (newParam.name / t)
               findNextParamArg(hOpt.getOrElse(g), newParamArgs, otherArgs, newFoundParams)
            }
         } else {
            findNextOtherArg(g, otherArgs, foundParams, Nil)
         }
      }

      def findNextOtherArg(g: Goal, otherArgs: List[Term], foundParams: Substitution, foundOtherArgs: List[Term]): Unit = {
         if (otherArgs.nonEmpty) {
            //foreach term t of type otherArgs (0) ^? foundParams above g
            val newTerms = terms.getTermsOfTypeBelowGoal(otherArgs.head ^? foundParams, g)
            newTerms.foreach { case (t, hOpt) =>
               findNextOtherArg(hOpt.getOrElse(g), otherArgs.tail, foundParams, foundOtherArgs ::: List(t))
            }
         } else {
            val args = foundParams.map(_.target) ::: foundOtherArgs
            terms += TermEntry(g, ApplySpine(currentFact.tm, args: _*), currentReturnType ^? foundParams)
         }
      }
   }

}


class TransitivityGeneration(blackboard:MMTBlackboard,rel: GlobalName, ded: GlobalName)
                            (implicit controller: Controller,oLP:String) extends TransitivityAgent(blackboard){

   val Ded = new UnaryLFConstantScala(ded.module, ded.name.toString)
   val Rel = new BinaryLFConstantScala(rel.module, rel.name.toString)
   override val head = Pi.path
   override val priority = 0

   def generate(blackboard: MMTBlackboard, interactive: Boolean) = ???

   def addTask()= taskQueue+=new TransitivityTask(this)

   def addFact(f:Fact,tdb:TransitivityDB): Unit ={
      f.tp match {
         case Ded(Rel(x,y)) =>
            tdb.getGraph(rel).add(x,y,f)
         case Rel(x,y) =>
            tdb.getGraph(rel).add(x,y,f)
         case _ => throw new IllegalArgumentException("Not a valid type of fact: need a transitive fact")
      }
   }


}
