package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend._
import objects._
import symbols._
import libraries._
import checking._
import uom._
import utils._
import notations.ImplicitArg

/* idea: allow for custom typing rules that lift an object level typing relation in a way similar to quotienting by an object level equality
 */

/**
 * convenience class for storing the names of an OuterInnerTerm
 *
 * @param outer outer operator
 * @param implArgsOuter positions of the implicit arguments of 'outer' (counting from 0)
 * @param before names of the before arguments (excluding implicit ones)
 * @param innerPos position of inner term within the arguments of 'outer' (including implicit ones)
 * @param inside names of the inside-arguments (excluding implicit ones)
 * @param implArgsInner positions of the implicit arguments of 'inner' (counting from 0)
 * @param after names of the after arguments (excluding implicit ones)
 *
 * this represents a left hand side of the form
 *   outer(before,inner(inside),after)
 * where before, inside, after do not include the implicit arguments
 */
case class OuterInnerNames(outer: GlobalName, implArgsOuter: List[Int], before: List[LocalName], 
                           innerPos: Int, inner: GlobalName, implArgsInner: List[Int], inside: List[LocalName], 
                           after: List[LocalName]
                           ) {
   /** before:::inside:::after */
   def allNames = (before:::inside:::after)
   /** drops implicit arguments from before- and after-arguments */
   def explArgsOuter(allBefore: List[Term], allAfter: List[Term]): (List[Term],List[Term]) = {
      val b = allBefore.zipWithIndex.filterNot(p => implArgsOuter.contains(p._2)).map{_._1}
      val a = allAfter .zipWithIndex.filterNot(p => implArgsOuter.contains(p._2 + allBefore.length)).map{_._1}
      (b,a)
   }
   /** drops implicit arguments from inside-arguments */
   def explArgsInner(allArgs: List[Term]): List[Term] =
      allArgs.zipWithIndex.filterNot(p => implArgsInner.contains(p._2)).map{_._1}
   def outerArity = implArgsOuter.length+before.length+1+after.length
}

object SimplificationRuleGenerator {
  /** the Tag used to spot constants with name N from which to simplification rules with name N/SimplifyTag */
  val SimplifyTag = "Simplify"
  protected val SolutionTag = "Solve"
  protected val under = List(Apply.path)
  private def rulePath(r: GeneratedDepthRule) = r.from.path / SimplifyTag
}
import SimplificationRuleGenerator._

class SimplificationRuleGenerator extends ChangeListener {
  override val logPrefix = "simp-rule-gen"
  private def getGeneratedRule(p: Path): Option[GeneratedDepthRule] = {
     p match {
        case p: GlobalName =>
           controller.globalLookup.getO(p / SimplifyTag) match {
              case Some(r: RuleConstant) => r.df.map(df => df.asInstanceOf[GeneratedDepthRule])
              case _ => None
           }
        case _ => None
     }
  }

  override def onAdd(e: StructuralElement) {onCheck(e)}
  override def onDelete(e: StructuralElement) {
     getGeneratedRule(e.path).foreach {r => controller.delete(rulePath(r))}
  }
  override def onCheck(e: StructuralElement) {
       val c = e match {
          case c: symbols.Constant if c.rl == Some(SimplifyTag) =>
             val name = c.name
             if (c.tpC.analyzed.isDefined) {
                // check if an up-to-date rule for this constant exists already: if so break, otherwise delete it
                getGeneratedRule(c.path) foreach {r =>
                   if (r.validSince >= c.tpC.lastChangeAnalyzed) {
                      log(s"rule for $name is up-to-date")
                      return
                   } else
                      controller.delete(rulePath(r))
                }
                c
             } else {
                if (c.tp.isDefined)
                   log(s"type of $name not valid or not checked yet, skipped")
                return
             }
          case _ => return
       }
       val tm = c.tpC.analyzed.get
       if (parser.ObjectParser.isOnlyParsed(tm)) {
          log("type only partially validated, skipped")
          return
       }
       tm match {
           case FunType(args, scp) =>
              val context = FunType.argsAsContext(args)
              scp match {
                case ApplySpine(OMS(eq), argls) if argls.length >= 2 =>
                    if (controller.globalLookup.getConstant(eq).rl == Some("Eq"))
                       generateRule(c, context, argls)
                   else
                      error(e, "not of eq-args shape")
               case ApplySpine(OMS(ded), List(ApplySpine(OMS(eq), argls))) if argls.length >= 2 =>
                   if (controller.globalLookup.getConstant(ded).rl == Some("Judgment") &&
                       controller.globalLookup.getConstant(eq).rl == Some("Eq")) {
                      generateRule(c, context, argls)
                   } else
                      error(e, "not of ded-eq-args shape")
               case _ =>
                  error(e, "not a depth rule")
              }
           case _ =>
              error(e, "not a depth rule")
       }
  }
  private def error(e: StructuralElement, msg: String) {
     logError(e.path + ": " + msg)
  }
  private def present(t: Term) = controller.presenter.asString(t)

  /** matcher for outer(..., inner(...), ...) */
  object OuterInnerTerm {
     def unapply(t: Term): Option[OuterInnerNames] = t match {
        case ApplyGeneral(OMS(outer), args) =>
           // we try to break args into bfr ::: OMA(inner, ins) ::: aft
           var bfr : List[LocalName] = Nil
           var inner : GlobalName = null
           var innerPos: Int = -1
           var ins : List[LocalName] = Nil
           var aft : List[LocalName] = Nil
           var isBefore = true // true if we are in bfr, false if we are in aft
           // the implicit arguments of the outer operator
           val implArgsOuter = controller.globalLookup.getConstant(outer).not match {
              case Some(n) => n.arity.flatImplicitArguments(args.length) map {case ImplicitArg(i,_) => i-1}
              case None => Nil
           }
           // the implicit arguments of the outer operator
           var implArgsInner : List[Int] = Nil // will be set once we know what the inner operator is
           // iterate through args to break it up
           args.zipWithIndex.foreach {case (arg, i) =>
              // no need to match the implicit arguments, so skip them
              if (! implArgsOuter.contains(i)) {
                 arg match {
                    case OMV(x) =>
                      if (isBefore)
                         bfr ::= x
                      else
                         aft ::= x
                    case ApplyGeneral(OMS(inr), args) =>
                      implArgsInner = controller.globalLookup.getConstant(inr).not match {
                           case Some(n) => n.arity.flatImplicitArguments(args.length) map { case ImplicitArg(i,_) => i-1 }
                           case None => Nil
                      }
                      if (isBefore) {
                         inner = inr
                         innerPos = i
                         args.zipWithIndex foreach {case (a,j) =>
                            if (! implArgsInner.contains(j)) {
                                a match {
                                  case OMV(x) =>
                                     ins ::= x
                                  case e =>
                                     return None // non-variable argument to inner
                                }
                            }
                         }
                         isBefore = false
                      } else
                          return None // second non-variable argument to outer
                    case _ => return None // other argument to outer
                  }
              }
           }
           if (isBefore)
              return None // no inner(vars) argument to outer
           Some(OuterInnerNames(outer, implArgsOuter, bfr.reverse, innerPos, inner, implArgsInner, ins.reverse, aft.reverse))
         case _ => return None // no outer(...) term
      }
   }
  
  private lazy val msc = new MatchStepCompiler(controller.globalLookup)

  /** @param args implicit ::: List(t1, t2) for a rule {context} t1 ~> t2 */
  private def generateRule(c: symbols.Constant, context: Context, args: List[Term]) {
     val ruleName = c.name / SimplifyTag
     def addSimpRule(r: Rule) {
         val ruleConst = RuleConstant(c.home, ruleName, OMS(c.path), Some(r))
         ruleConst.setOrigin(GeneratedFrom(c.path, this))
         controller.add(ruleConst)
         log("generated rule: " + r.toString)
     }
     val lhs = args.init.last
     val rhs = args.last
     log("generating rule " + ruleName)
     // match lhs to OMA(op, (var,...,var,OMA/OMID,var,...,var))
     lhs match {
       case OuterInnerTerm(names) =>
         if (!utils.subset(utils.inter(rhs.freeVars, context.domain), names.allNames)) {
           throw LocalError("implementation restriction: all free variables of right-hand side must occur in explicit arguments of left-hand side")
         }
         // create and add the rule
         val desc = ruleName.toPath + ": " + present(lhs) + "  ~~>  " + present(rhs)
         val rule = new GeneratedDepthRule(c, desc, under, names, rhs)
         addSimpRule(rule)
         // check if we can also generate a SolutionRule
         generateSolutionRule(c, names, rhs)
      case _ =>
         msc(context, lhs) match {
           case Some(ms) =>
             val rule = new RewriteRule(c.path, context.map(_.name), ms, rhs)
             addSimpRule(rule)
           case None =>
             error(c, "type does not match")
         }
    }
  }

  private def n2V(ns: List[LocalName]) = ns.map(OMV(_))
  /** check if we can also add a solution rule
     the idea is to apply
        outer(before, inner(..., v, ...), after) = v
     in order to transform the problem
        inner(..., v, ...) = t
     into
        v = outer(before, t, after)
  */
  private def generateSolutionRule(c: Constant, names: OuterInnerNames, rhs: Term) {
     // TODO obtain implicit arguments of outer in terms of arguments of inner, currently only trivial case handled
     if (names.implArgsOuter.nonEmpty) return
     rhs match {
        // rhs must be a variable
        case OMV(v) =>
           // v must not occur in bef or aft
           if ((names.before ::: names.after).contains(v)) return
           // v and all variables in bfr and aft must occur exactly once in the arguments to inner
           /** returns the unique position of x in names.inside, -1 otherwise */
           def exactlyAt(x: LocalName) : Int = {
              val xPos = names.inside.indexOf(x)
              if (xPos == -1) -1
              else {
                 val xLaterPos = names.inside.indexOf(x, xPos+1)
                 if (xLaterPos == -1) xPos
                 else -1
              }
           }
           val vPosition = exactlyAt(v)
           val bfrPositions = names.before map exactlyAt
           val aftPositions = names.after map exactlyAt
           if (vPosition :: bfrPositions ::: aftPositions contains -1) return
           val ruleName = c.name / SolutionTag
           log("generating rule " + ruleName)
           val desc = {
              val have = "|- " + present(ApplySpine(OMS(names.inner), n2V(names.inside) :_*)) + " = t"
              val infer = "|- " + v + " = " + present(ApplySpine(OMS(names.outer), n2V(names.before) ::: OMV("t") :: n2V(names.after) :_*))
              s"$ruleName: $have <==> $infer"
           }
           val rule = new GeneratedSolutionRule(c, desc, names, vPosition, bfrPositions, aftPositions)
           val rc = RuleConstant(c.home, ruleName, OMS(c.path), Some(rule)) //TODO nicer type
           rc.setOrigin(GeneratedFrom(c.path, this))
           controller.add(rc)
           log(desc)
        case _ =>
     }
  }
}

/**
 * a simplification rule generated by [[SimplificationRuleGenerator]]
 *
 * @param from the constant giving rise to the rule
 * @param desc string description of the rule
 * @param names structure of the left hand side
 * @param rhs the right hand side
 */
class GeneratedDepthRule(val from: Constant, desc: String, under: List[GlobalName], names: OuterInnerNames, val rhs: Term) extends MatchingSimplificationRule(names.outer) {
    override def toString = desc
    /** timestamp to avoid regenerating this rule when 'from' has not changed */
    val validSince = from.tpC.lastChangeAnalyzed

    /** the groups of indices of equal elements in 'names.before ::: names.inside ::: names.after'
     *  indices are counted from 0
     */
    private val nonlinearityConstraints: List[(LocalName,List[Int])] = {
       val all = (names.before ::: names.inside ::: names.after).zipWithIndex
       val grouped: List[(LocalName,List[(LocalName,Int)])] = all.groupBy(_._1).toList
       grouped.map {
          case (n,l) => (n, l.map(_._2))
       }
    }
    
    private val App = new notations.OMAUnder(under)
    /** object for matching the inner term */
    private class InnerTermMatcher(matchRules: Iterable[InverseOperator]) {
       /**
        * unifies matching OMA, strict OMS, OMS, literals that can be the result of applying a realized operator
        * @return list of matches: arguments, flag signaling whether the inner term is an OMS
        */
       def matches(t: Term): Iterable[(List[Term], Boolean)] = t match {
          case App(OMS(p), inside) if p == names.inner =>
            List((inside, false))
          case OMS(p) if p == names.inner =>
            List((Nil, true))
          case l: OMLIT =>
             // This case inverts any known injective function that returns this literal.
             // That is needed, e.g., to match the literal against the constant zero.
             // It's unclear how helpful it is in general.
             // TODO Can this introduce a cycle?
             matchRules.flatMap {m =>
                if (m.head != names.inner)
                  Nil
                else {
                  m.unapply(l) match {
                     case None =>
                       Nil
                     case Some(args) =>
                       List((args, args.isEmpty))
                  }
                }
             }
          case _ => Nil
       }
    }
   
    def apply(c: Context, rules: RuleSet, t: Term): Simplifiability = {
        val matchRules = rules.get(classOf[InverseOperator])
        val itm = new InnerTermMatcher(matchRules)
        t match {
          case App(OMS(outer),args) if outer == names.outer && args.length == names.outerArity =>
              val (before, arg::after) = args.splitAt(names.innerPos)
              itm.matches(arg) foreach {case (inside, _) =>
                // drop implicit arguments from before, inside, and after
                val (explBf, explAf) = names.explArgsOuter(before, after)
                val explIn = names.explArgsInner(inside)
                // match the remaining explicit arguments against bfrNames, insNames, aftNames
                if (explBf.length == names.before.length &&
                    explAf.length == names.after.length &&
                    explIn.length == names.inside.length) {
                    // for each non-linearity constraint, check that the corresponding elements of 'all' are identical
                    // and if so, add a case to the substitution subs
                   val all = explBf ::: explIn ::: explAf
                   var subs = Substitution()
                   val matchesNonlinearityConstraints = nonlinearityConstraints.forall {case (v, is) =>
                      val first::others = is // Nil impossible by definition of nonlinearityConstraints
                      val t = all(first)
                      // use the first position to extend the substitution
                      subs = subs ++ Sub(v,t)
                      // check that all remaining positions are indeed equal
                      others.forall(i => all(i) hasheq t)
                   }
                   // TODO we also need to substitute for those variables that only occur in implicit arguments
                   // TODO simplification further down the term may make the linearity constraints true without this rule being rechecked
                   if (matchesNonlinearityConstraints) {
                      // subs contains the needed substitution
                      val rhsS = rhs ^? subs
                      return Simplify(rhsS)
                   }
                }
              }
              RecurseOnly(List(under.length+1+names.innerPos)) // under + outer.name + innerPos 
          case _ =>
            Simplifiability.NoRecurse
        }
   }
}

/** tree structure of a [[Term]] optimized for efficient matching */
abstract class MatchStep
/** a fixed term in the template, typically an OMS */
case class CompareTo(pattern: Term, position: Int) extends MatchStep
/** a template variable that is to be matched */
case class SolveVariable(name: LocalName, position: Int) extends MatchStep
/** an OMA whose children must be matched recursively */
case class RecurseIntoOMA(children: List[MatchStep], position: Int) extends MatchStep {
  val childLength = children.length
}
/** an object that does not have to be compared, typically an implicit argument */
case object Skip extends MatchStep

class MatchStepCompiler(lup: Lookup) {
  /** compiles a term into it match steps for use in a [[StepWiseMatcher]] */
  private def applyR(templateVars: Context, template: Term, pos: Int): MatchStep = template match {
    case OMA(f,as) =>
      val rec = (f::as).zipWithIndex map {case (t,p) => applyR(templateVars, t,p)}
      RecurseIntoOMA(rec, pos)
    case OMV(n) => SolveVariable(n, pos)
    case t =>
      if (disjoint(t.freeVars, templateVars.map(_.name)))
        CompareTo(t, pos)
      else
        return null
  }
  def apply(templateVars: Context, template: Term): Option[MatchStep] = Option(applyR(templateVars, template, 0))
}

class RewriteRule(h: GlobalName, templateVars: List[LocalName], templateStep: MatchStep, rhs: Term) extends SimplificationRule(h) {
  private class Solution(val name: LocalName) {
    var value: Option[Term] = None
    def toSub = Sub(name, value.getOrElse(throw ImplementationError("unmatched variable")))
  }
  
  private def eqCheck(t1: Term, t2: Term) = t1 == t2
  
  def apply(context: Context, goal: Term): Simplifiability = {
    // invariant: stepsLeft.length == termsLeft.length
    var stepsLeft: List[MatchStep] = List(templateStep)
    var termsLeft: List[Term] = List(goal)
    var solutions: List[Solution] = templateVars map {n => new Solution(n)}
    while (stepsLeft.nonEmpty) {
      val term = termsLeft.head
      termsLeft = termsLeft.tail
      val step = stepsLeft.head
      stepsLeft = stepsLeft.tail
      step match {
        case CompareTo(pat,pos) =>
          if (eqCheck(termsLeft.head, pat)) {
            termsLeft = termsLeft.tail
          } else {
            return RecurseOnly(List(pos))
          }
        case SolveVariable(n,pos) =>
           solutions.find(_.name == n) match {
             case Some(sol) =>
              sol.value foreach {v =>
                if (!eqCheck(term, v)) {
                  return RecurseOnly(List(pos))
                } else {
                  sol.value = Some(term)
                }
              }
             case None =>
               throw ImplementationError("unknown variable")
           }
        case r: RecurseIntoOMA =>
          term match {
            case OMA(fun,args) if 1+args.length == r.childLength =>
              termsLeft = fun::args:::termsLeft
              stepsLeft = r.children ::: stepsLeft
            case _ =>
              return RecurseOnly(List(r.position))
          }
        case Skip =>
      }
    }
    val subs = Substitution(solutions.map(_.toSub) :_*)
    val res = rhs ^? subs
    Simplify(res)
  }
}

/**
 * a solution rule generated by [[SimplificationRuleGenerator]]
 *
 * names.inside is such that all variables in names.before and names.after
 * as well as the right hand side (which must be a variable) occur in it
 * positions in names.inside are counted from 0
 *
 * @param from the constant giving rise to the rule
 * @param desc string description of the rule
 * @param names structure of the left hand side of the corresponding simplification rule
 * @param vPosition the position of the right hand side in names.inside
 * @param bfrPositions the positions of the names.before in names.inside
 * @param aftPositions the positions of the names.after in names.inside
 */
class GeneratedSolutionRule(from: Constant, desc: String, names: OuterInnerNames,
                            vPosition: Int, bfrPositions: List[Int], aftPositions: List[Int]) extends ValueSolutionRule(names.inner) {
   override def toString = desc
   def applicable(t: Term) : Option[Int] = t match {
      case ApplySpine(OMS(this.head), args) =>
         if (vPosition < args.length)
            Some(vPosition+1)
         else
            None
      case _ => None
   }
   def apply(j: Equality) = j.tm1 match {
      case ApplySpine(_, args) =>
         val explArgs = names.explArgsInner(args)
         val before = bfrPositions.map(i => explArgs(i))
         val after  = aftPositions.map(i => explArgs(i))
         val t      = explArgs(vPosition)
         // TODO build implicit arguments of outer (currently guaranteed to be empty)
         val inverted = ApplyGeneral(OMS(names.outer), before ::: j.tm2 :: after)
         Some((Equality(j.stack, inverted, t, None), "inverting " + names.inner.name))
      case _ => None
   }
}
