package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend._
import objects._
import symbols._
import checking._
import uom._
import utils._
import notations.ImplicitArg

/**
 * convenience class for storing the names of an OuterInnerTerm
 * 
 * @param implArgsOuter the positions of the implicit arguments of 'outer'
 * @param implArgsInner the positions of the implicit arguments of 'inner'
 * 
 * this represents a left hand side of the form
 *   outer(before,inner(inside),after)
 * where before, inside, after do not include the implicit arguments 
 */
case class OuterInnerNames(outer: GlobalName, inner: GlobalName,
                           before: List[LocalName], inside: List[LocalName], after: List[LocalName],
                           implArgsOuter: List[Int], implArgsInner: List[Int]) {
   def explArgsOuter(allBefore: List[Term], allAfter: List[Term]): (List[Term],List[Term]) = {
      val b = allBefore.zipWithIndex.filterNot(p => implArgsOuter.contains(p._2)).map{_._1}
      val a = allAfter .zipWithIndex.filterNot(p => implArgsOuter.contains(p._2 + allBefore.length)).map{_._1}
      (b,a)
   }
   def explArgsInner(allArgs: List[Term]): List[Term] =
      allArgs.zipWithIndex.filterNot(p => implArgsInner.contains(p._2)).map{_._1}
}

class SimplificationRuleGenerator extends ChangeListener {
  override val logPrefix = "simp-rule-gen"
  /** the Tag used to spot constants with name N from which to simplification rules with name N/SimplifyTag */ 
  protected val SimplifyTag = "Simplify"
  protected val SolutionTag = "Solve"
  private def rulePath(r: GeneratedDepthRule) = r.from.path / SimplifyTag
  
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
       	 case FunType(ctx, scp) => 
       	    scp match {
       	      case ApplySpine(OMS(eq), argls) if argls.length >= 2 =>
       	          if (controller.globalLookup.getConstant(eq).rl == Some("Eq"))
       	             generateRule(c, argls)
                   else
                      error(e, "not of eq-args shape")
               case ApplySpine(OMS(ded), List(ApplySpine(OMS(eq), argls))) if argls.length >= 2 =>
                   if (controller.globalLookup.getConstant(ded).rl == Some("Judgment") &&
                       controller.globalLookup.getConstant(eq).rl == Some("Eq")) {
                      generateRule(c, argls)
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
           // we try break args into bfr ::: OMA(inner, ins) ::: aft
           var bfr : List[LocalName] = Nil
           var inner : GlobalName = null
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
           Some(OuterInnerNames(outer, inner, bfr.reverse, ins.reverse, aft.reverse, implArgsOuter, implArgsInner))
         case _ => return None // no outer(...) term
      }
   }

  /** @param args implicit ::: List(t1, t2) for a rule t1 ~> t2 */
  private def generateRule(c: symbols.Constant, args: List[Term]) {
     val t1 = args.init.last
     val t2 = args.last
     val ruleName = c.name / SimplifyTag
     log("generating rule " + ruleName)
     // match lhs to OMA(op, (var,...,var,OMA/OMID,var,...,var))
     t1 match {
       case OuterInnerTerm(n) =>
         // create and add the rule
         val desc = ruleName.toPath + ": " + present(t1) + "  ~~>  " + present(t2)
         val rule = new GeneratedDepthRule(c, desc, n, t2)
         val ruleConst = new RuleConstant(c.home, ruleName, OMS(c.path), Some(rule))
         ruleConst.setOrigin(GeneratedBy(this))
   	   controller.add(ruleConst)
         log(desc)
         // check if we can also generate a SolutionRule
         generateSolutionRule(c, n, t2)
      case _ => error(c, "type does not match")
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
           val rc = new RuleConstant(c.home, ruleName, OMS(c.path), Some(rule)) //TODO nicer type
           rc.setOrigin(GeneratedBy(this))
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
class GeneratedDepthRule(val from: Constant, desc: String, names: OuterInnerNames, val rhs: Term) extends DepthRule(names.outer, names.inner) {
    override def toString = desc
    /** timestamp to avoid regenerating this rule when 'from' has not changed */
    val validSince = from.tpC.lastChangeAnalyzed
    
    /** the groups of indices of equal elements in 'bfrNames ::: insNames ::: aftNames' */ 
    private val nonlinearityConstraints: List[(LocalName,List[Int])] = {
       val all = (names.before ::: names.inside ::: names.after).zipWithIndex
       val grouped: List[(LocalName,List[(LocalName,Int)])] = all.groupBy(_._1).toList
       grouped.map {
          case (n,l) => (n, l.map(_._2))
       }
    }
    
    def apply : Rewrite = { 
        // input term is outer(before, inner(inside), after) 
        (before : List[Term],inside : List[Term],after : List[Term]) => {
          // drop implicit arguments from before, inside, and after
          val (explBf, explAf) = names.explArgsOuter(before, after)
          val explIn = names.explArgsInner(inside)
          // match the remaining explicit arguments against bfrNames, insNames, aftNames
          if (explBf.length != names.before.length || 
              explAf.length != names.after.length || 
              explIn.length != names.inside.length) {
            // mismatch: inequal number
            NoChange
          } else {
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
             if (! matchesNonlinearityConstraints) {
                // mismatch: non-linearity constraints not met
                // TODO simplification further down the term may make the constraints true without this rule being rechecked
                NoChange
             } else {
                // subs contains the needed substitution
                val rhsS = rhs ^? subs
                GlobalChange(rhsS)
             }
          }
        }
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
                            vPosition: Int, bfrPositions: List[Int], aftPositions: List[Int])
   extends SolutionRule(names.inner) {
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
         // TODO generate new unknowns for the implicit arguments of outer 
         val inverted = ApplyGeneral(OMS(names.outer), before ::: j.tm2 :: after)
         Some((Equality(j.stack, inverted, t, None), "inverting " + names.inner.name))
   }
}