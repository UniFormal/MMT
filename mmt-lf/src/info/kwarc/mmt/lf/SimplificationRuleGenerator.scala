package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import utils._
import notations.ImplicitArg

class SimplificationRuleGenerator extends ChangeListener {
  override val logPrefix = "rule-gen"
  case class DoesNotMatch(msg : String = "") extends java.lang.Throwable(msg)
  override def onUpdate(e: ContentElement) {onAdd(e)}
  override def onAdd(e: ContentElement) {onCheck(e)}
  override def onDelete(p: Path) {
     controller.extman.ruleStore.delete {r => r.path == p}
  }
  override def onClear {
     controller.extman.ruleStore.delete {r => r.isInstanceOf[GeneratedDepthRule]}
  }
  override def onCheck(e: ContentElement) {
       val c = e match {
          case c: symbols.Constant if c.rl == Some("Simplify") =>
             if (c.tpC.analyzed.isDefined) {
                val rules = controller.extman.ruleStore.depthRules.pairs
                // check if an up-to-date rule for this constant exists already: if so break, otherwise delete it
                rules foreach {
                   case (_, r: GeneratedDepthRule) if r.path == c.path => 
                      if (r.validSince >= c.tpC.lastChangeAnalyzed) {
                         log("rule is up-to-date")
                         return
                      } else
                         controller.extman.ruleStore.delete(_ == r)
                   case _ =>
                }
                c
             } else {
                log("not valid, skipped")
                return
             }
          case _ => return
       }
       val tm = c.tpC.analyzed.get
       if (parser.AbstractObjectParser.isOnlyParsed(tm)) {
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
                       controller.globalLookup.getConstant(eq).rl == Some("Eq"))
                      generateRule(c, argls)
                   else
                      error(e, "not of ded-eq-args shape")
               case _ =>
                  error(e, "not a depth rule")
       	    }
           case _ =>
              error(e, "not a depth rule")
       }
  }
  private def error(e: ContentElement, msg: String) {
     logError(e.path + ": " + msg)
  }
  /** @param args implicit ::: List(t1, t2) for a rule t1 ~> t2 */
  private def generateRule(c: symbols.Constant, args: List[Term]) {
     val t1 = args.init.last
     val t2 = args.last
     val ruleName = c.name.toString
     log("generating rule for " + ruleName)
     try {
       // match lhs to OMA(op, (var,...,var,OMA/OMID,var,...,var))
       t1 match {
         case ApplySpine(OMS(outer), args) =>
           // we will try break args into bfr ::: OMA(inr, ins) ::: aft
           var bfr : List[LocalName] = Nil
           var inr : GlobalName = null
           var ins : List[LocalName] = Nil
           var aft : List[LocalName] = Nil
           var isBefore = true // true if we are in bfr, false if we are in aft
           // the implicit arguments of the outer operator
           val implArgsOuter = controller.globalLookup.getConstant(outer).not match {
              case Some(n) => n.arity.flatImplicitArguments(args.length)
              case None => Nil
           }
           // the implicit arguments of the outer operator
           var implArgsInner : List[ImplicitArg] = Nil // will be set once we know what the inner operator is
           // iterate through args to break it up
           args.zipWithIndex.foreach { case (arg, i) =>
              // no need to match the implicit arguments, so skip them
              if (! implArgsOuter.contains(ImplicitArg(i+1))) { 
                 arg match {    	                  
               	  case OMV(x) =>
               	    if (isBefore)
               	       bfr ::= x
               	    else
               	       aft ::= x
               	  case ApplyGeneral(OMS(inner), args) =>
               	    implArgsInner = controller.globalLookup.getConstant(inner).not match {
               	    	   case Some(n) => n.arity.flatImplicitArguments(args.length)
               	    	   case None => Nil
               	    }
               	    if (isBefore) {
                  	    inr = inner
                  	    args.zipWithIndex foreach {case (a,j) =>
                     	    if (! implArgsInner.contains(ImplicitArg(j+1))) {
               	              a match {
               	                case OMV(x) =>
               	                   ins ::= x
               	                case e =>
               	                   throw DoesNotMatch(controller.presenter.asString(e) + " not a variable for inner operation " + controller.presenter.asString(OMID(inner)))
               	              }
               	          }
                  	    }
                  	    isBefore = false
                  	 } else
                  	    throw DoesNotMatch(ruleName + " more than 1 inner operation detected " + args)    	  
                  }
              }
           }
           if (isBefore)
              throw DoesNotMatch("no inner operator detected in " + ruleName)
           // check that all var names are different
           val varls = (bfr ++ ins ++ aft)
           if (varls.length != varls.distinct.length)
              throw DoesNotMatch("there are some non-unique variables in " + ruleName + " : " + varls)
           val desc = ruleName + " simplify " +   
              controller.presenter.asString(t1) + "  ~~>  " + controller.presenter.asString(t2)
           val simplify = new GeneratedDepthRule(outer, inr, c, desc,
              bfr.reverse, ins.reverse, aft.reverse, implArgsOuter, implArgsInner, t2)
      	  controller.extman.ruleStore.add(simplify)
           log("succesfully registered rule: " + ruleName)
         case _ => error(c, "no outer op")
       }
     } catch {
        case e : DoesNotMatch => logError(e.msg)
        case e : Error => logError(e.shortMsg)
     }
  }
}


class GeneratedDepthRule(outer: GlobalName, inner: GlobalName, from: symbols.Constant, desc: String,
       bfrNames: List[LocalName], insNames: List[LocalName], aftNames: List[LocalName],
       implArgsOut: List[ImplicitArg], implArgsIn: List[ImplicitArg], lhs: Term) extends DepthRule(outer, inner){
    override def path = from.path
    override def parent = from.home
    override def toString = desc
    val validSince = from.tpC.lastChangeAnalyzed
    private val implOutInx = implArgsOut map { case ImplicitArg(i) => i-1 }
    private val implInInx = implArgsIn map { case ImplicitArg(i) => i-1 }
    def apply : Rewrite = { 
        (before : List[Term],inside : List[Term],after : List[Term]) => {
          val explBf = before.zipWithIndex.filterNot(p => implOutInx.contains(p._2)).map{_._1}
          val explIn = inside.zipWithIndex.filterNot(p => implInInx.contains(p._2)).map{_._1}
          val explAf = after.zipWithIndex.filterNot(p => implOutInx.contains(p._2 + before.length)).map{_._1}
          if (explBf.length != bfrNames.length || 
              explAf.length != aftNames.length || 
              explIn.length != insNames.length) {
            NoChange
          } else {
             val bfrch = (bfrNames zip explBf).map {
               case (x,t) => OMV(x) / t
             }
             val aftch = (aftNames zip explAf).map {
               case (x,t) => OMV(x) / t
             }
             val insch = (insNames zip explIn).map {
               case (x,t) => OMV(x) / t
             }
             val subs : Substitution = Substitution(bfrch ::: insch ::: aftch : _*)
             val lhsS = lhs^subs
             GlobalChange(lhsS)
          }
        }
   }  
}