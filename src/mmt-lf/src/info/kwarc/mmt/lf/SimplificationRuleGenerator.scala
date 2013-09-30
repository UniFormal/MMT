package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import utils._
import parser.ImplicitArg

class SimplificationRuleGenerator extends ChangeListener {
  override val logPrefix = "rule-gen"
  case class DoesNotMatch(msg : String = "") extends java.lang.Throwable(msg)
  def onAdd(e: ContentElement) {onCheck(e)}
  def onDelete(p: Path) {
     controller.extman.ruleStore.delete {r => r.path == p}
  }
  def onClear {
     controller.extman.ruleStore.delete {r => r.isInstanceOf[GeneratedDepthRule]}
  }
  def onCheck(e: ContentElement) {
    val c = e match {
       case c: symbols.Constant =>
          if (c.rl == Some("Simplify") && c.tpC.analyzed.isDefined &&
                ! controller.extman.ruleStore.depthRules.pairs.exists({case (k,r) => r.path == c.path})) // check if rule exists already
             c
          else {log("not1"); return}
       case _ => log("not"); return
    }
    val tm = c.tpC.analyzed.get
    try {
      val ruleName : String = c.name.toString
    	tm match {
    	  case FunType(ctx, scp) => 
    	    scp match {
    	      case OMSemiFormal(s) => throw DoesNotMatch("failed to parse rule " + ruleName)
    	      // match to OMA(OMID(=), (lhs, rhs))    	      
    	      case ApplySpine(OMS(eq), argls) =>
    	        val t1 = argls(argls.length - 2)
    	        val t2 = argls(argls.length - 1)
    	        if (controller.globalLookup.getConstant(eq).rl == Some("Eq")) {
                // match lhs to OMA(op, (var,...,var,OMA/OMID,var,...,var))
    	          t1 match {
    	            case ApplySpine(OMS(outer), args) =>
    	              var bfr : List[LocalName] = Nil
    	              var inr : GlobalName = null
    	              var ins : List[LocalName] = Nil
    	              var aft : List[LocalName] = Nil
    	              var isBefore = true
    	              var implicitArgs : List[OMV] = Nil
    	              var implArgsOut = List.empty[ImplicitArg]
    	              var implArgsIn = List.empty[ImplicitArg]
    	              controller.globalLookup.getConstant(outer).not match {
    	                case Some(n) => implArgsOut ++= n.arity.flatImplicitArguments(args.length)
    	                case None => Nil
    	              }
    	              val omvs = args.zipWithIndex.map { case (arg, i) =>
    	                if (!implArgsOut.contains(ImplicitArg(i+1))) { 
    	                arg match {    	                  
    	            	  case OMV(x) =>
    	            	    if (isBefore)
    	            	       bfr ::= x
    	            	    else
    	            	       aft ::= x
    	            	  case ApplyGeneral(OMS(inner), args) =>
    	            	    controller.globalLookup.getConstant(inner).not match {
    	            	    	case Some(n) => implArgsIn ++= n.arity.flatImplicitArguments(args.length)
    	            	    	case None => Nil
    	            	    }
    	            	     if (isBefore) {
    	            	        inr = inner
    	            	        args.zipWithIndex foreach { a =>
    	            	            if (!implArgsIn.contains(ImplicitArg(a._2+1))) {
    	            	              a match {
    	            	                case (OMV(x),vi) => ins ::= x
    	            	                case (e, _) => throw DoesNotMatch(controller.presenter.asString(e) + " not a variable for inner operation " + controller.presenter.asString(OMID(inner)))
    	            	              }
    	            	            }
    	            	        }
    	            	        isBefore = false
    	            	     } else
    	            	        throw DoesNotMatch(ruleName + " more than 1 inner operation detected " + args)    	  
    	                }
    	                }
    	              }
    	              if (isBefore) throw DoesNotMatch("no inner operator detected in " + ruleName) // check that inner OMA was detected
    	              // check that all var names are different
    	              val varls = (bfr ++ ins ++ aft)
    	              val unique = varls.distinct
    	              if (varls.length != unique.length) throw DoesNotMatch("there are some non-unique variables in " + ruleName + " : " + varls)
    	              val desc = ruleName + " simplify " +   
                         controller.presenter.asString(t1) + "  ~~>  " + controller.presenter.asString(t2)
    	              val simplify = new GeneratedDepthRule(outer, inr, c, desc,
    	                     bfr.reverse, ins.reverse, aft.reverse, implArgsOut, implArgsIn, t2)
 	            	  controller.extman.ruleStore.add(simplify)
    	              log("succesfully registered rule: " + ruleName)
    	            case _ => throw DoesNotMatch(ruleName + " no outer op")
    	          }
    	        } else throw DoesNotMatch(OMID(eq).toString + " is not of role Eq in " + ruleName)
    	      case _ => throw DoesNotMatch(ruleName + " is not well-formed OMA statement:\n\t" + controller.presenter.asString(scp))
    	    }
    	  case _ => throw DoesNotMatch(ruleName + " not a FunType")
    	}
   } catch {
     case e : DoesNotMatch => logError(e.msg)
     case e : Error => logError(e.shortMsg)
   }
  }
}


class GeneratedDepthRule(outer: GlobalName, inner: GlobalName, from: symbols.Constant, desc: String,
       bfrNames: List[LocalName], insNames: List[LocalName], aftNames: List[LocalName],
       implArgsOut: List[parser.ImplicitArg], implArgsIn: List[parser.ImplicitArg], lhs: Term) extends DepthRule(outer, inner){
    override def path = from.path
    override def parent = from.home
    override def toString = desc
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