package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import utils._

class SimplificationRuleGenerator extends RoleHandler with Logger {
  override val logPrefix = "rule-gen"
  case class DoesNotMatch(msg : String = "") extends java.lang.Throwable(msg)
  def isApplicable(role: String) : Boolean = role == "Simplify"
  def apply(c: symbols.Constant) {
    //TODO error msgs 
    if (! c.tp.isEmpty) {
      val tm = c.tp.get
      try {
        val ruleName : String = c.name.toString
    	tm match {
    	  case FunType(ctx, scp) => 
    	    scp match {
    	      // match to OMA(OMID(=), (lhs, rhs))
    	      case ApplySpine(OMID(eq), List(t1, t2)) =>
    	        if (controller.localLookup.get(eq).role.toString == "Constant:Eq") {
                // match lhs to OMA(op, (var,...,var,OMA/OMID,var,...,var))
    	          t1 match {
    	            case ApplySpine(OMS(outer), args) =>
    	              var bfr : List[LocalName] = Nil
    	              var inr : GlobalName = null
    	              var ins : List[LocalName] = Nil
    	              var aft : List[LocalName] = Nil
    	              var isBefore = true
    	              val omvs = args.map {
    	            	  case OMV(x) =>
    	            	    if (isBefore)
    	            	       bfr ::= x
    	            	    else
    	            	       aft ::= x
    	            	  case ApplyGeneral(OMS(inner), args) =>
    	            	     if (isBefore) {
    	            	        inr = inner
    	            	        ins = args map {
    	            	          case OMV(x) => x
    	            	          case _ => throw DoesNotMatch(ruleName + " not a variable for inner operation")
    	            	        }
    	            	        isBefore = false
    	            	     } else
    	            	        throw DoesNotMatch(ruleName + " more than 1 inner operation detected")    	  
    	              }
    	              if (isBefore) throw DoesNotMatch("no inner operator detected in " + ruleName) // check that inner OMA was detected
    	              // check that all var names are different
    	              val varls = bfr ++ ins ++ aft
    	              val unique = varls.distinct
    	              if (varls.length != unique.length) throw DoesNotMatch("there are some non-unique variables in " + ruleName)
    	              
 	            	  val simplify = new DepthRule(outer, inr){
    	                
    	                override def toString = ruleName + " : " + controller.presenter.asString(t1) + " ~~> " + controller.presenter.asString(t2)
 	            	   	private val bfrNames = bfr.reverse
 	            	   	private val aftNames = aft.reverse
 	            	   	private val insNames = ins
 	            	   	def apply : Rewrite = { 
 	            	   	  (before : List[Term],inside : List[Term],after : List[Term]) => { 
 	            	   	    val bfrch = (bfrNames zip before).map {
 	            	   	      case (x,t) => OMV(x) / t
 	            	   	    }
 	            	   	    val aftch = (aftNames zip after).map {
 	            	   	      case (x,t) => OMV(x) / t
 	            	   	    }
 	            	   	    val insch = (insNames zip inside).map {
 	            	   	      case (x,t) => OMV(x) / t
 	            	   	    }
 	            	   	    val subs : Substitution = Substitution(bfrch ::: insch ::: aftch : _*)
 	            	   	    val qq = t2^subs
 	            	   	    GlobalChange(qq)
 	            	   	  }
 	            	   	} 	
 	            	  }
 	            	  controller.extman.ruleStore.add(simplify)
    	              
    	            case _ => throw DoesNotMatch(ruleName + " no outer op")
    	          }
    	        }
    	      case _ => throw DoesNotMatch(ruleName + "equality of not role Eq")
    	    }
    	  case _ => throw DoesNotMatch(ruleName + " not a FunType")
    	}
      } catch {
        case e : DoesNotMatch => log(e.msg)
        case e : Throwable => log("unknown error occured, crashlanding..." + e.toString())
      }
    }
  }
}