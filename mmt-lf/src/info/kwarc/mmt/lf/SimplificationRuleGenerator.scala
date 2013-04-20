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
    	            	          case _ => throw DoesNotMatch()
    	            	        }
    	            	        isBefore = false
    	            	     } else
    	            	        throw DoesNotMatch()    	  
    	              }
    	              if (isBefore) throw DoesNotMatch() // check that inner OMA was detected
    	              // check that all var names are different
    	              val varls = bfr ++ ins ++ aft
    	              val unique = varls.distinct
    	              if (varls.length != unique.length) throw DoesNotMatch("there are some non-unqique variables")
    	              
 	            	  val simplify = new DepthRule(outer, inr){
    	                val ruleName : String = c.name.toString
    	                override def toString = String.format("%-60s", head.toPath) + " of " + ruleName
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
    	              
    	            case _ => // no match
    	          }
    	        }
    	      case _ => // no match 
    	    }
    	  case _ => // impossible case
    	}
      } catch {
        case e : DoesNotMatch => log(tm.toString + "does not match Simplification rule pattern")
        case e : Throwable => throw(e)
      }
    }
  }
}