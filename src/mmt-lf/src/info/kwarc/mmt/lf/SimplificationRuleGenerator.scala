package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import utils._


//object LF {
//   val path = DPath(utils.URI("http","cds.omdoc.org") / "urtheories") ? "LF"
//   val apply = path ? "apply"
//   val pi = path ? "Pi"
//   val lambda = path ? "lambda"
//   val arrow = path ? "arrow"
//}

class SimplificationRuleGenerator extends RoleHandler {
  case class DoesNotMatch(var msg : String = "") extends java.lang.Throwable(msg)
  def isApplicable(role: String) : Boolean = role == "Simplify"
  def apply(c: symbols.Constant) = {
    // register rule via controller.uom.register
    //TODO
    // error msgs 
    if (! c.tp.isEmpty) {
    	val tm = c.tp.get
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
    	              if (isBefore) throw DoesNotMatch() // only OMVs were detected
    	              //TODO check all var names must be different
    	              val varls = bfr ++ ins ++ aft
    	              val unique = varls.distinct
    	              if (varls.length != unique.length) throw DoesNotMatch()
    	              
    	            	  val simplify = new DepthRule(outer, inr){
    	            	   	private val bfrNames = bfr.reverse
    	            	   	private val aftNames = aft.reverse
    	            	   	private val insNames = ins
    	            	   	def apply = { 
    	            	   	  ((before : List[Term],inside : List[Term],after : List[Term]) 
    	            	   	  => {
    	            	   	    val bfrch = (bfrNames zip before).map {
    	            	   	      case (x,t) => OMV(x) / t
    	            	   	    }
    	            	   	    val aftch = (aftNames zip after).map {
    	            	   	      case (x,t) => OMV(x) / t
    	            	   	    }
    	            	   	    val insch = (insNames zip inside).map {
    	            	   	      case (x,t) => OMV(x) / t
    	            	   	    }
    	            	   	    val subs : Substitution = Substitution((bfrch ++ insch ++ aftch) : _*)
    	            	   	    val qq = t2^subs
    	            	   	    GlobalChange(qq)
    	            	   	  }
    	            	   	) : Rewrite } 	
    	            	  }
    	            	  controller.uom.register(simplify)
    	              
    	            case _ =>
    	          }
    	        }
    	      case _ =>  
    	    }
    	  case _ =>  
    	}
    }
  }
}


object OMVcheck {
  def apply(t : Term) : Boolean = t match {
    case t : OMV => true
//    case OMA(_,args) => args.foldLeft(true)( (a,b) => a && this.apply(b))
    case _ => false
  }
}