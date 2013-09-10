package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import utils._
import parser.ImplicitArg

class SimplificationRuleGenerator2 extends RoleHandler with Logger {
  override val logPrefix = "rule-gen"
  case class DoesNotMatch(msg : String = "") extends java.lang.Throwable(msg)
  def isApplicable(role: String) : Boolean = role == "Simplify"
  def apply(c: symbols.Constant) = {
    if (! c.tp.isEmpty) {
      val tm = c.tp.get
      try {
        val ruleName : String = c.name.toString
    	tm match {
    	  case FunType(ctx, scp) => 
    	    scp match {
    	      case OMSemiFormal(s) => throw DoesNotMatch("failed to parse rule " + ruleName)
    	      // match to OMA(OMID(=), (lhs, rhs))    	      
    	      case ApplySpine(OMID(eq), argls) =>
    	        val t1 = argls(argls.length - 2)
    	        val t2 = argls(argls.length - 1)
    	        if (controller.localLookup.get(eq).role.toString == "Constant:Eq") {
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
    	                case Some(n) => implArgsOut ++= n.flatten(args.length, 0, 0)._2
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
    	            	    	case Some(n) => implArgsIn ++= n.flatten(args.length, 0, 0)._2
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
    	              val uniqVarMap = unique.zipWithIndex.toMap
    	              val bfrSig = bfr.map{uniqVarMap(_)}
    	              val aftSig = aft.map{uniqVarMap(_)}
    	              val insSig = ins.map{uniqVarMap(_)}
    	              
//    	              if (varls.length != unique.length) throw DoesNotMatch("there are some non-unique variables in " + ruleName + " : " + varls)
    	              
 	            	  val simplify = new DepthRule(outer, inr){
    	                
    	                override def toString = ruleName + " : " + controller.presenter.asString(tm) + "  as rule\n" + 
    	                  String.format("%-60s", head.toPath) + " : " + controller.presenter.asString(t1) + " ~~> " + controller.presenter.asString(t2)
 	            	   	private val bfrNames = bfr.reverse
 	            	   	private val aftNames = aft.reverse
 	            	   	private val insNames = ins.reverse
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
 	            	   	    val varMap = (explBf ++ explIn ++ explAf).distinct.zipWithIndex.toMap
 	            	   	    // check if explicit variable signatures coincide with expected signatures
 	            	   	    if (explBf.map{varMap(_)} == bfrSig && 
 	            	   	        explIn.map{varMap(_)} == insSig &&
 	            	   	        explAf.map{varMap(_)} == aftSig) { 
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
 	            	   	      val t2s = t2^subs
 	            	   	      GlobalChange(t2s)
 	            	   	    } else NoChange
 	            	   	    }
 	            	   	  }
 	            	   	} 	
 	            	  }
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
        case e : DoesNotMatch => log(e.msg)
        case e : Throwable => log("unknown error occured in " + c.name + "\nreading tm = " + tm.toString + "\n" + e.toString())
      }
    }
  }
}