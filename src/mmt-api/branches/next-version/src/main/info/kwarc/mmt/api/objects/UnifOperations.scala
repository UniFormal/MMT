package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

import libraries._
import modules._
import symbols._
// import utils._
import frontend._
import objects.Conversions._
import scala.collection.mutable.HashSet


class TypingJudgement(c: Context, tm: Term, T: Option[Term]) {
	var context: Context = c
	var term: Term = tm
	var typeOfTerm: Option[Term] = T
	
	def getType() : Option[Term] = {
	  	return typeOfTerm
	}
	def getTerm() : Term = {
	  	return term
	}
	def getContext() : Context = {
	  	return context
	}
	// creates new objects
	def replaceTerm(t: Term) : TypingJudgement = {
	  	return new TypingJudgement(context, t, typeOfTerm)
	}
}

class EqualityJudgement(c: Context, tm1: Term, tm2: Term, T: Option[Term]) {
	var context: Context = c
	var term1: Term = tm1
	var term2: Term = tm2
	var typeAtEquality: Option[Term] = T
	
	def getFirstTerm() : Term = {
		return term1
	}
	
	def getSecondTerm() : Term = {
		return term2
	}

	def getType() : Option[Term] = {
		return typeAtEquality
	}
	
	def getContext() : Context = {
		return context
	}
	
	def headOfType() : Option[Path]= {
	  	typeAtEquality match  {
	  	  	case Some(tt) => return tt.head
	  	  	case None    => return None 
	  	}
	}
	
	def toTerms() : (Term, Term) = {
		return (tm1,tm2)
	}
	// creates new objects
	def toTypingPair() : (TypingJudgement, TypingJudgement) = {
		return ( (new TypingJudgement(context, term1, typeAtEquality)), 
				 (new TypingJudgement(context, term2, typeAtEquality)))
	}
	
	def toTypingFirst() : TypingJudgement = {
		return ( (new TypingJudgement(context, term1, typeAtEquality)))
	}
	
	def toTypingSecond() : TypingJudgement = {
		return ( (new TypingJudgement(context, term2, typeAtEquality)))
	}
	
	def replaceTerms( x: Term, y: Term) : EqualityJudgement = {
		return (new EqualityJudgement(context, x, y, typeAtEquality))
	}
	
}


object UnificationOperations {
  
	// returns true is x is not in the term
	def occurCheck(variable: Term, tm: Term) : Boolean = {
		println("occurCheck")	
		variable match {
		  	case OMV(name) => 
		  	  	tm match {
		  	  	  	case OMV(name) => false
		  	  	  	case OMID(const) => true
		  	  	  	case OMA(t1, listT) => 
		  	  	  	  	var value = occurCheck(variable, t1)
		  	  	  	    listT.foreach((t: Term) => value = value && occurCheck(variable, t))
		  	  	  		return value
		  	  	  	case OMBIND(binder, context, term) => 
		  	  	  	  	if (context.isDeclared(name))
		  	  	  	  		return true
		  	  	  	  	else
		  	  	  	  		occurCheck(variable, term)
		  	  	  	case _ => false
		  	  	}
		  	case _ => return false
		}
	}
		
	// should always be called after occurCheck b/c otherwise the variable in sub might be in the context of equation
	// NOTE: we don't replace in the type of the term; since replace is called only at the MMT level, so we don't have types
	// there
	def replace(equation: EqualityJudgement, sub: Sub) : EqualityJudgement = {
	  
		println("substiution of meta-variable")
		println((equation.term1^sub).toString)
		println((equation.term2^sub).toString)
		
		return equation.replaceTerms(equation.getFirstTerm()^sub, equation.getSecondTerm()^sub)

	}
	
	// is it necessary? a bit redundant with unifyMMT; we check twice for OMA basically
	def decApp(t: EqualityJudgement) : Option[List[EqualityJudgement]] = {
		t.toTerms() match {
		    case (OMA(x, listx), OMA(y, listy)) =>
		      	if (listx.length == listy.length) {
		  	    	var list = listx.zip(listy)
		  	    	var listEquations = List[EqualityJudgement]()
		  	    	for ((x,y) <- list) {
		  	    		listEquations = t.replaceTerms(x,y) :: listEquations
		  	    	}
		  	    	
		  	    	return Some(listEquations)
		      	}
		      	else
		      		return None
		    case _ => return None
		}
	}
		 
	def decBinder(c1: Context, c2: Context) : Option[(Substitution,List[EqualityJudgement])] = {
		/*
		println("in decBinder")
		println(c1.toString())
		println(c2.toString())
		*/
	  
		var subst : Substitution = Substitution()
		var context : Context = Context()
		var listEquations = List[EqualityJudgement]()
		
		def decBinderRecursive(g1: Context, g2: Context) : Boolean = {
			(g1.components,g2.components) match {
				case ( Nil, Nil) => return true
				case ( (VarDecl(x, aOpt1, None)) :: g1Tail, VarDecl(y, aOpt2, None) :: g2Tail ) =>
					(aOpt1, aOpt2) match {
				  	  	case (None, None) 	 =>	return decBinderRecursive(g1Tail, g2Tail)
				  	  	case (None, Some(a)) => return false
				  	  	case (Some(a), None) => return false
				  	  	case (Some(a1), Some(a2)) =>
					  	  	  	listEquations = (new EqualityJudgement(context, a1, a2^subst, None)) :: listEquations
					  	  	  	subst = Sub("y",OMV("x")) ++ subst
					  	  	  	// the type of variables is not important anymore
					  	  	  	context = VarDecl("x", aOpt1, None) ++ context
					  	  	  	return decBinderRecursive(g1Tail, g2Tail)
				  	}
				  	  	
			}
		}
		
		
		if( c1.length != c2.length) 
			return None
		else {
			if (decBinderRecursive(c1, c2))
			{
				/*
				var h = listEquations.head
				println((h.getContext()).toString())
				println((h.getFirstTerm()).toString())
				println((h.getSecondTerm()).toString())
				println((h.getType()).toString())
				println(subst.toString())
				println()
				*/
				return Some((subst, listEquations))
			}
			else
				return None
		}
	}
	
	
	// the same as the one in the LF file; should I use that one?
	def lookupDef(path : GlobalName)(implicit fl : FoundationLookup) : Option[Term] = fl.getDefiniens(path)
	//def normalize() 
}

