package info.kwarc.mmt.api.patterns
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects.Context._
import info.kwarc.mmt.api.objects.Substitution._
import info.kwarc.mmt.api.utils._
import scala.io.Source


// TODO: Adjust code for (optional) contexts for jokers and binders
// TODO: Adjust ReadXML.scala accordingly
class Pattern(val home: TheoryObj, val name : LocalName, val tp : Option[Term], val df: Option[Term]) extends Symbol {
   def toNode =
     <pattern name={name.flat}>
       {if (tp.isDefined) <type>{tp.get.toOBJNode}</type> else Nil}
       {if (df.isDefined) <definition>{df.get.toOBJNode}</definition> else Nil}
     </pattern>
     
   def role = info.kwarc.mmt.api.Role_Pattern
   def components = Nil
   override def toString = 
     "Pattern for " + name.flat + (if (tp.isDefined) " " + tp.get.toString else "")
}

class Instance(val home : TheoryObj, val name : LocalName, val pattern : GlobalName, val matches : List[Match]) extends Symbol {
   def toNode = 
     <instance name={name.flat}>
     {matches map {
       case Length(n) =>
          <match value={n.toString}/> 
       case OMOBJ(tl) =>
          <match>{tl map (t => t.toOBJNode)}</match> 
       }
      }
     </instance>

   def role = info.kwarc.mmt.api.Role_Instance
   def components = Nil
   override def toString = 
     "Instance " + name.flat + " of pattern " + pattern.toString  
}

object PRep {
	def apply(fn: Term, n: Int): Term = OMA(OMID(mmt.repetition),List(fn,OMI(n)))
    def unapply(t: Term) : Option[(Term,Int)] = t match {
		case OMA(mmt.repetition,List(fn,OMI(n))) => Some((fn,n.toInt))
		case _ => None
	}
}

object Pattern {
  def elaborate(inst: Instance, lib: Lookup): Constant = {
    val pat : Pattern = lib.getPattern(inst.pattern) // TODO There are two getPattern methods now, one should be eliminated.
    val t : Option[Term] = pat.tp.map(mergeTerm(_,inst.matches)) // x => merge(x,...)
    /* pat.tp match 
    	case Some(tm) => Some(merge(tm, inst.matches))
    	case None     => None
    	}
       val t : Option[Term] = if (pat.tp.isDefined) Some(merge(pat.tp.get,inst.matches)) else None
    */
    val d : Option[Term] = pat.df.map(mergeTerm(_,inst.matches))     
    new Constant(inst.home,inst.name,t,d,null,Some(inst))
  }

  /**
   * Takes a term with possibly several binders and separates the bound variables from the inner-most body
   * @param tm a openmath term 
   * @param ctx a possibly empty context
   * @return a pair of a term and a context
   */
  def splitTerm(tm: Term, ctx: Context = Context()):(Context,Term) = { //TODO: If we have a fixed joker context, then we dont need splitting.
	  tm match {
	 	  case OMBIND(bin,con,bdy) if bin == mmt.jokerbinder => splitTerm(bdy,ctx ++ con)
	 	  case _ => (ctx,tm)
	  }
  }

  /**
   * Takes a term and a list of matches, 
   * generates a new term by substituting the bound values in the argument term by values in the matches. 
   * @param tm a term with possibly several joker binders on the top of the syntax tree
   * @param matches a list containing a match for every joker variable in tm
   * @return an MMT term where each joker variable is replaced by the corresponding match
   */
  def mergeTerm(tm: Term, matches: List[Match]): Term = {
	 	val (con,bdy) =  splitTerm(tm)	 	
	 	val itm = (con zip matches).foldLeft[Term](bdy) {
	 		case (st,(c,OMOBJ(List(t)))) => 
	 		   val sub = Sub(c.name,t) 
	 		   val subs = Substitution(sub) 
	 		   (st ^ subs)
	 		case (st,(c,OMOBJ(tl))) => 
	 		     substituteList(st,c.name,tl)	 		
	 		case (st,(c,Length(n))) => 
	 		   val sub = Sub(c.name,OMI(n))
	 		   val subs = Substitution(sub)
	 		   expandRepetitionInd(st ^ subs)	     
	 	}
	 	removeIndex(itm) 	
  }
  
  def expandRepetitionInd(tm: Term): Term = {
	  tm match {
	 	  case OMA(fun,args) => 
	 	  val expargs = 
	 	 	  args flatMap {
	 	    	 case OMA(fn,List(f,OMI(n))) if (fn == mmt.repetition) => 
	 	    	 //Currently no nested repetitions, thus no recursion
	 	           List.tabulate[Term](n.toInt)(i => OMA(OMID(mmt.index),List(OMI(i + 1),f)))
	 	    	 case arg => List(arg) //Currently no repetition in binders, thus no case for it
	 	     }
	 	   OMA(fun,expargs)
	 	  case OMBIND(bin,con,bdy) => OMBIND(bin,expandRepetition(con),expandRepetitionInd(bdy))
	 	  case OMATTR(arg,key,value)=> OMATTR(expandRepetitionInd(arg),key,expandRepetitionInd(value))
	 	  case OMM(arg,via) => OMM(expandRepetitionInd(arg),via)
	 	  case OME(err, args) => OME(expandRepetitionInd(err),args.map(expandRepetitionInd))
	 	  case obj => obj 
	  }
  }
  
  def expandRepetition(tm: Term): Term = {
	  tm match {
	 	  case OMA(fun,args) => 
	 	  val expargs = 
	 	 	  args flatMap {
	 	    	 case OMA(fn,List(f,OMI(n))) if (fn == mmt.repetition) => //Currently no nested repetitions, thus no recursion
	 	         List.tabulate[Term](n.asInstanceOf[Int])(_ => f)
	 	    	 case arg => List(arg) //Currently no repetition in binders, thus no case for it
	 	     }
	 	   OMA(fun,expargs)
	 	  case OMBIND(bin,con,bdy) => OMBIND(bin,expandRepetition(con),expandRepetition(bdy))
	 	  case OMATTR(arg,key,value)=> OMATTR(expandRepetition(arg),key,expandRepetition(value))
	 	  case OMM(arg,via) => OMM(expandRepetition(arg),via)
	 	  case OME(err, args) => OME(expandRepetition(err),args.map(expandRepetition))
	 	  case obj => obj 
	  }
  }
  
  def expandRepetition(con: Context): Context = {
	  con.map(
	 		  {case TermVarDecl(n,tp,df,attrs @ _*) => 
	 		   TermVarDecl(n,tp.map(expandRepetition),df.map(expandRepetition),attrs.map(x => (x._1,expandRepetition(x._2))) : _*)
	 		  case v => v
   }
	    		   )
  }
  
  def substituteList(tm: Term, vr:String, tl: List[Term]): Term = {
	   tm match {
	 	  case OMA(mmt.index,List(OMI(i),fn)) => 
	 	    OMA(OMID(mmt.index),List(OMI(i),fn ^ Substitution(Sub(vr,tl(i.toInt - 1)))))
	 	  case OMA(fn,args) => OMA(substituteList(fn,vr,tl),args.map(arg => substituteList(arg,vr,tl)))
	 	  case OMBIND(bin,con,bdy) => OMBIND(bin,substituteList(con,vr,tl),substituteList(bdy,vr,tl))
	 	  case OMATTR(arg,key,value)=> OMATTR(substituteList(arg,vr,tl),key,substituteList(value,vr,tl))
	 	  case OMM(arg,via) => OMM(substituteList(arg,vr,tl),via)
	 	  case OME(err, args) => OME(substituteList(err,vr,tl),args.map(substituteList(_,vr,tl)))
	 	  case obj => obj
	 	  }
  }
   
  def substituteList(con: Context, vr:String, tl: List[Term]): Context = {
	  con.map(
	 		  {case TermVarDecl(n,tp,df,attrs @ _*) => 
	 		   TermVarDecl(n,tp.map(substituteList(_,vr,tl)),df.map(substituteList(_,vr,tl)),attrs.map(x => (x._1,substituteList(x._2,vr,tl))) : _*)
	 		  case v => v
	 		  }
	 		   )
  }
   
  def removeIndex(tm:Term): Term = {
	   tm match {
	  	   case OMA(OMID(mmt.index),List(OMI(i),fun)) => fun
	  	   case OMA(fn,args) => OMA(removeIndex(fn),args.map(removeIndex))
	  	   case OMBIND(bin,con,bdy) => OMBIND(bin,removeIndex(con),removeIndex(bdy))
	 	   case OMATTR(arg,key,value)=> OMATTR(removeIndex(arg),key,removeIndex(value))
	 	   case OMM(arg,via) => OMM(removeIndex(arg),via)
	 	   case OME(err, args) => OME(removeIndex(err),args.map(removeIndex))
	 	   case obj => obj
	   }
  }
   
  def removeIndex(con: Context): Context = {
	  con.map(
	 		  {case TermVarDecl(n,tp,df,attrs @ _*) => 
	 		   TermVarDecl(n,tp.map(removeIndex),df.map(removeIndex),attrs.map(x => (x._1,removeIndex(x._2))) : _*)
	 		  case v => v
	 		  }
	 		   )
  }
  
  /*
  
  def patternCheck(c: Constant, lib: Lookup): Option[Instance] = {
        val pts = getPatterns(c.parent,lib)
        pts.map(pt => try {check(pt,c.tp,c.df)} catch {case NoMatch => None})
	    //Instance(parent : MPath, name : LocalPath, val pattern : SPath, val matches : List[Match])
        None
  }
   
  def getPatterns(parent: MPath, lib: Lookup): List[Pattern] = { //TODO Finish the method
	   val t = lib.getTheory(parent)
	   if (t.meta == None) Nil else
	   //TODO Finish 
	   Nil
  }
   
  def check(pt: Pattern, tm: Option[Term], df: Option[Term]): Option[Instance] = {
	   (tm,pt.tp) match {
	  	   //case (None,None) => 
	  	   //case (None,Some) =>
	  	   case (Some(t),None) => None 
	       case (Some(t),Some(p)) => checkTerm(t,Context(),p,Context())(pt.con)
	   }
	   (pt.df,df) match {
	  	   //case (None,None) => 
	  	   case (None,Some(t)) => None
	  	   //case (Some,None) => 
	  	   case (Some(t),Some(p)) => checkTerm(t,Context(),p,Context())(pt.con) 
	   }
	    None 
  }
       
  def checkTerm(tm: Term, tcon: Context, ptm: Term, pcon: Context)(implicit jcon: Context): List[Sub] = {
	   (tm,ptm) match {
	  	   case (OMS(p),OMS(s)) => if (p == s) Nil else throw NoMatch
	  	   case (OMV(m),OMV(n)) if (pcon.isDeclared(n)) => 
	  	     if (index(m) == index(n)) Nil
	  	     else throw NoMatch
	  	   case (om,OMV(n)) => jcon(n) match {
	  	  	   case TermVarDecl(_,Some(tp),None,_*) => Sub(OMV(n),om) //TODO how does tp look like? 
	  	  	   // joker vars must have types and do not have definiens  
	  	   }
	  	   case (OMA(fn,ar),OMA(fm,ap)) => checkTerm(fn,tcon,fm,pcon) ::: checkList(ar,tcon,ap,pcon) 
	   	   case (OMBIND(bn,cn,bd),OMBIND(bm,cm,bc)) => checkTerm(bn,tcon,bm,pcon) ::: checkTerm(bd,tcon ++ cn,bc,pcon ++ cm)
	  	   case _ => throw NoMatch  
	   }
  }
   
  def checkList(lt1: List[Term], con1: Context, lt2: List[Term], con2: Context)(implicit jcon: Context): List[Sub] = { //TODO Return substitutions in all cases
	   (lt1,lt2) match {
	  	   case (Nil,Nil) => Nil
	  	   case (Nil, PRep(fn,n) :: m) => checkList(Nil,con1,m,con2) // Length(n,0) 
	  	   case (Nil, _)  => throw NoMatch
	  	   case (l, Nil)  => throw NoMatch
	       case (a :: l, PRep(fn,n) :: m) =>
	         try {
	        	 val lm1 = checkTerm(a,con1,fn,con2)
	        	 val lm2 = checkList(l,con1,lt2,con2)
	        	 lm1 ::: lm2  // Length(n+1,0)?????
	         } catch {
	        	 case NoMatch => checkList(lt1,con1,m,con2) // Length(n,0)
	         }
	       case (a :: l, e :: m) => 
	        val lt1 = checkTerm(a,con1,e,con2)
	        val lt2 = checkList(l,con1,m,con2)
	        lm1 ::: lm2 
	   }
  }
	     
  */
   
}

abstract class Match
case class Length(n : Int) extends Match
case class OMOBJ(tl : List[Term]) extends Match

case object NoMatch extends java.lang.Throwable 

/*
object Test {
	val f1 = OMS()
	val v1 = OMV("x")
	def main(args : Array[String]) {
		print(Pattern.checkTerm(OMA(f,List(v1))).toString)
	}
}
}
*/