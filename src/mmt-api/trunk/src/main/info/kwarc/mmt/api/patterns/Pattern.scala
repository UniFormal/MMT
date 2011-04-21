package info.kwarc.mmt.api.patterns
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects.Context._
import info.kwarc.mmt.api.objects.Substitution._
import info.kwarc.mmt.api.utils._
import scala.io.Source


class Pattern(val home: TheoryObj, val name : LocalName, val params: Context, val con : Context) extends Symbol {
   def toNode =
     <pattern name={name.flat}>
       <parameters>{params.toNode}</parameters>
   	   <declarations>{con.toNode}</declarations>
     </pattern>
     
   def role = info.kwarc.mmt.api.Role_Pattern
   def components = Nil
   override def toString = 
     "Pattern for " + name.flat + " " + params.toString + " " + con.toString
}

class Instance(val home : TheoryObj, val name : LocalName, val pattern : GlobalName, val matches : Substitution) extends Symbol {
   def toNode = 
     <instance name={name.flat} pattern={pattern.toPath}>
     matches.toNode
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
  def elaborate(inst: Instance, lib: Lookup): Context = {
    val pt : Pattern = lib.getPattern(inst.pattern) 
    // TODO There are two getPattern methods now, one should be eliminated.
    pt.con map {
     case TermVarDecl(n,Some(tp),Some(df),at) => TermVarDecl(n,Some(tp ^ inst.matches),Some(df ^ inst.matches),at)
     case TermVarDecl(n,Some(tp),None,at)     => TermVarDecl(n,Some(tp ^ inst.matches),None,at)
     case TermVarDecl(n,None,Some(df),at)     => TermVarDecl(n,None,Some(df ^ inst.matches),at)
     case TermVarDecl(n,None,None,at)         => TermVarDecl(n,None,None,at)
    }
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
  
   
}
//abstract class Nat 
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