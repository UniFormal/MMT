package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import utils._
import libraries._
import modules._
import presentation._
import Conversions._

import scala.xml.{Node}

/*
object Succ {
   def apply(n: Nat) : Nat = n + 1
   def unapply(n:Nat) : Option[Nat] = if (n > 0) Some(n - 1) else None 
}
*/

object FlatSequence {
	def unapply(s: Sequence) : Option[List[Term]] = s.items match {
		case Nil => Some(Nil)
		case hd :: tl => hd match {
			case t: Term => unapply(SeqItemList(tl)).map(t :: _)
			case _ => None
		}
	}
}

object Ind {
	def apply(seq : String, i : String) : Term = Index(SeqVar(seq),OMV(i))

}

object Rep {
	def apply(tp : Term, n : Term): SeqItem = SeqSubst(tp, "i", SeqUpTo(n)) 
}

object RepInt {
	def apply(fn : Term, n : Int): SeqItem = Rep(fn, OMI(n))
}

object normalize {
  //def normalizeSeq(seq : List[SeqItem]) : List[SeqItem] = seq.flatMap(normalizeSeqItem)      
  def normalizeSeq(seq : Sequence) : Sequence = SeqItemList(seq.items.flatMap(normalizeSeqItem))	  
  def normalizeSeqItem(sit : SeqItem) : List[SeqItem] = {
	  //TODO Check normalization against the inference rules
	  sit match {	
	 	case SeqSubst(s1,n,s2) => 
	 		val ns2 = normalizeSeq(s2)
	 		ns2.items.flatMap {
	 			case e : Term => normalizeSeq(s1 ^ Substitution(TermSub(n,e))).items	 				
	 			case s => List(SeqSubst(normalizeSeq(s1),n,s))
	 		}	 			

	 	case SeqUpTo(m) =>
	 		m match { 
	 			case OMI(n) => List.range(1,n.toInt).map(i => OMI(i))
	 			case _ => List(SeqUpTo(normalizeTerm(m)))
	 		}
    	case SeqVar(n) => List(sit)
    	case e : Term => List(normalizeTerm(e))
	  }
	  
	 	/*case e : Term =>
	 	    		s2 match {
	 	    			case SeqItemList(items) =>
	 	    				val en = normalizeTerm(e)
	 	    				items.flatMap(i => normalizeSeqItem(SeqSubst(en,n,i)))
	 	    			case tm : Term => List(normalizeTerm(e) ^ Substitution(TermSub(n,normalizeTerm(tm))))
	 	    			case SeqVar(m) => List(SeqSubst(normalizeTerm(e),n,SeqVar(m)))
	 	    			case SeqSubst(ep,np,sp) => normalizeSeqItem(SeqSubst(e,n,normalizeSeq(SeqSubst(ep,np,sp)))) 	 	  	     
	 	    			case SeqUpTo(m) =>
	 	    				val mn = normalizeTerm(m)
	 	    				val en = normalizeTerm(e)
	 	    				mn match {
	 	    					case OMI(i) =>
	 	    						val l = List.range(1,i.toInt)	  
	 	    						l.map(x => en ^ Substitution(TermSub(n,OMI(x))))
	 	    					case _ => List(SeqSubst(en,n,mn))
	 	    				}	 	  	     
	 	    		}
	 			case SeqItemList(items) => 
	 				val ns2 = normalizeSeq(s2)
	 					items.flatMap(normalizeSeqItem(_)).map {
	 						case e : Term => ns2.items.map {
	 							case x : Term => e ^ Substitution(TermSub(n,x))
	 							case  _ => e
	 						}
	 						case _ => this
	 					}
	 			case SeqSubst(s1,n,s2) => List(SeqSubst(normalizeSeq(s1),n,normalizeSeq(s2)))
	 			case _ => throw Invalid("normalization failed")
	 			
	 		}
	 	*/
	 	//case SeqUpTo(OMI(n)) => List.range(1,n.toInt).map(i => OMI(i))
	    //case SeqUpTo(e) => List(SeqUpTo(normalizeTerm(e)))		
	   /*
    	case SeqSubst(e,n,SeqSubst(e',m,s)) => 
    	   normalizeSeqItem(SeqSubst(e',m,s)).map(t => normalizeTerm(e ^ Substitution(Sub(n,t))))
	 	case SeqSubst(e,n,tm : Term) => List(normalizeTerm(e) ^ Substitution(TermSub(n,normalizeTerm(tm)))	 	
	 	case SeqSubst(e,n,SeqVar(m)) => List(SeqSubst(normalizeTerm(e),n,SeqVar(m)))
	 	case SeqSubst(e,n,SeqItemList(items)) => 
	 	   val en = normalizeTerm(e)
	 	   items.flatMap(i => normalizeSeqItem(SeqSubst(en,n,i)))
	 	case SeqSubst(e,n,SeqUpTo(m)) =>
	 	   val mn = normalizeTerm(m)
	 	   val en = normalizeTerm(e)
	 	   mn match {
	 	  	   case OMI(i) =>
	 	 	   	  val l = List.range(1,i.toInt)	  
	 	  	   	  l.map(x => en ^ Substitution(TermSub(n,OMI(x))))
	 	  	   case _ => List(SeqSubst(en,n,mn))
	 	   }
	 	*/  
  }

  def normalizeTerm(tm : Term) : Term = {
	  tm match {
	 	 /* 
	 	 case Ind(ind, seq) =>
	 	     val indN = normalizeNat(ind)
	 	     val seqN = seq.flatMap(normalizeSeqItem)
	 	     (indN, seqN) match {
	 	    	 case (OMI(n),tms : List[Term]) => tms(n.toInt)
	 	    	 case _ => Ind(indN, seqN)
	 	     }
	 	  */  
	 	  case Index(seq,ind) => 
	 	   val seqN = normalizeSeq(seq)
	 	   val indN = normalizeNat(ind)
	 	   (seqN,indN) match {
	 	  	   case (FlatSequence(items),OMI(n)) => items(n.toInt)
	 	  	   case _ => Index(seqN,indN)
	 	   }

          case OMA(fn,args) => 
          	val argsN = args.flatMap(normalizeSeqItem)
          	if (argsN.isEmpty) normalizeTerm(fn)
          	else OMA(normalizeTerm(fn),args.flatMap(normalizeSeqItem))
	 	  case OMBIND(bin,con,bdy) =>
	 	      val (conN,sub) = normalizeContext(con)
	 	      OMBIND(normalizeTerm(bin), conN, normalizeTerm(bdy ^ sub))	 	  
	 	  case OMATTR(arg,key,value)=> OMATTR(normalizeTerm(arg),key,normalizeTerm(value)) //TODO normalize method for key?
	 	  case OMM(arg,via) => OMM(normalizeTerm(arg),via)
	 	  case OME(err, args) => OME(normalizeTerm(err),args.map(normalizeTerm))
	 	  case obj => obj //TODO cases
	  }
  }
  
  def normalizeContext(context : Context) : (Context,Substitution) = {
     context match {
        case con ++ TermVarDecl(x,tpO,dfO,attrs @_*) => 
           val (conN, sub) = normalizeContext(con) 
           val tpN = tpO map {tp => normalizeTerm(tp ^ sub)}
           val dfN = dfO map {df => normalizeTerm(df ^ sub)}
           (conN ++ TermVarDecl(x,tpN,dfN,attrs :_*), sub ++ TermSub(x,OMV(x)))
         /*  
        case con ++ SeqVarDecl(x,tpO,dfO,attrs @_*) => 
           val (conN, sub) = normalizeContext(con) 
           val tpN = tpO map {tp => normalizeSeq(tp ^ sub)}
           val dfN = dfO map {df => normalizeSeq(df ^ sub)}
           (tpN,dfN) match {
              case (FlatSequence(ts),FlatSequence(ds)) if ts.length == ds.length =>
                 (List.range(1,ts.length) zip (ts zip ds)).map {
                    case (i,(t,d)) => TermVarDecl(x + "/" + i,t,d)   
                 }
              case (_,_) => (conN ++ SeqVarDecl(x,tpN,dfN,attrs :_*), sub ++ SeqSub(x,OMSV(x)))                     
           }
         */
        case Context() => (Context(),Substitution()) 
     }
  }
  
  def normalizeNat(t : Term) : Term = {
	  t match {
	 	  case OMI(n) => OMI(n)
	 	  case t => t
	  }
  }
  
  def termToInt(t : Term) : Option[Int] = {
	  t match {
	 	  case OMI(n) => Some(n.toInt)
	 	  case _ => None
	  }
  }
	 
}  

 
  
 /*(
  def expandSeq(seq : Term) : List[Term] = {
	seq match {
		case Ellipsis(tm,i,OMI(a),OMI(z)) => 
		List.range(a.intValue,z.intValue).map(x => tm ^ Substitution(Sub(i,OMI(x))))
		case _ => throw IllTerm //TODO Do the remaining cases.
	}
  }

  
  def removeIndex(seq : Term, ind : Term) : Term = {
	  ind match {
	       case OMI(i) => expandSeq(seq)(i.intValue)
	       case _ => throw IllTerm
	       }
  }
*/
  
/*
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
 */

/*
object Ellipsis {
	def apply(body : Term, name : String, from : Term, to : Term) : Term =
		OMBIND(OMA(OMID(mmt.ellipsis),List(from,to)),Context(TermVarDecl(name,Some(OMID(mmt.nat)),None,null)), body)
	def unapply(t : Term) : Option[(Term,String,Term,Term)] = 
		t match {
		case OMBIND(OMA(OMID(mmt.ellipsis),List(k,l)),Context(TermVarDecl(i,Some(OMID(mmt.nat)),None,null)),tm) => 
		  Some((tm,i,k,l))
		case _ => None
	}
}
*/
/*
abstract class SeqItem 
case class Ellipsis(expr : Term, i : String, from : Term, to : Term) extends SeqItem 
case class SeqVar(name : String) extends SeqItem
case class SeqTerm(tm : Term) extends SeqItem
case class Sequence(items : SeqItem*) extends Obj {
	 def ::(tm : Term) : Sequence = Sequence(SeqTerm(tm) +: items : _*)
}
*/
/*
object Seq {
	def apply(seq : Term*) = OMA(OMID(mmt.seq),seq.toList)
	def unapplySeq(tm : Term) : Option[Seq[Term]] = 
		tm match {
		case OMA(OMID(mmt.seq),l) => Some(l)
		case _ => None 
	}
}
*/
/*
object Seq {
	def apply(args : Term*) = 
		//val l = args.toList.length
		SeqTerm(args.index)	
*/
/*
case class Index(index : Term, seq : Sequence) extends Term {
	def head = None
	def role = Role_value
	//def components = List(ind,seq) TODO: add compinents
}
*/	
	/*
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
	 	    	 case OMA(fn,List(f,OMI(n))) if (fn == mmt.repetition) => 
	 	    	 //Currently no nested repetitions, thus no recursion
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
  
  def removeIndex(tm : Term) : Term = {
	   tm match {
	  	   case OMA(OMID(mmt.index),List(OMI(i),fn)) => fn
	  	   case OMA(fn,args) => OMA(removeIndex(fn),args.map(removeIndex))
	  	   case OMBIND(bin,con,bdy) => OMBIND(bin,removeIndex(con),removeIndex(bdy))
	 	   case OMATTR(arg,key,value)=> OMATTR(removeIndex(arg),key,removeIndex(value))
	 	   case OMM(arg,via) => OMM(removeIndex(arg),via)
	 	   case OME(err, args) => OME(removeIndex(err),args.map(removeIndex))
	 	   case obj => obj
	   }
  }
  
  def removeIndex(con : Context) : Context = {
	  con.map(
	 		  {case TermVarDecl(n,tp,df,attrs @ _*) => 
	 		   TermVarDecl(n,tp.map(removeIndex),df.map(removeIndex),attrs.map(x => (x._1,removeIndex(x._2))) : _*)
	 		  case v => v
	 		  }
	 		   )
  }
  */
// case object IllTerm extends java.lang.Throwable