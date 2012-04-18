package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import utils._
import libraries._
import modules._
import presentation._
import Conversions._

import scala.xml.{Node}

object Index {
   def apply(seq : Term, term : Term) : Term = OMA(OMS(mmt.index),List(seq,term))
   def unapply(t: Term) : Option[(Term,Term)] = t match {
	   case OMA(OMS(mmt.index), List(seq,i)) => Some((seq,i))
	   case _ => None
   }
}

object SeqMap {
	def apply (seq : Term, i : String, to : Term) : Term = 
	  OMBIND(OMA(OMS(mmt.seqmap),List(to)),Context(TermVarDecl(i,None,None)),seq)
	def unapply(t : Term) : Option[(Term,String,Term)] = t match {
		case OMBIND(OMA(OMS(mmt.seqmap),List(to)),Context(TermVarDecl(i,_,_,_*)),seq) => Some((seq,i,to))
		case _ => None
	}
}

object Rep {
	def apply (seq : Term, to : Term) : Term = 
	  SeqMap(seq,"",to)
	def unapply(t : Term) : Option[(Term,Term)] = t match {
		case SeqMap(seq,"",to) => Some((seq,to))
		case _ => None
	}
}

object Sequence {
  def apply (seq : Term*) : Term = 
    OMA(OMS(mmt.seq),seq.toList)
  def unapply(t : Term) : Option[List[Term]] = t match {
    case OMA(OMS(mmt.seq),args) => Some(args)
    case _ => None
  }
}

object FlatSequence {
  def apply (seq : Term*) : Term = Sequence(seq :_*) 
  def toList(t : Term) : List[Term] = t match {
    case Sequence(args) => args flatMap toList
    case t => List(t)
  }
  def unapply (t : Term) : Option[List[Term]] = Some(toList(t))
}

/*
object SeqNormalize {
  //def normalizeSeq(seq : List[SeqItem]) : List[SeqItem] = seq.flatMap(normalizeSeqItem)      
  def normalizeSeq(seq : Term) : List[Term] = {
	  seq match {
	    case Index(s,i) => Nil
//	    case SeqMap(s,i,n) => Substitution(Sub(i,))
	  } 
      /*
	 	case SeqSubst(s1,n,s2) => 
	 		val ns2 = normalizeSeq(s2)
	 		ns2.items.flatMap {
	 			case e : Term => normalizeSeq(s1 ^ Substitution(TermSub(n,e))).items	 				
	 			case s => List(SeqSubst(normalizeSeq(s1),n,s))
	 		}	 			
	 	case SeqUpTo(m) =>
	 		m match { 
	 			case OMI(n) => 1.to(n.toInt).map(OMI(_)).toList
	 			case _ => List(SeqUpTo(normalizeTerm(m)))
	 		}
    	case SeqVar(n) => List(sit)
	  }
	  */
  }
}
*/

 /*
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
	     //most frequent cases come first for optimization
	     case OMID(p) => OMID(p)
	     case OMV(n) => OMV(n)
        case OMA(fn,args) => 
            val argsN = args.flatMap(normalizeSeqItem)
            if (argsN.isEmpty) normalizeTerm(fn)
            else OMA(normalizeTerm(fn),argsN)
        case OMBIND(bin,con,bdy) =>
            val (conN,sub) = normalizeContext(con)
            if (conN.isEmpty) normalizeTerm(bdy ^? sub)
            else OMBIND(normalizeTerm(bin), conN, normalizeTerm(bdy ^? sub)) // calling ^? to avoid traversal in the typical case where sub is empty   
	 	  case Index(seq,ind) => 
	 	   val seqN = normalizeSeq(seq)
	 	   val indN = normalizeNat(ind)
	 	   (seqN,indN) match {
	 	  	   case (FlatSequence(items),OMI(n)) => items(n.toInt - 1)
	 	  	   case _ => Index(seqN,indN)
	 	   }

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
           val tpN = tpO map {tp => normalizeTerm(tp ^? sub)}
           val dfN = dfO map {df => normalizeTerm(df ^? sub)}
           (conN ++ TermVarDecl(x,tpN,dfN,attrs :_*), sub ++ TermSub(x,OMV(x)))
        case con ++ SeqVarDecl(x,tpO,dfO,attrs @_*) => 
           val (conN, sub) = normalizeContext(con) 
           val tpN = tpO map {tp => normalizeSeq(tp ^ sub)}
           val dfN = dfO map {df => normalizeSeq(df ^ sub)}
           (tpN,dfN) match {
              case (Some(FlatSequence(ts)),Some(FlatSequence(ds))) if ts.length == ds.length =>
              	val scon = (1.to(ts.length).toList zip (ts zip ds)).map {
              		case (i,(t,d)) => TermVarDecl(x + "/" + i,Some(t),Some(d))   
                }              	
              	(conN ++ scon, sub ++ SeqSub(x,SeqItemList(scon.map(d => OMV(d.name)))))
              case (Some(FlatSequence(ts)),None) => 
              	val scon = (1.to(ts.length).toList zip ts).map {
              		case (i,t) => TermVarDecl(x + "/" + i,Some(t),None)
              	}
              	(conN ++ scon, sub ++ SeqSub(x,SeqItemList(scon.map(d => OMV(d.name)))))
              case (None,Some(FlatSequence(ds))) =>
              	val scon = (1.to(ds.length).toList zip ds).map {
              		case (i,d) => TermVarDecl(x + "/" + i,None,Some(d))
              	}
              	(conN ++ scon, sub ++ SeqSub(x,SeqItemList(scon.map(d => OMV(d.name)))))
              case (_,_) => (conN ++ SeqVarDecl(x,tpN,dfN,attrs :_*), sub ++ SeqSub(x,SeqVar(x)))                     
           }
         
        case Context() => (Context(),Substitution()) 
     }
  }
  
  def normalizeNat(t : Term) : Term = {//TODO Complete?
	  t match {
	 	  case OMI(n) => OMI(n)
	 	  /*
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
	 	  case OMATTR(arg,key,value)=> OMATTR(normalizeTerm(arg),key,normalizeTerm(value)) 
	 	  case OMM(arg,via) => OMM(normalizeTerm(arg),via)
	 	  case OME(err, args) => OME(normalizeTerm(err),args.map(normalizeTerm))
	 	  */
	 	  case t => t
	  }
  }
  
  def termToInt(t : Term) : Option[Int] = {
	  t match {
	 	  case OMI(n) => Some(n.toInt)
	 	  case _ => None
	  }
  }	 
*/