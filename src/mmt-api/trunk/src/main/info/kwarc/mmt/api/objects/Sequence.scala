package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import utils._
import libraries._
import modules._
import presentation._
import Conversions._

import scala.xml.{Node}

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
        case con ++ SeqVarDecl(x,tpO,dfO,attrs @_*) => 
           val (conN, sub) = normalizeContext(con) 
           val tpN = tpO map {tp => normalizeSeq(tp ^ sub)}
           val dfN = dfO map {df => normalizeSeq(df ^ sub)}
           (tpN,dfN) match {
              case (Some(FlatSequence(ts)),Some(FlatSequence(ds))) if ts.length == ds.length =>
              	val scon = (List.range(1,ts.length) zip (ts zip ds)).map {
              		case (i,(t,d)) => TermVarDecl(x + "/" + i,Some(t),Some(d))   
                }              	
              	(conN ++ scon, sub ++ SeqSub(x,SeqItemList(scon.map(d => OMV(d.name)))))
              case (Some(FlatSequence(ts)),None) => 
              	val scon = (List.range(1,ts.length) zip ts).map {
              		case (i,t) => TermVarDecl(x + "/" + i,Some(t),None)
              	}
              	(conN ++ scon, sub ++ SeqSub(x,SeqItemList(scon.map(d => OMV(d.name)))))
              case (None,Some(FlatSequence(ds))) =>
              	val scon = (List.range(1,ds.length) zip ds).map {
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
}  

 