package info.kwarc.mmt.lfs

import info.kwarc.mmt.api._
import utils._
import libraries._
import modules._
import presentation._
import objects._
import Conversions._

import info.kwarc.mmt.lf._
import scala.xml.{Node}


object Index {
   def apply(seq : Term, index : Term) : Term = OMA(OMS(LFS.index),List(seq,index))
   def unapply(t: Term) : Option[(Term,Term)] = t match {
	   case OMA(OMS(LFS.index), List(seq,i)) => Some((seq,i))
	   case _ => None
   }
}

object SeqMap {
	def apply (seq : Term, index : LocalName, to : Term) : Term = 
	  OMBIND(OMA(OMS(LFS.seqmap),List(to)),Context(VarDecl(index,None,None)),seq)
	def unapply(t : Term) : Option[(Term,LocalName,Term)] = t match {
		case OMBIND(OMA(OMS(LFS.seqmap),List(to)),Context(VarDecl(index,_,_,_*)),seq) => Some((seq,index,to))
		case _ => None
	}
}

object Rep {
	def apply (seq : Term, to : Term) : Term = 
	  SeqMap(seq,LocalName.Anon,to)
	def unapply(t : Term) : Option[(Term,Term)] = t match {
		case SeqMap(seq,LocalName.Anon,to) => Some((seq,to))
		case _ => None
	}
}

object Sequence {
  def apply (seq : Term*) : Term = seq.length match {
    case 0 => OMS(LFS.seq)
    case _ => OMA(OMS(LFS.seq),seq.toList)
  }  
  def unapply(t : Term) : Option[List[Term]] = t match {
    case OMA(OMS(LFS.seq),args) => Some(args)
    case OMS(LFS.seq) => Some(Nil)
    case _ => None
  }
}

object FlatSequence {
  def apply (seq : Term*) : Term = {
    val l : List[Term] = seq.toList flatMap toList
    l match {
      case List(t) => t
      case l => Sequence(l:_*)
    }
  }
  def toList(t : Term) : List[Term] = t match {
    case Sequence(args) => args flatMap toList
    case t => List(t)
  }
  def unapply (t : Term) : Option[List[Term]] = Some(toList(t))
}

/*
 * normalizes an sequence expression/symbol in OMDoc/MMT to the corresponding sequence of OMDoc/MMT  
 */
object SeqNormalize {   
  def length(tm : Term)(implicit lookup : Lookup, context : Context) : Term = {
    tm match {
      case t @ SeqMap(seq,index,to) => 
        val len = length(normalize(seq))
        val toN = normalize(to)
        (len,toN) match {
          case (OMI(m),OMI(n)) => OMI(m * n)
          case _ => t
        }
      case t @ Sequence(seq) => 
        val len = seq.map(s => normalize(length(s)))
        len.tail.foldLeft(len.head){case (x,y) => Add(x,y)}         
      case t @ OMA(Lambda(x,tp,body),args) =>
        val len = length(normalize(tp))
        len match {
          case OMI(n) if args.length == n => length(body ^ (OMV(x) / Sequence(args : _*)))
          case _ => t
        }      
      case OMBIND(fn,ctx,body) => length(body)
      case _ => OMI(1) //TODO Finish all cases
      
    }
  }
  def normalize(t : Term)(implicit lookup : Lookup, context : Context) : Term = {
	  t match {
	    case SeqMap(seq,index,to) => 	      	      
	      val toN = normalize(to)
	      toN match {
	        case OMI(n) => 
	          val l = (0 until n.toInt) map {
	            i => normalize(seq ^ (OMV(index) / OMI(i))) //Substitution(Sub(index,i))	            
	          }
	          Sequence(l : _*)
	        case _ => SeqMap(normalize(seq),index,toN)    
	      }
	    case Sequence(seq) => FlatSequence(seq map normalize : _*)
	    case Index(seq,index) => 
	      val seqN = normalize(seq)
	      val indexN = normalize(index)
	      val len = length(seqN)
	      (seqN,indexN,len) match {
	         case (Sequence(ts),OMI(i),OMI(n)) if ts.length == n => ts(i.toInt - 1)
	         case (_,_,_) => Index(seqN,indexN)
	      }
	    case Add(s,t) => 
	      val sN = normalize(s) 
	      val tN = normalize(t)
	      (sN,tN) match {
	        case (OMI(m),OMI(n)) => OMI(m + n)
	        case _ => Add(sN,tN)
	      }
	    case Subtract(s,t) => 
	      val sN = normalize(s) 
	      val tN = normalize(t)
	      (sN,tN) match {
	        case (OMI(m),OMI(n)) => OMI(m - n)
	        case _ => Subtract(sN,tN)
	      }
	    case Multiply(s,t) => 
	      val sN = normalize(s) 
	      val tN = normalize(t)
	      (sN,tN) match {
	        case (OMI(m),OMI(n)) => OMI(m * n)
	        case _ => Multiply(sN,tN)
	      }   
	    case OMATTR(arg,key,value) => OMATTR(normalize(arg),key,normalize(value))
	    case OMM(arg,via) => OMM(normalize(arg),normalize(via)) 
	    case OMA(func,args) => OMA(normalize(func),args map normalize)
	    case OME(err, args) => OME(normalize(err),args map normalize)
	    case OMBIND(bin,con,body) => 
	      val (conN,sub) = normalize(con)
	      OMBIND(normalize(bin),conN,normalize(body ^ sub))
	    case t => t
	  }       	    
  }
  
  def normalize(cont : Context)(implicit lookup : Lookup, context : Context) : (Context,Substitution) = {
    cont match {
      case con ++ VarDecl(x,tpO,dfO,attrs @_*) =>
        val (conN, sub) = normalize(con)
        val tpN = tpO map {tp => normalize(tp ^ sub)}
        val dfN = dfO map {df => normalize(df ^ sub)}                
        val (vds,t) = (tpN,dfN) match {
          case (Some(Sequence(tps)),None) => 
            val lpair = tps.zipWithIndex map {
              case (tp,i) => (VarDecl(x / i.toString,Some(tp),None,attrs :_*),OMV(x / i.toString))
            }
            val (vds,vs) = lpair.unzip
            (vds,Sequence(vs :_*))            
          case (None,Some(Sequence(dfs))) => 
            val lpair = dfs.zipWithIndex map {
              case (df,i) => (VarDecl(x / i.toString,None,Some(df),attrs :_*),OMV(x / i.toString))
            }
            val (vds,vs) = lpair.unzip
            (vds,Sequence(vs :_*))            
          case (None,None) => (List(VarDecl(x,None,None,attrs :_*)),OMV(x))
          case (Some(Sequence(tps)),Some(Sequence(dfs))) => (List(VarDecl(x,tpN,dfN,attrs :_*)),OMV(x)) //TODO Fix this case.
          case (_,_) => (List(VarDecl(x,tpN,dfN,attrs :_*)),OMV(x))
        }        
        (conN ++ vds, sub ++ OMV(x) / t)
      case Context() => (Context(),Substitution())
    }
  }
  
}
