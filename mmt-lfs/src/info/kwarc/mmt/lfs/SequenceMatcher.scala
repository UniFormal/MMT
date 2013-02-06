package info.kwarc.mmt.lfs

import info.kwarc.mmt.api._
import frontend._
import libraries._
import modules._
import objects._
import patterns._
import presentation._
import utils._
import Conversions._

import info.kwarc.mmt.lf._
import SeqNormalize._

import scala.xml.{Node}

class SequenceMatcher(controller : Controller, var metaContext : Context) {
  def apply(seq1 : Term, seq2 : Term, con : Context = Context()) : Boolean = {
    val lookup = controller.globalLookup
    val seq1N = normalize(seq1)(lookup,con)
    val seq2N = normalize(seq2)(lookup,con)    
    val len1N = normalize(length(seq1)(lookup,con))(lookup,con) 
    val len2N = normalize(length(seq2)(lookup,con))(lookup,con)
    (len1N,len2N) match {
      case (OMI(m),OMI(n)) if (m == n) => 
        (seq1N,seq2N) match {
          case (Sequence(sq1),Sequence(sq2)) => 
            sq1.toList.zip(sq2.toList).forall {
              case (s1,s2) => 
                /*
                val mat = new Matcher(controller,metaContext)
                mat(s1,s2) match {// matcher now returns Option[Substitution] instead of Boolean
                  case Some(_) => true
                  case None => false
                }
                */
                false
            }         
          //TODO Other sequence cases:          
          case (OMI(s),OMI(t)) => s == t                   
          case (OMV(v),OMV(w)) if (v == w) => con.isDeclared(v) && con.isDeclared(w) 
          case (OMA(f1,args1),OMA(f2,args2)) => 
             apply(f1 : Term,f2,con) && args1.zip(args2).forall { 
                case (x,y) => apply(x,y,con) 
             }
          case (OMBIND(b1, ctx1, bod1), OMBIND(b2,ctx2,bod2)) => apply(b1,b2,con) && apply(bod1,bod2,con ++ ctx1 ++ ctx2)
          case (OMS(s),OMS(t)) => apply(OMID(s),OMID(t),con)
          case (OMID(s), OMID(t)) => s.toString() == t.toString()
          case (_,_) => false 
        }    
    }
  }
}
    
    