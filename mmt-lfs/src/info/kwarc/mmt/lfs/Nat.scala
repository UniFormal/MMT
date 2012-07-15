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

object Add {
  def apply(n : Term, m : Term) : Term = OMA(OMS(LFS.plus),List(n,m))  
  def unapply(t : Term) : Option[(Term,Term)] = {
    t match {
      case OMA(OMS(LFS.plus),List(n,m)) => Some((n,m))
      case _ => None
    }    
  } 
}

object Subtract {
  def apply(n : Term, m : Term) : Term = OMA(OMS(LFS.minus),List(n,m))  
  def unapply(t : Term) : Option[(Term,Term)] = {
    t match {
      case OMA(OMS(LFS.minus),List(n,m)) => Some((n,m))
      case _ => None
    }    
  } 
}

object Multiply {
  def apply(n : Term, m : Term) : Term = OMA(OMS(LFS.times),List(n,m))  
  def unapply(t : Term) : Option[(Term,Term)] = {
    t match {
      case OMA(OMS(LFS.times),List(n,m)) => Some((n,m))
      case _ => None
    }    
  } 
}


