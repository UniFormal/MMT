package info.kwarc.mmt.hets

import info.kwarc.mmt.api._
import modules._
import patterns._
import utils._
import objects._

/**
 * exports an OMDoc theory (DeclaredTheory in controller lib) 
 * to a pseudo-XML file
 */
class Exporter {
	def write(out : File, ins : Instance) = {
	  
	  val args = ins.matches.components	  
	  val nd : scala.xml.Node = 
		  <instance pattern={ins.pattern.toString()} name={ins.name.toString()}>
		  	
		  </instance>
	}
	
	def toNode(t : Term) = { 
	  val out : scala.xml.Node = t match {
			case OMV(n) => { 
				<var name={n.toPath}> </var>				
			}
			case OMA(f,args) => <bind> </bind>
			case OMBIND(binder,ctx,bd) => <bind> </bind> 
			case OMBINDC(binder,ctx,cnd,bd) => <bind> </bind>  
	  } 
	  null
	}
}