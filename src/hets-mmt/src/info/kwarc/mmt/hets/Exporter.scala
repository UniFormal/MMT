package info.kwarc.mmt.hets

import info.kwarc.mmt.api._
import modules._
import patterns._
import utils._
import objects._
import MyList._

/**
 * exports an OMDoc theory (DeclaredTheory in controller lib) 
 * to a pseudo-XML file
 */
class Exporter {
	def insToNode(out : File, ins : Instance) : scala.xml.Node = {
		
	    // get substituting terms
	  val args = ins.matches map {x => toNode(x)}
		
		// wrap instance declaration
	  	<instance pattern={ins.pattern.last} name={ins.name.toPath}>
			{  args	}
		</instance>		  
	}
	
	/**
	 * translates OM MMT objects to simplified XML nodes
	 */
	def toNode(t : Term) : scala.xml.Node = { 
	  t match {
			case OMV(n) => <var name={n.toPath}/>	
			case OMS(s) => <app name={s.name.toPath}/>
			//TODO have reference p_i hidden in the path or declared separately? 
			case OMA(OMS(s),args) => <app name={s.name.toPath}>
									{/* if p_i, produce another node? */}
									{args map(x => toNode(x))} 
								</app>
			case OMBIND(OMS(s),ctx,bd) =>
			  ctx.variables.toList match {
			    case (VarDecl(v,None, None) :: Nil) =>
			       <bind binder={s.name.toPath} var={v.toPath}> {toNode(bd)} </bind> 
			    case (VarDecl(v,Some(tp), None) :: Nil) =>
			      <tbind binder={s.name.toPath} var={v.toPath}> {toNode(tp)} {toNode(bd)} </tbind> 
			    case _ => <error msg="binder context does not match"/>
			  }
			  
	  } 
	  
	}
	
	def compile(outDir : File, parent : DeclaredTheory) {	  
	  val outName = parent.name.toString + ".xml"
	  val instances = parent.components.mapPartial { x => x match {
	    case x : Instance => Some(x)
	    case _ => None
	  	}	     
	  }
	  val out = File(outDir.toJava.getPath() + "/" + outName)
	  if (!outDir.toJava.exists()) {
	    println("creating dirs " + outDir.toJava.getPath())
	    outDir.toJava.mkdirs()
	  }
	  val fw = new java.io.FileWriter(out.toJava.getPath())
	  val ins = parent.getDeclarations.mapPartial {
      	case p: patterns.Instance => Some(p)
      	case _ => None
	  }
	  val nodes : List[scala.xml.Node]= ins map { 
	     insToNode(out,_)
	  }
	  println("writing to file: " + out.toJava.getPath())
	  println((nodes map {x => x.toString()}).mkString)
	  val pp = new scala.xml.PrettyPrinter(100,2)
	  val docNode = pp.format(nodes.head)
	  fw.write((nodes map {x => x.toString}).mkString("\n"))
	  fw.close()
	}
}

object ExporterTest {  
  def main(outDir : String, test : DeclaredTheory) = {
    new Exporter().compile(File(outDir),test)
  }  
}