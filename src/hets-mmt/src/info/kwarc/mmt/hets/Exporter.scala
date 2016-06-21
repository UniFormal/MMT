package info.kwarc.mmt.hets

import info.kwarc.mmt.api._
import modules._
import patterns._
import utils._
import objects._
import MyList._
import frontend._

/**
 * exports an OMDoc theory (DeclaredTheory in controller lib) 
 * to a pseudo-XML file
 */
class Exporter(c : Controller = new Controller) {
	val controller = c
  
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
			case OMS(s) => s.name match {
			  		case p / i => {
			  		  val ins : Instance = controller.globalLookup.get(GlobalName(s.module, LocalName(p))) match {
			  		    case x : Instance => x
			  		    case _ =>  throw GetError(GlobalName(s.module, LocalName(p)).toString + " is not an Instance")
			  		  }
			  		  <app name={i.toPath} pattern={ins.pattern.name.toPath} instance={ins.name.toPath} />  
			  		} 
			  		case i => <app name={s.name.toPath}/>  
				}
			case OMA(OMS(s),args) => 
			  					<app name={s.name.toPath}>
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
	
	def compile(outDir : File, theory : DeclaredTheory) {	  
	  val outName = theory.name.toString + ".xml"
	  val instances = theory.components.mapPartial { x => x match {
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
	  val ins = theory.getDeclarations.mapPartial {
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