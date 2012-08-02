package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._

import info.kwarc.mmt.api._
import documents._
import utils._
import frontend._
import backend._
import symbols._
import libraries._
import modules._
import objects._
import presentation._
import patterns._
import objects.Conversions._
import utils.MyList._
import scala.io.Source

/*	substitution compiler: omdoc theory -> omdoc instances
 *  reads an omdoc file with a declared theory in it, parses into controller
 *  retrieves the theory as mmt data structure from the controller, retrieves patterns from the controller
 *  pattern-checks declarations in theory with patterns via PatternChecker -> list of pattern instances
 *  writes out instances into an omdoc file in the same directory as the input file, different file name
 */
// TODO
/* [SOLVED] written instances cannot be turned into content via "archive content" yet, bugs to fix
 * reorder, clarify, clean the code - right now the code is crap 
 * make code compatible (make calls same as to any other compiler), can parse patterns from file
 * output the instance files into separate folder (not "/compiled/")
 */

case class SomeError(msg : String) extends java.lang.Throwable(msg)  

class SubsCompiler extends Compiler {
  
  def isApplicable(src : String) : Boolean = src.substring(src.lastIndexOf(".")) == "omdoc"    
  
  override def compile(in : File, out : File) : List[SourceError] = {
    var errors: List[SourceError] = Nil
    val fileName = in.toJava.getName
    val path = in.toJava.getPath
    
    // compute output directory
    // TODO too specific, only applies to tptp
    var fileDir = ""
    if (path.contains("Axioms"))
      fileDir = path.substring(path.lastIndexOf("Axioms"), path.lastIndexOf("/"))
    else
      fileDir = path.substring(path.lastIndexOf("Problems"), path.lastIndexOf("/"))
    val dir = out.toJava.getParentFile
    if (!dir.exists)
      dir.mkdirs
    
      
    val patcon = new PatternController()
	val pc = new PatternChecker(patcon.controller)
    
    // retrieve constant list
	val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
	val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base  
	val thName = fileName.substring(0,fileName.lastIndexOf("."))
   	val constTheory = patcon.controller.globalLookup.getTheory(pbbase  ? thName) match {
      case c : DeclaredTheory => c
      case _ => throw SomeError("no constants in " + pbbase + "?" + thName)
    }
    val constList : List[Constant] = constTheory.valueListNG mapPartial {
      case p : Constant => Some(p)
      case _ => None
    }
    
    // retrieve patterns    
    val pattTheory = patcon.controller.globalLookup.getTheory(tptpbase ? "THF0") match {
      case t : DeclaredTheory => t
      case _ => throw GetError("no patterns in " + tptpbase + "?" + "THF0")      
    }        
    val pattList : List[Pattern] = pattTheory.valueListNG mapPartial {
      case p : Pattern => Some(p)
      case _ => None
    }    
    
    val inst = pc.getInstance(constList, pattList)
    
//    val q = constTheory.
    
    // write to output file            
    write(out.setExtension("omdoc"), fileDir, fileName, inst)
    
    errors
  }  
   
  
  
  def write(out: File, fileDir: String, name: String, ins : List[Instance]) = {
    	
	  	val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
		val docPath = out.toJava.getPath
		val pp = new scala.xml.PrettyPrinter(100,2)
		val fw = new java.io.FileWriter(docPath)	  		
	  	
//		val base = (patcon.controller.getBase) / fileDir
		
		val nd : scala.xml.Node = 
		<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={tptpbase.toString()}>
		   <theory name={"test"} base={tptpbase.toString()} meta={""}>     
			{ins.map(x => x.toNode)}
			 </theory>
		</omdoc>
		val docNode = pp.format(nd)
		fw.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
		fw.close
	
	
  } 
    
}




class PatternController {
  
        val controller = new Controller
        val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
        val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base
        
        controller.handleLine("file test.mmt")
        
        val baseType = new Pattern(OMID(tptpbase ? "THF0"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"))
        val typedCon = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedCon"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))) )
        val axiom = new Pattern(OMID(tptpbase ? "THF0"), LocalName("axiom"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) , OMV("c") % OMA(OMS(tptpbase ? "THF0" ? "$istrue"), List(OMV("F"))) )
        val typedConDef = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedConDef"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMV("A")))),Some(OMV("D"))))
        val theorem = new Pattern(OMID(tptpbase ? "THF0"), LocalName("theorem"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "THF0" ? "$istrue"), List(OMV("F")))),Some(OMV("D"))))
        controller.add(baseType)
        controller.add(typedCon)
        controller.add(axiom)
        controller.add(typedConDef)
        controller.add(theorem) 
        
  
}

object CompTest {
	
	val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
	val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base
	
	val patcon = new PatternController()

	def main(args : Array[String]) {
	  
//	  controller.handleLine("file pattern-test.mmt")
//	  controller.add(baseType)
//	  controller.add(typedCon)
//	  controller.add(axiom)
//	  controller.add(typedConDef)
//	  controller.add(theorem)
	  
	 val thName : String = "AGT027^1"
	 
//	 val constTheory = patcon.controller.globalLookup.getTheory(pbbase  ? thName) match {
//      case c : DeclaredTheory => c
//      case _ => throw Error("no constants in " + pbbase + "?" + thName)
//    }
//    val constList : List[Constant] = constTheory.valueListNG mapPartial {
//      case p : Constant => Some(p)
//      case _ => None
//    }
	  
    //     get list of patterns from controller
//    val pp = try {
//      patcon.controller.globalLookup.getPattern(tptpbase ? "THF0" ? "axiom")
//    } catch {
//      case GetError(m) => throw GetError("tptpbase ? \"THF0\" ? \"axiom\" \n" + m)
//    }
//    val pattTheory = patcon.controller.globalLookup.getTheory(tptpbase ? "THF0") match {
//      case t : DeclaredTheory => t
//      case _ => throw GetError("no patterns in " + tptpbase + "?" + "THF0")      
//    }        
//    val pattList : List[Pattern] = pattTheory.valueListNG mapPartial {
//      case p : Pattern => Some(p)
//      case _ => None
//    }
		 
	  val compiler = new SubsCompiler
	  
	  val in = new File(new java.io.File("/home/aivaras/TPTP/tptp/compiled/Problems/AGT/AGT027^1.omdoc"))
	  val out = new File(new java.io.File("/home/aivaras/TPTP/tptp/instance/Problems/AGT/AGT027^1p.omdoc"))
	  
	  compiler.compile(in, out)
    
//	  val matches = constList.map {
//        a => 
//          pattList.map {
//          p =>  
//            pc.patternCheck(List(a),p)
//        }
//      }.flatten.flatten      
	  
	  println("\nend")
	  
	}
  
}