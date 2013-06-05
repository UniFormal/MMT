// outdated TPTP pattern-checker
/*
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
 import backend._
 import scala.xml._

/*	pragmatic OMDoc compiler: omdoc theory -> omdoc pragmatic declaration (instances)
 *  reads an omdoc file with a declared theory in it, parses into controller
 *  retrieves the theory as mmt data structure from the controller, retrieves patterns from the controller
 *  pattern-checks declarations in theory with patterns via PatternChecker -> list of pattern instances
 *  writes out instances into an omdoc file in the same directory as the input file, different file name
 */
 /* [SOLVED] written instances cannot be turned into content via "archive content" yet, bugs to fix
 * [?] make code compatible (make calls same as to any other compiler), can parse patterns from file
 */

class InstanceCompiler extends Compiler {
  
  def isApplicable(src : String) : Boolean = src == "omdoc"    
  
  private var tptppath : String = null
  override val logPrefix = "comp. pragmatic"    
  override def init(con: Controller, args: List[String]) {
     tptppath = args(0)     
     super.init(con, Nil)
  }
    
  case class CompilationError(msg : String) extends java.lang.Throwable(msg)     
    
  def compile(in : File, dpath: Option[DPath], out : File) : List[SourceError] = {
    var errors: List[SourceError] = Nil
    val fileName = in.toJava.getName
    val path = in.toJava.getPath
    
    // compute output directory
    // TODO too specific, only applies to TPTP
    var fileDir = ""
//    if (path.contains("Axioms"))
//      fileDir = path.substring(path.lastIndexOf("Axioms"), path.lastIndexOf("/"))
//    else
//      fileDir = path.substring(path.lastIndexOf("Problems"), path.lastIndexOf("/"))
    fileDir = path.substring(0,path.lastIndexOf("/"))  
    val dir = out.toJava.getParentFile
    if (!dir.exists)
      dir.mkdirs
    
    // pattern controller contains hard-coded declaration patterns  
    val patcon = new PatternController()
	val pc = new PatternChecker(patcon.controller)
    
	val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
	val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base  
	val thName = fileName.substring(0,fileName.lastIndexOf("."))
	
	if (out.toJava.exists()) return { log("file exists, skip compilation")     ; errors}
	
//	val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
//	val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base
	//    val cont = new Controller
	//    cont.handleLine("archive add /home/aivaras/TPTP/tptp")
	//    cont.get(tptpbase ?  thName)
	
	// compute output directory
	// TODO too specific, only applies to TPTP
		//    if (path.contains("Axioms"))
		//      fileDir = path.substring(path.lastIndexOf("Axioms"), path.lastIndexOf("/"))
		//    else
		//      fileDir = path.substring(path.lastIndexOf("Problems"), path.lastIndexOf("/"))
		fileDir = path.substring(0,path.lastIndexOf("/"))  
			
			// pattern controller contains hard-coded declaration patterns  
//    patcon.controller.get(pbbase ?  thName)
	
// read XML
//    val read = new XMLReader(new Controller)

//  val thName = fileName
// retrieve constant declarations
		val constTheory = patcon.controller.globalLookup.getTheory(pbbase  ? thName) match {
		case c : DeclaredTheory => c
		case _ => throw CompilationError("not a declared theory " + pbbase + "?" + thName)
		}
		val constList : List[Constant] = constTheory.getConstants
		// retrieve patterns    
		val pattTheory = patcon.controller.globalLookup.getTheory(tptpbase ? "THF0") match {
		case t : DeclaredTheory => t
		case _ => throw GetError("not a declared theory " + tptpbase + "?" + "THF0")      
		}        
		val pattList : List[Pattern] = pattTheory.getPatterns
		// get list of all Instance for the theory
		val inst =  pc.getInstance(constList, pattList)

		// write to output file            
		write(out.setExtension("omdoc"), fileDir, fileName, inst)
		// log errors
		if (!errors.isEmpty) errors.foreach(e => log(e.toString))

		errors
}  

def write(out: File, fileDir: String, name: String, ins : List[Instance]) = {
	val thName = name.substring(0,name.lastIndexOf(".")) + "pragmatic"
	val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
	val docPath = out.toJava.getPath
	val pp = new scala.xml.PrettyPrinter(100,2)
	val fw = new java.io.FileWriter(docPath)       
	//    val base = (patcon.controller.getBase) / fileDir
	val nd : scala.xml.Node = 
	<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={tptpbase.toString()}>
	<theory name={thName} base={tptpbase.toString()} meta={"http://cds.omdoc.org/foundations/lf/lf.omdoc?lf"}>     
	{ins.map(x => x.toNode)}
	</theory>
	</omdoc>
	val docNode = pp.format(nd)
	fw.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
	fw.close
} 

 }

 // controller with patterns in it
 class PatternController {
	 
	 val controller = new Controller
	 val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
	 val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base
	 	
	 controller.handleLine("archive add /home/aivaras/TPTP/tptp")
	 controller.handleLine("archive add /home/aivaras/TPTP/LogicAtlas")
	 
	 val baseType = new Pattern(OMID(tptpbase ? "THF0"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"), None)
	 val typedCon = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedCon"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))), None )
	 val axiom = new Pattern(OMID(tptpbase ? "THF0"), LocalName("axiom"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) , OMV("c") % OMA(OMS(tptpbase ? "THF0" ? "$istrue"), List(OMV("F"))), None )
	 val typedConDef = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedConDef"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMV("A")))),Some(OMV("D"))), None)
	 val theorem = new Pattern(OMID(tptpbase ? "THF0"), LocalName("theorem"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "THF0" ? "$istrue"), List(OMV("F")))),Some(OMV("D"))), None)
	 controller.add(baseType)
	 controller.add(typedCon)
	 controller.add(axiom)
	 controller.add(typedConDef)
	 controller.add(theorem) 
 }
 

 // compilation test
 object CompTest {
	 
	 //  val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
	 //  val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base
	 
	 def main(args : Array[String]) {
		 
		 val compiler = new InstanceCompiler
		 
		 val in = File(new java.io.File("/home/aivaras/TPTP/tptp/compiled/Problems/AGT/AGT027^1.omdoc"))
		 val out = File(new java.io.File("/home/aivaras/TPTP/tptp/pragmatic/Problems/AGT/AGT027^1.omdoc"))
		 
		 compiler.compile(in, None, out)
		 
		 println("\nENDofCompTest")
		 
	 }
	 
}
*/