package info.kwarc.mmt.tptp

import tptp._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._

/**
 * TPTP Compiler, translates TPTP sources to OMDoc
 */
class TptpCompiler extends Compiler {
  
//  val parameterRenaming: ParameterRenaming

  def isApplicable(src : String) : Boolean = {
    true
  }

  override def compile(in : File, out : File) : List[CompilerError] = {
    var errors: List[CompilerError] = Nil
    val fileName = in.toJava.getName
    val parser = new TptpParser(in.toJava)
    val translator = new TptpTranslator(fileName)
    // parameterRenaming.newTheory();
    
    var input: TptpParserOutput.TptpInput = parser.parseNext
    while (input != null) {
      input match {
        case tli: SimpleTptpParserOutput.TopLevelItem => {
            translator.translate(tli) match {
              case Some(x) => TptpTranslator.add(x.asInstanceOf[StructuralElement])
              case None => println("Error translating " + tli.toString) // TODO add to errors
            }
        }
        case _ => println("Error, unknown input") // TODO errors
      }
      input = parser.parseNext
    }
    write(out, fileName)
    System.exit(1)
    Nil
  }
  
	def write(out: File, name: String) {
		val docPath = out.toJava.getPath
		val base = TptpUtils.baseURI
		val pp = new scala.xml.PrettyPrinter(100,2)
		val th = TptpTranslator.controller.get(new DPath(base) ? name)
		val fw = new java.io.FileWriter(docPath)
		
		val nd : scala.xml.Node = 
		<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={base.toString}>
			{th.toNode}
		</omdoc>
	
		val docNode = pp.format(nd)
		fw.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
		fw.close
	}

  override def init(args : List[String] = Nil) {
//    this.transformer = new FormulaTransformer();
//    this.transformer.registerRule(new AlphaRename());
//    this.transformer.registerRule(new EliminateEquivalence());
//    this.transformer.registerRule(new EliminateImplication());
//    this.transformer.registerRule(new NegationNormalization());
//    this.transformer.registerRule(new PrenexNormalization());
//    this.transformer.registerRule(new ACStandardization());
//    parameterRenaming = new ParameterRenaming();
//    this.transformer.registerRule(parameterRenaming);
  }
}