package info.kwarc.mmt.owl

import java.io.File
import java.net.URI
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.moc._
import scala.xml._


object Diff{
  def main(args: Array[String]) {
    
    val report = new FileReport(new java.io.File("controller.log"))
	val checker = new FoundChecker(new DefaultFoundation, report)
	val firstController = new Controller(checker, report)
    val secondController = new Controller(checker, report)
	firstController.handle(ExecFile(new java.io.File("startup.mmt")))
	secondController.handle(ExecFile(new java.io.File("startup.mmt")))
	 
    val firstSource : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\ObjectPropertyAxiom\\objectPropertyAxiom.omdoc")
    val secondSource : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\ObjectPropertyAxiom\\objectPropertyAxiomChanged.omdoc")
	    
    val firstDoc : DPath  = firstController.read(firstSource)
    val secondDoc : DPath = secondController.read(secondSource)
        
    var diff = Differ.diff(firstController, secondController, firstDoc, secondDoc)
//    println(diff.toNode.toString())
    val pretty = new PrettyPrinter (150, 3) 
    println(pretty.format(diff.toNode))
   
  }
  
  
}