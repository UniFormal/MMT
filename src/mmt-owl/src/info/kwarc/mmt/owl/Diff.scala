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
    
	val firstController = new Controller
    val secondController = new Controller
	firstController.handle(ExecFile(new java.io.File("startup.mmt")))
	secondController.handle(ExecFile(new java.io.File("startup.mmt")))
	 
    //val firstSource : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\DatatypeDefinitionAxiom\\datatypeDefinitionAxiom.omdoc")
    //val secondSource : File = new File("E:\\Fall10\\CompSem\\Project\\OWLMMT\\Test\\Axioms\\DatatypeDefinitionAxiom\\datatypeDefinitionAxiomChanged.omdoc")
	//val firstSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\Identifiers\\identifiers.omdoc")
    //val secondSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\Identifiers\\identifiersChanged.omdoc")
	//val firstSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\ChangeImpacts\\changeImpactsIDs.omdoc")
    //val secondSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\ChangeImpacts\\changeImpactsIDsChanged.omdoc")
	//val firstSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\TestMMTDiff\\testMMTDiff.omdoc")
    //val secondSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\TestMMTDiff\\testMMTDiffChanged.omdoc")
	val firstSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\Deneme\\denemeWithIDs.omdoc")
    val secondSource : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\Deneme\\denemeWithIDsChanged.omdoc")
                
    val firstDoc : DPath  = firstController.read(firstSource)
    val secondDoc : DPath = secondController.read(secondSource)
        
    var diff = Differ.diff(firstController, secondController, firstDoc, secondDoc)
//    println(diff.toNode.toString())
    val pretty = new PrettyPrinter (150, 3) 
    println(pretty.format(diff.toNode))
       
  }
  
  
}