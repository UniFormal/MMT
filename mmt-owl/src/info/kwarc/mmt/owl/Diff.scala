package info.kwarc.mmt.owl

import java.io.File
import java.net.URI
import info.kwarc.mmt.api._
import utils.FileConversion._
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
	
	var currentVersion : File = null
	var olderVersion : File = null
	
	if(args.length < 1) 
	{ println("USAGE: info.kwarc.mmt.owl.Diff NEWFILE [OLDFILE]")  
	  exit
	}
	
	currentVersion = new File(args(0))
		
	if(args.length < 2)
	{ 
	  // get the older version from svn if it exists
	  val currentName = currentVersion.getName()    // NAME 
	  val currentDir = currentVersion.getParent()   // DIR
	  
	  if(currentDir == null)
	  { if( (((new File(".svn"))./("text-base"))./(currentName + ".svn-base")).exists() == false)
	    { println("USAGE: info.kwarc.mmt.owl.Diff NEWFILE OLDFILE")  
	      exit
	    }
	    else 
	      //.svn/text-base/Name.svn-base 
		  olderVersion = ((new File(".svn"))./("text-base"))./(currentName + ".svn-base")
	  }
	  else
	  {  if(((((new File(currentDir))./(".svn"))./("text-base"))./(currentName + ".svn-base")).exists() == false)
	  	 { println("USAGE: info.kwarc.mmt.owl.Diff NEWFILE OLDFILE")  
	       exit
	  	 }
		 else
			  // DIR/.svn/text-base/NAME.svn-base
			  olderVersion = (((new File(currentDir))./(".svn"))./("text-base"))./(currentName + ".svn-base")
	  }          
	 }
	 else 
		 olderVersion = new File(args(1))
			
	//val olderVersion : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\ChangeImpacts\\changeImpactsIDs.omdoc")
    //val currentVersion : File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\ChangeImpacts\\changeImpactsIDsChanged.omdoc")
                
    val olderDoc : DPath  = firstController.read(olderVersion)._1.path
    val currentDoc : DPath = secondController.read(currentVersion)._1.path
        
    var diff = Differ.diff(firstController, secondController, olderDoc, currentDoc)
//    println(diff.toNode.toString())
    val pretty = new PrettyPrinter (150, 3) 
    println(pretty.format(diff.toNode))
       
  }
  
  
}