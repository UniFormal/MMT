package info.kwarc.mmt.lf.compile

import info.kwarc.mmt.lf.compile._
import info.kwarc.mmt.api._
import modules._
import symbols._
import libraries._
import objects._
import utils._
import frontend._
import patterns._
import presentation._

/*
 * translates an mmt theory to a logic syntax
 */


class Theory2LogicSyntax {
	case class TheoryLookupError(msg : String) extends java.lang.Throwable(msg) 
  
	/*
	 * translates a theory to a logic syntax LogicSyntax = (List[Category], CatRef, List[Declaration])
	 */
	def translateTheory(theo : Theory)  : LogicSyntax = {
	  val logsyn = theo match {
	    case theo : DeclaredTheory => {
	      val decList = theo.valueListNG
	      val formCat = getForm(decList) match {
	        case Some(x) => x
	        case None => throw TheoryLookupError("could not determine formula category in: " + decList.toString)
	      }
	      println(formCat)
	      println(getCats(decList))
	      LogicSyntax(getCats(decList),formCat,null)
	    }	      	   
	    case theo : DefinedTheory => throw TheoryLookupError("a DefinedTheory")
	    case _ =>  throw TheoryLookupError("unidentified theory") 
	  }
	  
	  logsyn
//	  LogicSyntax(null,CatRef("empty"),null)
	}
	
	def getDecls(sl : List[Symbol]) : List[Category] = {
	  sl.foreach(a => a match {
	    case a : Pattern =>
	  })
	  null
	}
	
	def getCats(sl : List[Symbol]) : List[Category] = {
	  val cons = new MyList(sl).mapPartial(a => a match {
	    case a : Constant => a.tp match {
	      case Some(OMA(x,l)) => Some(Connective(a.name.toString, OMA(x,l).args.init.map(c => CatRef( LocalName.parse(c.toString).last.toString() )) )) // function/connective declaration
	      case _ => None
	    }
	    case _ => None
	  })
	  List(Category("bool",cons))
	}
	
	def getForm(sl : List[Symbol]) : Option[CatRef] = {
	  sl.foreach(a => a match {
	    case a : Constant => if (a.df == None) {
	    	a.tp match {
	    	  case Some(OMID(x)) => if (OMID(x).path.last == "type") return Some(CatRef(OMID(x).path.last))
	    	  case _ => None
	    	}
	    }	      
	    case _ => None
	  })
	  None
	}
  
}
/*
 * test object
 */
object Test {
  case class TestError(msg : String) extends java.lang.Throwable(msg)  
  def main(args : Array[String]) = {
    val cont = new Controller()
    
    
    // add file to archive, go through structure!
    // read a source file
    val sourceFile1 = "/home/aivaras/TPTP/MMT/theories/source/pl.mmt"
//    val sourceFile2 = "/home/aivaras/TPTP/LogicAtlas/source/logics/propositional/syntax/syntax.elf" 
//    cont.handleLine("archive add /home/aivaras/TPTP/LogicAtlas")
//    cont.handleLine("archive latin source-structure")
//    cont.handleLine("achive latin compile")
    cont.handleLine("archive add /home/aivaras/TPTP/MMT/theories")  
    cont.handleLine("archive mmt source-structure")
    cont.handleLine("archive mmt compile")
    println("reading file " + sourceFile1)
    val file = scala.io.Source.fromFile(sourceFile1)
    val (doc, errl) = cont.textReader.readDocument(file,DPath(URI("")))(cont.termParser.apply(_))
    println("errors: " + errl.toString)
    val path = DPath(URI("http://cds.omdoc.org/foundational"))
//    println(cont.globalLookup.getAllPaths().toString)
    val theo =  cont.localLookup.getTheory(path ? "PL") match {
      case d : DeclaredTheory => d
      case _ => throw TestError("attempted retrieving not a DeclaredTheory")
    }
    val tls = new Theory2LogicSyntax()
    println(tls.translateTheory(theo))
    
//    cont.globalLookup.getTheory(path ? "PL")
    println("\n\nend")
  }
}

