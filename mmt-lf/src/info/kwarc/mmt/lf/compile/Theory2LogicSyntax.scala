package info.kwarc.mmt.lf.compile

import info.kwarc.mmt.lf._
//import info.kwarc.mmt.lf.{URI => _}
import info.kwarc.mmt.lf.compile._
import info.kwarc.mmt.api._
import modules._
import symbols._
import libraries._
import objects._
import utils._
import MyList._
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
//	      val formCat = getForm(decList) match {
//	        case Some(x) => x
//	        case None => throw TheoryLookupError("could not determine formula category in: " + decList.toString)
//	      }
//	      println(formCat)
//	      println(getCats(decList))
//	      LogicSyntax(getCats(decList),formCat,null)
//	      val f = getForm(decList) match {
//	        case Some(x) => x
//	        case None => throw TheoryLookupError("no categories could be determined")
//	      }
	      LogicSyntax(getCats(decList,CatRef("bool")),CatRef("bool"),null)
	    }	      	   
	    case theo : DefinedTheory => throw TheoryLookupError("a DefinedTheory")
	    case _ =>  throw TheoryLookupError("unidentified theory") 
	  }
	  
	  logsyn
//	  LogicSyntax(null,CatRef("empty"),null)
	}
	
	def getDecls(sl : List[Symbol]) : List[Category] = {
	  sl.foreach(a => a match {
	    case _ =>
	  })
	  null
	}
	/*
	def getCats(sl : List[Symbol], cat: Option[String]) : List[Category] = {
	  val cons = sl mapPartial {
	    case a : Constant => a.tp match {
	      case Some(FunType(in, out)) =>
	         val catrefs = in.map {
	           case (None, t) =>
	              t match {
	                 case OMS(c) => // a constructor
	                 case Apply(OMS(c), ts) => // a constructor for c (only in case of non-Nil arguments of category)
	                 case _ => // impossible
	              }
	           case (Some(x), t) => //unsupported for now
	         }
	         out match {
	           case Univ(1) => Some(CatRef(""))//TODO return category // a category; in should be Nil for now; 'type'
	           case OMS(c) => None //TODO // a constructor
//	              Connective(a.name.toString, catrefs) //?
	           case Apply(OMS(c), ts) => None //? // a constructor for c (only in case of non-Nil arguments of category), like 'tm i'
	           case _ => None // throw error?  // impossible
	         }
	      case _ => None
	    }
	    case a : Pattern => None //TODO
	    case _ => None
	  }
	  List(Category("bool",cons))
	} */
	def getCats(sl : List[Symbol], cat : CatRef) : List[Category] = {
	  sl mapPartial {
	    case a : Constant => a.tp match {
	      case Some(FunType(in, out)) => {
	        if (in != null) in foreach {a => print(a.toString + "   ")}
	        println(out.toString())
	        //TODO check if 'out' is of the same category as 'cat'
	        if (in == null) None else {
	        	val catrefs = in.map {
	        		case (None, t) => println("") 
	        		  None
	        	}
	        }
	        
	        None
	      }
	      case _ => None // not FunType
	    }
	    case _ => None // not Constant
	  }
	  List()
	}
	
	
	def getCatRefs(sl : List[Symbol]) : List[CatRef] = {
	  sl mapPartial {
	    case a : Constant =>  a.tp match {
	      	case Some(FunType(in, out)) => if (out == Univ(1) && in == null) Some(CatRef("bool")) else None     
	      	case _ => None
	      }
	    
	    case _ => None
	  }
	  
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
    val (doc, errl) = cont.textReader.readDocument(file,DPath(utils.URI("")))(cont.termParser.apply(_))
    println("errors: " + errl.toString)
    val path = DPath(utils.URI("http://cds.omdoc.org/foundational"))
//    println(cont.globalLookup.getAllPaths().toString)
//    val tt = cont.localLookup.getTheory(path ? "PL")
//    println(tt.getClass.toString)
    val theo =  cont.localLookup.getTheory(path ? "PL") match {
      case d : DeclaredTheory => d
      case _ => throw TestError("attempted retrieving not a DeclaredTheory")
    }
    println(theo.toString())
    val tls = new Theory2LogicSyntax()
    println(theo.valueListNG foreach {a => a.toString})
    println(tls.translateTheory(theo))
    
//    cont.globalLookup.getTheory(path ? "PL")
    println("\n\nend")
  }
}

