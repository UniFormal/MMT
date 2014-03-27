package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.presentation._
import scala.collection.mutable.ArrayStack
import info.kwarc.mmt.lf._
import scala.collection._


object TranslationController {
	
    var controller = {
       val c = new frontend.Controller
//       c.setFileReport(File("mizar.log"))
//       c.setCheckNone //c.setFoundChecker(new libraries.DefaultFoundation(controller.report))
       c
    }
    
    var query : Boolean = false
	  
	//new frontend.Controller(libraries.NullChecker, new FileReport(new java.io.File("mizar.log")))
	
    //set during translation
    var currentBase : String = null
    var currentVersion : String = null
    var currentAid : String = null
    var currentDoc : Document = null
    
    def localPath = LocalName(currentAid)
    def currentThyBase : DPath = DPath(Mizar.mmlBase / currentVersion.toString)
    def currentTheory : MPath = currentThyBase ? localPath
    def currentSource = Mizar.mathHubBase + "/source/" + currentVersion + "/" + currentAid.toLowerCase() + ".miz"

    var anonConstNr = 0
    var defs = 0
    var theorems = 0
    var notations = 0
    var regs = 0
    var schemes = 0
    var varContext : ArrayStack[Term] = new ArrayStack
    var locusVarContext : ArrayStack[Term] = new ArrayStack
    // consider/set/reconsider everywhere and let in proofs
    var constContext : mutable.HashMap[Int,Term] = mutable.HashMap()
    //deftheorems, lemmas, assume and others 
    var propContext : mutable.HashMap[Int,Term] = mutable.HashMap()
	
    def clear() = {
      constContext = mutable.HashMap()
      propContext = mutable.HashMap()
      //controller.clear
      anonConstNr = 0
    }
    
    def getLmName(nrO : Option[Int]) : String = nrO match {
      case Some(nr) => 
        "Lm" + nr
      case None => 
        anonConstNr += 1
        "AnonLm" + anonConstNr
    }
	
	
	def addSourceRef(mmtEl : metadata.HasMetaData, mizEl : MizAny) = mizEl.sreg match {
      case None => None
      case Some(sregion) => 
        val start = parser.SourcePosition(-1, sregion.start.line, sregion.start.col)
        val end = parser.SourcePosition(-1, sregion.end.line, sregion.end.col)
        val ref = parser.SourceRef(URI(currentSource), parser.SourceRegion(start, end))
        mmtEl.metadata.add(metadata.Link(parser.SourceRef.metaDataKey, ref.toURI))
    }
	
	def add(e : StructuralElement) {
		controller.add(e)
	}
	
	def resolveVar(nr : Int) : Term = {
	    varContext(varContext.length - nr) 
	}
		
	def resolveLocusVar(nr : Int) : Term = {
	  assert (locusVarContext.length >= nr,"TranslationController.resolveLocusVar " +
	  		"for number " + nr +" which is over the size of context " + locusVarContext)
	  locusVarContext(locusVarContext.length - nr)
	}
	
	def addLocalProp(nrO : Option[Int]) : LocalName = nrO match {
	  case Some(nr) =>
	    val name = LocalName("p" + nr)
	    propContext(nr) = OMV(name)
	    name
      case _ => OMV.anonymous
	}
	
	def addGlobalProp(nrO : Option[Int], sName : String) = nrO match {
	  case Some(nr) =>
	    val name = LocalName(sName)
	    propContext(nr) = OMID(MMTUtils.getPath(TranslationController.currentAid, name))
	  case _ => None
	}
	
	def resolveProp(nr : Int) : Term = try {
	  propContext(nr)
	} catch {
	  case _ : Throwable => 
	    println(propContext)
	    throw new java.lang.Error("propContext lookup failed for " + nr)
	}
	
	def addGlobalConst(nr : Int, kind : String) : LocalName = {
	  val name = LocalName(kind + nr)
	  constContext(nr) = OMID(MMTUtils.getPath(TranslationController.currentAid, name))
	  name
	}
	
	def addLocalConst(nr : Int) : LocalName = {
	  val name = LocalName("c" + nr)
	  constContext(nr) = OMV(name)
	  name
	}
	
	def resolveConst(nr : Int) : Term = {
	  if (query) {
	    Mizar.apply(OMID(MMTUtils.getPath("qvar","const")), OMV("c" + nr.toString))	    
	  } else {
		constContext(nr)
	  }
	}
	
	def addQVarBinder() = {
	  val name = "x" + varContext.length
	  varContext.push(Mizar.apply(OMID(DPath(Mizar.mmlBase) ? "qvar" ? "qvar"), OMV(name)))
	}
	
	def addVarBinder(n : Option[String]) : String = n match {
		case Some(x) => 
			varContext.push(OMV(x))
			x			
		case None =>
			val name = "x" + varContext.length
			varContext.push(OMV(name))
			name
	}
	
	def clearConstContext() = {
	  constContext = mutable.HashMap()
	}
	
	def clearVarBinder() = {
		varContext.pop()
	}
	
	def clearVarContext() = {
		varContext = new ArrayStack()
	}
	
	def addLocusVarBinder(tm : Term) : Unit = {
		locusVarContext.push(tm)
	}
	
	def addRetTerm(path: GlobalName) = {
	    locusVarContext.length match {
	      case 0 => locusVarContext.push(OMID(path))
	      case _ => locusVarContext.push(Mizar.apply(OMID(path), locusVarContext : _*))
	    }
	}
	
	def clearLocusVarBinder() {
		locusVarContext.pop()
	}

	def clearLocusVarContext() {
		locusVarContext = new ArrayStack()
	}
		
	def getFreeVar() : String = {
		var i : Int = 0
		val totalContext = varContext.toList ::: locusVarContext.toList
		while (totalContext.contains(OMV("x" + i))) {
			i = i + 1
		}
		"x" + i
	}
	
	def makeConstant(n: LocalName, t: Term) =
    Constant(OMMOD(currentTheory), n, None, Some(t), None, None)
  def makeConstant(n: LocalName, tO: Option[Term], dO: Option[Term]) =
    Constant(OMMOD(currentTheory), n, None, tO, dO, None)
      
  def makeDefConstant(kind : String, absnr : Int, tp : Term, df : Term) = {
	  val lname = LocalName(kind + absnr.toString)
	  val name = currentTheory ? lname
	  ParsingController.dictionary.getFormatByAbsnr(currentAid, kind, absnr) match {
	    case Some(format) if format.symbol != null => //found a format so will add notation
	      val argnr = format.argnr
	      val leftargnr = format.leftargnr
	      val markers = format.rightsymbol match {
	        case None => //single delimiter
	          def genMarkers(args : Int, namePos : Int) : List[Marker] = args match {
	            case 0 => Nil
	            case 1 => Arg(0) :: Nil
	            case n => 
	              val delimS = if (namePos == 1) format.symbol.name else ","
	              Arg(n - 1) :: Delim(delimS) :: genMarkers(n - 1, namePos - 1)
	          }
	          genMarkers(argnr, leftargnr)
	        case Some(rightsymbol) => // double delimiter
	          val first = Delim(format.symbol.name)
	          val last = Delim(rightsymbol.name)
	          val rest = 0.to(argnr - 1).toList.map(Arg(_))
	          first :: (rest ::: List(last))
	      }
        val not = new TextNotation(name, Mixfix(markers), Precedence.integer(0), None)
        val alias = ParsingController.resolveDef(currentAid, kind, absnr).map(LocalName(_))
        val notC = NotationContainer.apply(not)
        Constant(OMMOD(currentTheory), lname, alias ,Some(tp), Some(df), None, notC)
	    case _ => 
	      makeConstant(lname, Some(tp), Some(df))
	  }
	}   
      
}