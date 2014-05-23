package main.scala.info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.frontend.ExtensionManager
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.utils.mmt
import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.symbols._

class EditingServicePlugin(val controller : Controller) {
  def getAutocompleteResponse(request: MMTAutoCompleteRequest) : MMTAutoCompleteResponse = 
  { 
      val prefix = request.getPrefix
      val mpath = Path.parseM(request.getMPath,mmt.mmtbase)
      
      val myPaths = Names.resolve(OMMOD(mpath), Nil, prefix)(controller.globalLookup)
      val response = myPaths.map(_.path.toPath)
      new MMTAutoCompleteResponse(response)
  }
  
   def getResolveIncludesResponse(request: MMTResolveIncludesRequest) = {
      
	  val symbol = request.getSymbol
      val mpath = Path.parseM(request.getMPath, mmt.mmtbase)
      
      val respMap = new collection.mutable.HashMap[String,Any]()
      Names.resolveIncludes(OMMOD(mpath),symbol)(controller.library) match {
        case None => respMap("found") = true
        case Some(myPaths) => 
          respMap("found") = false
          respMap("options") = myPaths.map(_.from.toPath) 
      }
	  new MMTResolveIncludesResponse(respMap.toList)
  }
   
   def minIncl(to: MPath) : List[MPath] = {
    val thy = controller.get(to) match {
      case theor: DeclaredTheory => theor
      case _ => throw ServerError("No theory found")
    }
    val incl = thy.getIncludes
    //thy.components.length 
    def remover(checked: List[MPath], rest : List[MPath]) : List[MPath] = rest match{
      case Nil => checked
      case (hd::tl) => 
        if(tl.exists(x=>controller.globalLookup.visible(OMMOD(x)) .contains(OMMOD(hd))) ||
            checked.exists(x=>controller.globalLookup.visible(OMMOD(x)) .contains(OMMOD(hd))))
        	remover(checked,tl)
    	else remover(hd::checked,tl)	
    }
    
   remover(Nil,incl)
  }
  
 
  def getMinIncludes(request : MMTMinIncludesRequest) : MMTMinIncludesResponse= {
      val mpath = Path.parseM(request.getMPath, mmt.mmtbase)		
    
      val newIncludes = minIncl(mpath)
      new MMTMinIncludesResponse(newIncludes.map(_.toString))
  }
  
  def getTermInference(request: MMTTermInferenceRequest) : MMTTermInferenceResponse = {
          
      val mpath = Path.parseM(request.getMPath, mmt.mmtbase)
      val sref = new SourceRef(mpath.doc.uri, SourceRegion(SourcePosition(-1,0,0),SourcePosition(-1,0,1)))
      
      val term = controller.termParser(ParsingUnit(sref, OMMOD(mpath), Context(), request.getTerm), e=>throw e)
      
      def getHoles(term: Term , context : Context) : List[(Term,Context)] = {
        term match {
          case Hole(t) => (t,context)::Nil
          case OMBINDC(nterm,ncontext,nbodyList)=> (nterm::nbodyList).flatMap(getHoles(_,context++ncontext))
          case OMBIND(nterm,ncontext,nbody) => getHoles(nbody, context++ncontext)
          case OMA(f, args) => (f :: args).flatMap(getHoles(_, context))
          case _ => Nil
        }
      }
      //responses in cases
      val returnNoHoles = "Term Complete"
      val holeContextList =  getHoles(term,Context())
      
      //response Term Complete if there are no Holes, No Rules if no rule is applicable otherwise return the rules
      holeContextList match { 
        case Nil => val returnNoHoles = "Term Complete" :: Nil
        			new MMTTermInferenceResponse(returnNoHoles)
        case _ =>  
          val hole = holeContextList.head._1
          val context = holeContextList.head._2
          val prover = new Prover(controller) 
          val rules = prover.applicable(hole) (Stack(Frame(OMMOD(mpath),context)))
          rules match{ 
        			case Nil => val returnNoRules = "No Rules" :: Nil
        						new MMTTermInferenceResponse(returnNoRules)
        			case _ =>  new MMTTermInferenceResponse(rules.map(_.label.toString))
        						
          }
      } 
  }
  
  def getSymbolCompletion(spathS: String) : List[String] = {
      try {
        val spath = Path.parseS(spathS, mmt.mmtbase)
        val constants = controller.get(spath) match {
          case c : Constant => c.not match {
            case None => List(c.name -> Nil) 
            case Some(a) => List(c.name -> a.fixity.markers)
         }
          case _ => Nil
        }
      
        def modifyStringRepresent (name:LocalName,markers:List[Marker],accumulator:String,hasSymbol:Boolean) : (String) = {
          val separator = " " 
          markers match {
            case Nil => 
              if(hasSymbol) accumulator 
              else name.toString + accumulator
            case hd::b  => if(hd.toString=="%n"){ 
              modifyStringRepresent(name,b,accumulator,false)
            } else if(hd.toString.length()>1 && hd.toString.substring(0,2)=="%I") 
              modifyStringRepresent(name,b,accumulator,hasSymbol) 
              else modifyStringRepresent(name,b,accumulator+ separator +hd.toString,hasSymbol)
          }
        }
        constants.map({case (x,y)=>modifyStringRepresent(x,y,"",true)})
      } catch {
        case e : Throwable => Nil
      }
  }
}