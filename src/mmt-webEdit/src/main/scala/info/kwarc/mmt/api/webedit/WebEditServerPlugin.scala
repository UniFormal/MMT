package scala.info.kwarc.mmt.api.webedit
import java.lang.String
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.modules.DeclaredTheory
import objects._
import libraries._
import scala.util.parsing.json._
import scala.concurrent._
import tiscaf._
import scala.collection.mutable.HashMap._

class WebEditServerPlugin extends ServerExtension("editing") with Logger {
  
  def error(msg : String) : HLet = {
    log("ERROR: " + msg)
    Server.errorResponse(msg)
  }
  
  def apply(uriComps: List[String], query: String, body : Body): HLet = {
    try {
      uriComps match {
        case "save" :: _ => getSaveResponse
        case "preview" :: _ => getPreviewResponse
        case "autocomplete" :: _ => getAutocompleteResponse
        case "resolve" :: _ => getResolveResponse
        case "minIncludes" :: _ => getMinIncludes
        case "symbolCompletion" :: _ => getSymbolCompletion
        case "termInference" :: _ => getTermInference
        case _ => error("Invalid request: " + uriComps.mkString("/"))
      }
    } catch {
      case e : Error => 
        log(e.longMsg) 
        Server.errorResponse(e.longMsg)
      case e : Exception => 
        error("Exception occured : " + e.getStackTrace())
    }
  }
  
  private def getSaveResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      val response = "TODO"
      Server.TextResponse(response).aact(tk)
    }
  }
  
  private def getPreviewResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val response = "TODO"
      Server.TextResponse(response).aact(tk)
    }
  }
 
  
  private def getResolveResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val symbol = params.get("symbol").getOrElse(throw ServerError("No symbol found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)
      
      val respMap = new collection.mutable.HashMap[String, Any]()
      Names.resolveIncludes(OMMOD(mpath),symbol)(controller.library) match {
        case None => respMap("found") = true
        case Some(myPaths) => 
          respMap("found") = false
          respMap("options") = new JSONArray(myPaths.map(_.from.toPath))
      }
      Server.JsonResponse(new JSONObject(respMap.toMap)).aact(tk)
    }
  }
  
   
  private def getCoverCardinality(includes: List[MPath], theory : MPath)=  {
      val thy = controller.get(theory) match {
      case theor: DeclaredTheory => theor
      case _ => throw ServerError("No theory found")
    }
      val fromCheck = thy.getIncludes
      fromCheck.filter(x=>includes.contains(x))
  }
  
  
  private def getMax(includes : List[MPath], theories : List[MPath], current:Int,currentCover: List[MPath]): List[MPath] = 
    theories match {
      case hd::tl => 
        val card = getCoverCardinality(includes,hd)
        if(card.length > current) getMax(includes,tl,card.length,card)
        else
          getMax(includes,tl,current,currentCover)
      case Nil => currentCover
    }
  
  
  private def minInclLevel(incl : List[MPath], availTheories: List[MPath],CoverTheories: List[MPath]): List[MPath] = { 
    val k = getMax(incl,availTheories,0,Nil)
    val removed = incl.filterNot(x=>k.contains(x))
    if(removed == Nil) k:::CoverTheories else minInclLevel(removed,availTheories,k:::CoverTheories)  
  }
  
  
  private def minIncl(to: MPath) : List[MPath] = {
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
  
 
  private def getMinIncludes : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)		
    
      val newIncludes = minIncl(mpath)
      val response = new JSONArray(newIncludes.map(_.toPath))
      Server.JsonResponse(response).aact(tk)
    }
  }
    
   
  private def getAutocompleteResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)
      
      val myPaths = Names.resolve(OMMOD(mpath), Nil, prefix)(controller.globalLookup)
      val response = new JSONArray(myPaths.map(_.path.toPath))
      Server.JsonResponse(response).aact(tk)
    }
  }
  
  private def getTermInference : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val termS = params.get("term").getOrElse(throw ServerError("No type found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
          
      val mpath = Path.parseM(mpathS, mmt.mmtbase)
      val sref = new SourceRef(mpath.doc.uri, SourceRegion(SourcePosition(-1,0,0),SourcePosition(-1,0,1)))
      
      val term = controller.termParser(ParsingUnit(sref, OMMOD(mpath), Context(), termS), e => throw e)
      
      def getHoles(term: Term , context : Context) : List[(Term,Context)] = {
        term match {
          case Hole(t) => (t,context)::Nil
          case OMBINDC(nterm,ncontext,nbodyList)=> (nterm::nbodyList).flatMap(getHoles(_,context++ncontext))
          case OMBIND(nterm,ncontext,nbody) => getHoles(nbody, context++ncontext)
          case OMA(f, args) => (f :: args).flatMap(getHoles(_, context))
          case _ => Nil
        }
      }
      val returnNoHoles = "Term Complete"
      val holeContextList =  getHoles(term,Context())
      
      //response Term Complete if there are no Holes, No Rules if no rule is applicable otherwise return the rules
      holeContextList match { 
        case Nil => val returnNoHoles = "Term Complete" :: Nil
        			Server.JsonResponse(new JSONArray(returnNoHoles)).aact(tk)
        case _ =>  
          val hole = holeContextList.head._1
          val context = holeContextList.head._2
          val prover = new Prover(controller) 
          val rules = prover.applicable(hole) (Stack(Frame(OMMOD(mpath),context)))
          rules match{ 
        			case Nil => val returnNoRules = "No Rules" :: Nil
        						Server.JsonResponse(new JSONArray(returnNoRules)).aact(tk)
        			case _ => val response = new JSONArray(rules.map(_.label.toString))
        						Server.JsonResponse(response).aact(tk)
          }
      }
    }
    
  }
  
  private def getSymbolCompletion : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)
      
      val myPaths = Names.resolve(OMMOD(mpath), Nil, prefix)(controller.globalLookup)
      val paths=(myPaths.map(_.path)) 
      
      val constants = paths.map(controller.get) collect {
       case c : Constant => c.not match {
         case None => c.name -> Nil 
         case Some(a) => c.name -> a.fixity.markers
       }
      }
      
      def modifyStringRepresent (name:LocalName,markers:List[Marker],accumulator:String,hasSymbol:Boolean) : (String) = {
		val separator = " " 
         markers match {
		  case Nil => if(hasSymbol) accumulator 
				  	  else name.toString + accumulator
		  case hd::b  => if(hd.toString=="%n"){ 
			  				modifyStringRepresent(name,b,accumulator,false)
			  				}
						else if(hd.toString.length()>1 && hd.toString.substring(0,2)=="%I") 
							modifyStringRepresent(name,b,accumulator,hasSymbol) 
		  				else 
		  				    modifyStringRepresent(name,b,accumulator+ separator +hd.toString,hasSymbol)
		}
		
      }

      val stringResponse = constants.map({case (x,y)=>modifyStringRepresent(x,y,"",true)})
      val response = new JSONArray(stringResponse)
      Server.JsonResponse(response).aact(tk)
    }
  }
  
}
