package info.kwarc.mmt.frameit

import info.kwarc.mmt.api._

import uom._
import web._
import frontend._
import objects._
import symbols._
import modules._

import scala.collection._
import scala.collection.immutable._
import tiscaf._

case class FrameitError(val text : String) extends Error(text)


class FrameitPlugin extends ServerExtension("frameit") with Logger {
  
  override val logPrefix = "frameit"
    
   /** Server */
   def apply(uriComps: List[String], query : String, body : web.Body): HLet = {
     try {
       uriComps match {
         case "get" :: _ => GetResponse
         case _ => errorResponse("Invalid request: " + uriComps.mkString("/"))

       }
     } catch {
       case e : Error => 
         log(e.shortMsg)
         errorResponse(e.shortMsg)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage())
     }
   }
   
   private def CORS_AllowOrigin(origin : String) = true //for now
   
   private def checkCORS(tk : HTalk) : HTalk = tk.req.header("Origin")  match {
     case None => tk
     case Some(s) => CORS_AllowOrigin(s) match {
       case true => tk.setHeader(" Access-Control-Allow-Origin", s)
       case false => tk
     }
   }
   
   private def GetResponse : HLet = new HSimpleLet {
	 implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
     def act(tk : HTalk) = try {

       val cpathS = tk.req.param("solPath").getOrElse(throw FrameitError("no solPath found"))
       val vpathS = tk.req.param("viewPath").getOrElse(throw FrameitError("no viewPath found"))
       
       val cpath = Path.parse(cpathS) match {
         case c : CPath => c
         case gn : GlobalName => CPath(gn, DefComponent) //assuming definition
         case p => throw FrameitError("Expected CPath or Global Name found " + p)
       }
       
       val vpath = Path.parse(vpathS) match {
         case vp : MPath => vp
         case p => throw FrameitError("Expected MPath found " + p)
       }
       val view = controller.get(vpath) match {
         case d : DeclaredView => d
         case _ => throw FrameitError("expected view")
       }
       
       val tm = simplify(pushout(cpath, vpath), view.to.toMPath)
       var tmS = tm.toString
       TextResponse(tmS).aact(tk)
     } catch {
       case e : Error => log(e.shortMsg);errorResponse(e.shortMsg).aact(tk)
       case e : Exception => errorResponse("Exception occured : " + e.getMessage()).aact(tk)
     }
   }
   
   private def simplify(t : Term, home : MPath) : Term = {
     log("Before: " + t.toString)
     val tS = controller.simplifier(t, objects.Context(VarDecl(OMV.anonymous, None, Some(OMMOD(home)), None)))
     log("After: " + tS.toString)
     tS
   }
   
   
   def pushout(cpath : CPath, vpaths : MPath*) : Term = {
     val comp = controller.get(cpath.parent) match {
       case c : Constant => cpath.component match {
         case DefComponent => c.df.getOrElse(throw FrameitError("No definition found for constant: " + cpath.parent))
         case TypeComponent => c.tp.getOrElse(throw FrameitError("No type found for constant: " + cpath.parent))      
       }
       case s => throw FrameitError("Expected component term found " + s.toString)
     }
     log(comp.toString)
     vpaths.foldLeft(comp)((tm, v) => pushout(tm, v))
   }
   
   private def pushout(tm : Term, vpath : MPath) : Term = {
     val view = controller.get(vpath) match {
       case v : DeclaredView => v
       case s => throw FrameitError("Expected view found " + s.toString)
     }
     
     val rules = makeRules(view)
     pushout(tm)(rules)
   }
   
   private def makeRules(v : DeclaredView) : HashMap[Path, Term]= {
     v.from match {
       case OMMOD(p) =>
         var rules = new HashMap[Path,Term]
         v.getDeclarations collect {
           case c : Constant =>
             c.df.foreach {t =>
               println((p ? c.name).toString + " #->#" + t.toString)
               rules += (p ? c.name -> t)
             }
         }
         rules
       case _ => throw FrameitError("view.from not OMMOD " + v.from)
     }
   }
   
   private def pushout(t : Term)(implicit rules : HashMap[Path, Term]) : Term = t match {
     case OMS(p) => 
       if (rules.isDefinedAt(p)) rules(p) else t
     case OMA(f, args) => OMA(pushout(f), args.map(pushout))
     case OMBIND(b, con, body) => OMBIND(pushout(b), pushout(con), pushout(body))
     case _ => t
   }
   
   private def pushout(con : Context)(implicit rules : HashMap[Path, Term]) : Context = {
     val vars = con.variables map {
       case VarDecl(n, tp, df, not) => VarDecl(n, tp.map(pushout), df.map(pushout), not)
     }
     Context(vars : _*)
   }
   
   // Utils
  private def bodyAsString(tk: HTalk): String = {
    val bodyArray: Array[Byte] = tk.req.octets.getOrElse(throw  FrameitError("no body found"))
    new String(bodyArray, "UTF-8")
  }
  
  private def errorResponse(text : String) : HLet = {
    TextResponse(s"MMT Error in FrameIT extension: $text ")
  }
  
  /**
   * A text response that the server sends back to the browser
   * @param text the message that is sent in the HTTP body
   */
  private def TextResponse(text: String): HLet = new HSimpleLet {    
    def act(tk: HTalk) {
      val out = text.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
        .write(out)
    }
  }
  
}