package info.kwarc.mmt.latex
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import objects._
import parser._
import presentation._
import frontend._
import scala.collection._
import zgs.httpd._

class LatexState(val dpath : DPath) {
  
  val context = new mutable.HashMap[LocalName, String]
  val seqReader = new parser.SeqBufReader
  val parserState = new ParserState(new Reader(seqReader), dpath)
  
  
  def setParserState(text : String) {
    seqReader.appendLine(text)
  }
  
  def waitDone() {  
    
    while(!seqReader.isDone) {
      Thread.sleep(10)
    } 
  }
  
}


class LatexPresenter extends Presenter with ServerPlugin {
   var currentJob : String = ""
     
     
   val states = new mutable.HashMap[String, LatexState] 
   
   
   
   def isApplicable(format: String) = format == "latex"
   def apply(c : StructuralElement, rh : RenderingHandler) {
      rh(c.toString)
   }
   
   def apply(o : Obj, rh : RenderingHandler) = apply(o, rh, states(currentJob).context)
   
   def apply(o: Obj, rh: RenderingHandler, con : Map[LocalName,String]) {
      o match {
         case OMS(p) => rh(s"\\${p.last}{${p.toPath}}")
         case OMA(OMID(p), args) =>
            rh(s"\\${p.last}{${p.toPath}}")
            args foreach {a =>
              rh("{")
              apply(a, rh)
              rh("}")
            }
         case OMBIND(OMID(p), Context(VarDecl(v, tpOpt, _)), scope) =>
         case OMV(v) => s"\\varref{${v.toPath}{${con(v)}}"
      }
   }
   
   
   /** Server */
   
   def isApplicable(uriComp : List[String]) = {
     println("uriComp" + uriComp)
     uriComp.head == ":latex"
   }
   
   def apply(uriComps: List[String]): Option[HLet] = uriComps.tail match {
     case "postdecl" :: _ => Some(PostDeclResponse)
     case "getobjpres" :: _ => Some(GetObjPresResponse)
     case _ => None
   }
 
   
   private def PostDeclResponse : HLet = new HLet {
    def act(tk : HTalk) {
      val text = bodyAsString(tk)
      val jobname = tk.req.header("jobname").getOrElse(throw ServerError(<error><message>found no jobname for request</message></error>))
      val dpathS = tk.req.header("dpath").getOrElse(throw ServerError(<error><message>found no dpath</message></error>))
      val dpath = Path.parseD(dpathS, utils.mmt.mmtbase)
      
      println("received : " + jobname + " : " + text)
      if (!states.isDefinedAt(jobname)) {
 
        val state = new LatexState(dpath)
        state.setParserState(text)
        states(jobname) = state
        
        val psr = new LatexStructureParser(controller)        
        val thread = new Thread(new Runnable {          
          def run() {
            psr(state.parserState, dpath)
          }
        })
        thread.start
        
        TextResponse("Initiated Parsing").act(tk)
      } else {
        val ltxState = states(jobname)
        ltxState.setParserState(text)
        ltxState.waitDone
        TextResponse("Continuing parsing").act(tk)
      }
    }
  }
   
    private def GetObjPresResponse : HLet = new HLet {
    def act(tk : HTalk) {
      val text = bodyAsString(tk)
      val home = tk.req.header("home").map(Path.parse).getOrElse {
        val jn = tk.req.header("jobname").getOrElse(throw ServerError(<error><message>found no dpath</message></error>))
        states(jn).parserState.home
      }
      
      println("received get : " + text)
      println(home)
      
      def toLatex(tm : Term) : String = tm match { //TODO quite a hack, should be improved
        case OMID(p) => p.last
        case OMA(f, args) => toLatex(f) match {
          case "@" => toLatex(args.head)
          case _ => "\\" + toLatex(f) + args.map(toLatex).mkString("{","}{","}")
        }
        case OMBIND(b, con, body) => toLatex(b) match {
          case "unknown" => toLatex(body) 
          case _ => "\\" + toLatex(b) + "{" + con.variables.map(v => vToLatex(v)).mkString(", ") + "}" + "{" + toLatex(body) + "}" 
        }
        case OMV(n) => n.toString
      }
      
      def vToLatex(v : VarDecl) : String = v match {
        case VarDecl(a, Some(tp), None, _*) => println(tp.toNode); a.toString + " : " + toLatex(tp) 
        case _ => v.name.toString
      } 
      
      home match { 
        case mod : MPath => 
          val pu = new parser.ParsingUnit(new parser.SourceRef(mod.doc.uri, parser.SourceRegion.parse("0.0.0-0.0.0")), objects.OMMOD(mod), objects.Context(), text)
          println(text)
          val tm = controller.termParser(pu)
          println(toLatex(tm))
          TextResponse(toLatex(tm)).act(tk)
        case _ => throw ServerError(<error><message>support for non-module paths not implemented yet</message></error>)
      }
      
    }
    
  }     
  
  private def bodyAsString(tk: HTalk): String = {
    val bodyArray: Array[Byte] = tk.req.octets.getOrElse(throw ServerError(<error message="no body found"/>))
    new String(bodyArray, "UTF-8")
  }
  
  /**
   * A text response that the server sends back to the browser
   * @param text the message that is sent in the HTTP body
   */
  private def TextResponse(text: String): HLet = new HLet {
    def act(tk: HTalk) {
      val out = text.getBytes("UTF-8")
      tk.setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
        .write(out)
        .close
    }
  }
  
   
}


class LatexStructureParser(controller : Controller) extends StructureAndObjectParser(controller) {
  var addedDecls : List[String] = Nil
  
   override def seCont(se: StructuralElement)(implicit state: ParserState) {
      log(se.toString)
      SourceRef.update(se, SourceRef(state.container.uri,currentSourceRegion))
      addedDecls ::= se.path.toPath 
      controller.add(se)
   }}
