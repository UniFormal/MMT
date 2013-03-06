package info.kwarc.mmt.latex
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import objects._
import parser._
import symbols._
import presentation._
import frontend._
import scala.collection._
import zgs.httpd._

class LatexState(val dpath : DPath, controller : Controller) {
  
  val dictionary = new mutable.HashMap[LocalName, String] // ?needed
  
  val parser = new LatexStructureParser(this, controller)        

  def context = varContexts match {
    case Nil => Context()
    case hd :: Nil => hd
    case l => l.reduceLeft((x,y) => x ++ y)
  }
  
  var varContexts : List[Context] = Nil
  def addContext(con : Context = Context()) = varContexts ::= con
  def addVar(v : VarDecl) = varContexts = (varContexts.head ++ v) :: varContexts.tail
  def clearContext() = varContexts = varContexts.tail
  
  var mod : MPath = null //current module
  
  val seqReader = new SeqBufReader
  val parserState = new ParserState(new Reader(seqReader), dpath)
  
  
  def setParserState(text : String) {
    seqReader.appendLine(text)
  }
  
  def waitDone() {  
    
    while(!seqReader.isDone) {
      Thread.sleep(10)
    } 
    Thread.sleep(100)
  }
  
}

class LatexPresenter extends Presenter with ServerPlugin {
   var currentJob : String = ""
     
     
   val states = new mutable.HashMap[String, LatexState] 
   
   
   
   def isApplicable(format: String) = format == "latex"
   def apply(c : StructuralElement, rh : RenderingHandler) {
      rh(c.toString)
   }
   
   def apply(o : Obj, rh : RenderingHandler) = apply(o, rh, states(currentJob).dictionary)
   
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
   
   private def ttoLatex(tm : Term, scope : MPath) : String = {
     val notations = AbstractObjectParser.getNotations(controller, OMMOD(scope))
     val notationsHash = notations.map(n => (n.name, n)).toMap
     ttoLatex(tm)(notationsHash)
     
   }

   
   private def ttoLatex(tm : Term)(implicit notations : Map[GlobalName,TextNotation]) : String = {
     controller.pragmatic.pragmaticHead(tm) match {
       case OMS(p) => "\\" + Utils.latexName(p)
       case OMA(OMS(p), args) => notations.get(p) match {
         case Some(not) => 
           val markers = not.markers
           val sqargs = markers collect {
             case SeqArg(n,sep) => (n,sep) 
           }
           sqargs match {
             case Nil => ttoLatex(OMS(p)) + args.map(ttoLatex).mkString("{","}{","}") 
             case hd :: Nil => //one seq arg, must detect corresponding content arguments
               val pos = hd._1
               val sep = hd._2
               val arity = not.getArity.length
               val (begin,rest) = args.splitAt(pos - 1)
               val (seq, end) = rest.splitAt(args.length - arity)
               s"\\mmtseq{${seq.map(ttoLatex).mkString(sep.s)}}"
             
              case _ => throw ImplementationError("Multiple sequence arguments in the same notation not supported yet")
           }        
         case None => ttoLatex(OMS(p)) + args.map(ttoLatex).mkString("{","}{","}") 
     }
       case OMBIND(OMS(p), con, body) => notations.get(p) match {
         case Some(not) =>
           not.markers find {
             case Var(_,_,Some(sep)) => true
             case _ => false
           } match {
             case Some(Var(_,_,Some(sep))) => 
               val vars = con.variables.map(vToLatex).mkString(sep.s)
               s"${ttoLatex(OMS(p))}{$vars}{${ttoLatex(body)}}"
             case _ => 
               assert(con.variables.length == 1, "expected one variable argument")
               s"${ttoLatex(OMS(p))}{${vToLatex(con.variables.head)}}{${ttoLatex(body)}}"
           }
         case None => 
           assert(con.variables.length == 1, "expected one variable argument")
           val v = con.variables.head
           s"${ttoLatex(OMS(p))}{${vToLatex(v)}}{${ttoLatex(body)}}"
       }
       case OMA(OMV(v), args) => s"${ttoLatex(OMV(v))} ${args.map(ttoLatex).mkString(" ")}"
       case OMV(n) => n.toString
       case t => println("@@@@:\n" +  t.toNode); throw ImplementationError("not done yet")
 
     }
   }
   
   def vToLatex(v : VarDecl)(implicit notations : Map[GlobalName,TextNotation]) : String = v match {
     case VarDecl(a, Some(tp), None, _*) => println(tp.toNode); a.toString + " : " + ttoLatex(tp) 
     case _ => v.name.toString
   }    
   
   /** Server */
   
   def isApplicable(uriComp : List[String]) = {
     println("uriComp" + uriComp)
     uriComp.head == ":latex"
   }
   
   def apply(uriComps: List[String]): Option[HLet] = uriComps.tail match {
     case "postdecl" :: _ => Some(PostDeclResponse)
     case "getobjpres" :: _ => Some(GetObjPresResponse)
     case "context" :: _ => Some(ContextResponse)
     
     case _ => None
   }
 
   private def ContextResponse : HLet = new HLet {
    def act(tk : HTalk) {
    	val jobname = tk.req.header("jobname").getOrElse(throw ServerError(<error><message>found no jobname for request</message></error>))
    	val request = tk.req.header("request").getOrElse(throw ServerError(<error><message>found no context update request</message></error>))
    	val text = bodyAsString(tk)
    	println("###### received context : " + request + ":" + text)
    	if (!states.isDefinedAt(jobname)) {
    	  throw ServerError(<error><message>given jobname: {jobname} not active</message></error>)
    	} else {
    	  val state = states(jobname)
    	  request match {
    	    case "clear" => state.clearContext()
    	    case "new" => state.addContext()
    	    case "addvar" =>  text.split(":").toList match {
    	      case name :: tpS :: Nil => 
    	        println("currentContext :" + state.context)
    	        val pu = new parser.ParsingUnit(parser.SourceRef(state.mod.doc.uri,SourceRegion.ofString(tpS)), OMMOD(state.mod), state.context, tpS)
    	        val tp = controller.termParser(pu)
    	        val v = VarDecl(LocalName(name), Some(tp), None)
    	        state.addVar(v)
    	      case _ =>  throw ServerError(<error><message>invalid var: {text}</message></error>)

    	    }
    	    
    	  }
    	  TextResponse("Success").act(tk)
    	}		
    }
   }
   
   
   private def PostDeclResponse : HLet = new HLet {
    def act(tk : HTalk) {
      val text = bodyAsString(tk)
      val jobname = tk.req.header("jobname").getOrElse(throw ServerError(<error><message>found no jobname for request</message></error>))
      val dpathS = tk.req.header("dpath").getOrElse(throw ServerError(<error><message>found no dpath</message></error>))
      val dpath = Path.parseD(dpathS, utils.mmt.mmtbase)
      
      println("received : " + jobname + " : " + text)
      if (!states.isDefinedAt(jobname)) {
 

        val state = new LatexState(dpath, controller)
        state.setParserState(text)
        states(jobname) = state
        
        val thread = new Thread(new Runnable {          
          def run() {
            state.parser(state.parserState, dpath)
          }
        })
        thread.start        
        state.waitDone
        val resp = state.parser.addedMacros.mkString("\n")
        state.parser.addedMacros = Nil
        TextResponse(resp).act(tk)	
      } else {
        val ltxState = states(jobname)
        ltxState.setParserState(text)
        ltxState.waitDone
        val resp = ltxState.parser.addedMacros.mkString("\n")
        println("RESP :" + resp + ": END RESP")
        ltxState.parser.addedMacros = Nil
        TextResponse(resp).act(tk)
      }
    }
  }
  
   
  private def GetObjPresResponse : HLet = new HLet {
    def act(tk : HTalk) {
      val text = bodyAsString(tk)
      val jobname = tk.req.header("jobname").getOrElse(throw ServerError(<error><message>found no dpath</message></error>))
      val state = states(jobname)
      val home = tk.req.header("home").map(Path.parse).getOrElse {        
        state.mod
      }
      
      println("received get : " + text)
      println(home)
      println("Context is : " + state.context)
      
      home match { 
        case mod : MPath => 
          val pu = new parser.ParsingUnit(parser.SourceRef(mod.doc.uri,SourceRegion.ofString(text)), objects.OMMOD(mod), state.context, text)
          val tm = controller.termParser(pu)
          val (unknowns,tmU) = parser.AbstractObjectParser.splitOffUnknowns(tm)
          println("Term Node" + tmU.toNode)
          val etp = LocalName("expected_type")
          val oc = new Solver(controller, OMMOD(mod), unknowns ++ VarDecl(etp, None, None))
          val j = Typing(Stack(mod, state.context), tmU, OMV(etp))
          oc(j)
          println(oc.getPartialSolution)
          oc.getSolution match {
            case Some(sub) =>
              val tmR = tmU ^ sub
              println("Sending ResponseR" + ttoLatex(tmR, mod))
              TextResponse(ttoLatex(tmR, mod)).act(tk)
            case None =>            
              println("Sending ResponseU" + ttoLatex(tmU, mod))
              // throw ServerError(<error><message> type reconstruction failed </message></error>)
              TextResponse(ttoLatex(tmU,mod)).act(tk) //until things are stable sending non-reconstructed term 
          }
        case _ => throw ServerError(<error><message>support for non-module paths not implemented yet</message></error>)
      } 
    }
  }     
  
  private def sanitizeInput(text : String) : String = {
    text.replaceAllLiterally("\\ldots", "…")
  }
  
  private def bodyAsString(tk: HTalk): String = {
    val bodyArray: Array[Byte] = tk.req.octets.getOrElse(throw ServerError(<error message="no body found"/>))
    val text = new String(bodyArray, "UTF-8")
    sanitizeInput(text)
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


class LatexStructureParser(ltxState : LatexState, controller : Controller) extends StructureAndObjectParser(controller) {
   var addedMacros : List[String] = Nil
  
   
   override def seCont(se: StructuralElement)(implicit state: ParserState) {
      log(se.toString)
      SourceRef.update(se, SourceRef(state.container.uri,currentSourceRegion))
      se match {
        case c : Constant => c.not match {
          case None => addedMacros ::= s"\\newcommand{\\${Utils.latexName(c.path)}}{${c.name.toString}}"
          case Some(notation) => addedMacros ::= Utils.makeMacro(notation) 
        }
        case m : modules.Module => ltxState.mod = m.path
        case _ => 
      }      
      controller.add(se)
   }
}

object Utils {
   private val sep = ""
   def latexName(mmtName : GlobalName) : String = "mmt" + sep + mmtName.module.toMPath.last + sep + mmtName.last
   
   private def makeMacro(marker : Marker, mmtName : GlobalName) : String = marker match {
     case Delim(str) => s"\\link{$str}{${mmtName.toPath}}"
     case SecDelim(str,_) => s"\\link{$str}{${mmtName.toPath}}"
     case Arg(i) => s"#${i.abs}"
     case SeqArg(i,d) => s"\\mmtseq{#${i.abs}}{${makeMacro(d, mmtName)}}"
     case Var(i, t, sO) => s"#${i.abs}" //TODO 
     case p : PlaceholderDelimiter => throw ImplementationError("Marker PlaceholderDelimiter shouldn't occur in Notations")
   }
    
    
   def makeMacro(not : TextNotation) : String = {
     val mmtName = not.name
     val markers = not.markers
     val body = markers.map(makeMacro(_, mmtName)).mkString(" ")
     val label = s"\\hyperdef{mmt}{${mmtName.toPath}}{}"
     val args = (1 to not.getArity.length).map(i => s"#$i").mkString("")
     
     s"\\gdef\\${Utils.latexName(mmtName)}$args{$body}\n $label\n"     
   }

   
   
}

