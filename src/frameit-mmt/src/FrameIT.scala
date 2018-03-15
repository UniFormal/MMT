package info.kwarc.mmt.frameit

import info.kwarc.mmt.api._
import uom._
import web.{Body, _}
import frontend._
import info.kwarc.mmt.api.backend.XMLReader
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.utils.URI
import objects._
import symbols._
import modules._

import scala.collection._
import scala.collection.immutable._


case class FrameitError(text : String) extends Error(text)

class FrameViewer extends Extension {

  def pushout(cpath : CPath, vpaths : MPath*) : Term = {
     val comp = controller.get(cpath.parent) match {
       case c : Constant => cpath.component match {
         case DefComponent => c.df.getOrElse(throw FrameitError("No definition found for constant: " + cpath.parent))
         case TypeComponent => c.tp.getOrElse(throw FrameitError("No type found for constant: " + cpath.parent))
       }
       case s => throw FrameitError("Expected component term found " + s.toString)
     }
     pushout(comp, vpaths : _*)
   }

  def pushout(tm : Term, vpaths : MPath*) : Term = {
    vpaths.foldLeft(tm)((t, v) => pushoutOne(t, v))
  }

   private def pushoutOne(tm : Term, vpath : MPath) : Term = {
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
           case c: Constant =>
             c.df.foreach { t =>
               c.name match {
                 case LocalName(ComplexStep(path: MPath) :: ln) =>
                   rules += (p ? ln -> t)
                 case _ =>
               }
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

   private def pushout(con : Context)(implicit rules : HashMap[Path, Term]) : Context = con map (_ map pushout)
}

class FrameitPlugin extends ServerExtension("frameit") with Logger with MMTTask {

  override val logPrefix = "frameit"
  val test : Class[FrameViewer] = classOf[FrameViewer]
  lazy val fv = controller.extman.get(classOf[FrameViewer]).headOption.getOrElse {
    val a = new FrameViewer
    controller.extman.addExtension(a)
    a
  }//new FrameViewer(controller)

   /** Server */

   /*
   private def CORS_AllowOrigin(origin : String) = true //for now

   private def checkCORS(tk : HTalk) : HTalk = tk.req.header("Origin")  match {
     case None => tk
     case Some(s) => CORS_AllowOrigin(s) match {
       case true => tk.setHeader(" Access-Control-Allow-Origin", s)
       case false => tk
     }
   }
   */
   /*
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

       val tm = simplify(fv.pushout(cpath, vpath), view.to.toMPath)
       var tmS = tm.toString
       TextResponse(tmS).aact(tk)
     } catch {
       case e : Error => log(e.shortMsg);errorResponse(e.shortMsg).aact(tk)
       case e : Exception => errorResponse("Exception occured : " + e.getMessage).aact(tk)
     }
   }
   */

   private def simplify(t : Term, home : MPath) : Term = {
     log("Before: " + t.toString)
     //val solver = new Solver(controller,cu,rules)
     val th = controller.get(home) match {
       case thx : DeclaredTheory => thx
       case _ => throw FrameitError("No DeclaredTheory: " + home)
     }
     controller.simplifier.apply(th)
     val con = (objects.Context(home) :: th.getIncludes.map(p => objects.Context(p))).flatten
     //val rules = RuleSet.collectRules(controller,con)
     //println(rules.getAll.toList)
     //val solver = new Solver(controller,CheckingUnit.byInference(None,con,t),rules)
     val traverser = new StatelessTraverser {
       override def traverse(t: Term)(implicit con: Context, init: State): Term = t match {
         case tm@OMS(p) =>
           controller.get(p) match {
             case const : FinalConstant if const.df.isDefined =>
               Traverser(this,const.df.get)
             case _ => tm
           }
         case _ => Traverser(this,t)
       }
     }
     var tS = traverser(t,())
     /*
     tS = solver.safeSimplifyUntil(tS)({
       case tm : OMLIT => Some(tm)
       case tm : UnknownOMLIT => Some(tm)
       case _ => None
     })(objects.Stack(con),NoHistory)._1
     */
     //val tS = solver.forcesimplify(t)(objects.Stack(con),NoHistory)
     tS = controller.simplifier(tS,con)//controller.simplifier(t, objects.Context(home))
     log("After: " + tS.toString)
     tS
   }
   /*
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
    *
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

*/

  lazy val checker = {
    val ret = new MMTStructureChecker(new RuleBasedChecker)
    controller.extman.addExtension(ret)
    ret
  }
  implicit val ce : CheckingEnvironment = new CheckingEnvironment(controller.simplifier,ErrorThrower,RelationHandler.ignore, this)

  val dpath = DPath(URI.http colon "cds.omdoc.org") / "FrameIT"
  val sitpath = dpath ? "situation_theory"
  val viewpath = dpath ? "situation_problem_view"
  def view = controller.get(viewpath) match {
    case dv : DeclaredView => dv
    case _ => throw new FrameitError("view does not exist!")
  }
  def sittheory = controller.get(sitpath) match {
    case dt : DeclaredTheory => dt
    case _ => throw new FrameitError("situation theory does not exist!")
  }

  def getdomcod = {
    val dom = view.from match {
      case OMMOD(p) => controller.get(p) match {
        case th : DeclaredTheory => th
        case _ => throw FrameitError("DeclaredView expected")
      }
      case _ => throw FrameitError("Expected MPath, found " + view.from)
    }
    val cod = view.to match {
      case OMMOD(p) => controller.get(p) match {
        case th : DeclaredTheory => th
        case _ => throw FrameitError("DeclaredTheory for situation theory expected")
      }
      case _ => throw FrameitError("Expected MPath, found " + view.from)
    }
    (dom,cod)
  }

  implicit val unifun : StructuralElement => Unit = x => controller.add(x)

  def apply(request: ServerRequest): ServerResponse = request.path match {
    case "init" :: rest => try {
      controller.handleLine("build FrameIT mmt-omdoc")
      ServerResponse.TextResponse("Success")
    } catch {
      case e : Exception => ServerResponse.errorResponse("Error initializing: " + e.getMessage)
    }
    case "pushout" :: rest => if (request.query.trim.startsWith("theory=")) {
      try {
        val sol = Path.parse(request.query.trim.drop(7)) match {
          case m : MPath => controller.get(m) match {
            case t : DeclaredTheory => t
            case _ => throw FrameitError("Solution theory not a DeclaredTheory")
          }
          case _ => throw FrameitError(request.query.trim.drop(7) + " not an MPath")
        }
        controller.simplifier(sol)
        try { checker.apply(sol) } catch {case e : Exception =>
          throw new FrameitError("Solution theory does not type check: " + e.getMessage)}
        val nodes = sol.getConstants.map(c => {
          val tp = c.tp.map(x => simplify(fv.pushout(c.path $ TypeComponent, viewpath), view.to.toMPath))
          val df = c.df.map(x => simplify(fv.pushout(c.path $ DefComponent, viewpath), view.to.toMPath))
          Constant(c.home,c.name,Nil,tp,df,None)
        }).map(_.toNode)
        ServerResponse.XmlResponse(<theory>{nodes}</theory>)
      } catch {
        case e : Exception => ServerResponse.errorResponse(e.getMessage)
      }
    } else ServerResponse.errorResponse("Malformed query")
    case "add" :: rest =>
      try {
        request.body.asXML match {
          case <content>{seq @ _*}</content> =>
            val sitth = seq.head
            val viewxml = seq.tail.head
            val reader = new XMLReader(controller)

            try {
              reader.readDocument(dpath, sitth)
            } catch {
              case e: Exception =>
                throw new FrameitError("Malformed omdoc in situation theory: " + e.getMessage)
            }
            try {
              reader.readDocument(dpath, viewxml)
            } catch {
              case e: Exception =>
                throw new FrameitError("Malformed omdoc in view: " + e.getMessage)
            }
            val (dom,cod) = getdomcod
            controller.simplifier(view)
            controller.simplifier(dom)
            controller.simplifier(cod)
            try { checker.apply(dom) } catch {case e : Exception =>
              throw new FrameitError("Domain of view does not type check: " + e.getMessage)}
            try { checker.apply(cod) } catch {case e : Exception =>
              throw new FrameitError("Codomain of view does not type check: " + e.getMessage)}
            try { checker.apply(view) } catch {case e : Exception =>
              throw new FrameitError("View does not type check: " + e.getMessage)}
            try { checker.apply(sittheory) } catch {case e : Exception =>
              throw new FrameitError("Situation theory does not type check: " + e.getMessage)}
            val istotal = dom.getConstants.forall(c => {
            // println("Checking " + c.name)
              view.getDeclarations.exists(d => d.name == ComplexStep(dom.path) / c.name)
            })
            if(!istotal) throw FrameitError("View not total")
            ServerResponse.TextResponse("Okay")
          case _ => throw new FrameitError("Malformed FrameIT request : not of form <content><THEORY><VIEW></content>")
        }
      } catch {
        case e : Exception => ServerResponse.errorResponse(e.getMessage)
      }
    case _ => ServerResponse.errorResponse("Neither \"add\" nor \"pushout\"")
  }

  def run(solthS : String, vpathS : String) : String = {
    /*
    val cpath = Path.parse(cpathS) match {
      case c : CPath => c
      case gn : GlobalName => CPath(gn, DefComponent) //assuming definition
      case p => throw FrameitError("Expected CPath or Global Name found " + p)
    }
    */
    val sol = Path.parse(solthS) match {
      case m : MPath => controller.get(m) match {
        case t : DeclaredTheory => t
        case _ => throw FrameitError("DeclaredTheory expected")
      }
      case _ => throw FrameitError("DeclaredTheory expected")
    }
    val vpath = Path.parse(vpathS) match {
      case vp : MPath => vp
      case p => throw FrameitError("Expected MPath found " + p)
    }
    val view = controller.get(vpath) match {
      case d : DeclaredView => d
      case _ => throw FrameitError("expected view")
    }
    val dom = view.from match {
      case OMMOD(p) => controller.get(p) match {
        case th : DeclaredTheory => th
        case _ => throw FrameitError("DeclaredTheory expected")
      }
      case _ => throw FrameitError("Expected MPath found " + view.from)
    }
    val cod = view.to match {
      case OMMOD(p) => controller.get(p) match {
        case th : DeclaredTheory => th
        case _ => throw FrameitError("DeclaredTheory expected")
      }
      case _ => throw FrameitError("Expected MPath found " + view.from)
    }

    val checker = new MMTStructureChecker(new RuleBasedChecker)
    controller.extman.addExtension(checker)
    implicit val ce : CheckingEnvironment = new CheckingEnvironment(controller.simplifier,ErrorThrower,RelationHandler.ignore, this)
    controller.simplifier(view)
    controller.simplifier(sol)
    controller.simplifier(dom)
    controller.simplifier(cod)
    //controller.simplifier.flatten(th)
    checker.apply(dom)
    checker.apply(cod)
    checker.apply(view)
    checker.apply(sol)
    val istotal = dom.getConstants.forall(c => {
      // println("Checking " + c.name)
      view.getDeclarations.exists(d => d.name == ComplexStep(dom.path) / c.name)
    })
    if(!istotal) throw FrameitError("View not total")
    sol.getConstants.map(c => {
      val tp = c.tp.map(x => simplify(fv.pushout(c.path $ TypeComponent, vpath), view.to.toMPath))
      val df = c.df.map(x => simplify(fv.pushout(c.path $ DefComponent, vpath), view.to.toMPath))
      Constant(c.home,c.name,Nil,tp,df,None)
    }).map(_.toNode.toString).mkString("\n")

    //simplify(fv.pushout(cpath, vpath), view.to.toMPath)
  }

}
