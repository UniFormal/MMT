package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import notations._
import frontend._
import backend._
import objects._
import utils._
import documents._
import flexiformal._
import scala.collection.immutable.{HashMap}
import scala.xml.Node
import parser._

class MWSHarvestExporter extends Exporter {
  val outDim = Dim("export", "mws")
  val key = "mws-harvest"
  override val outExt = "harvest"

    
  def exportTheory(t: DeclaredTheory, bf: BuildFile) { 
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
     def narrToCML(n : NarrativeObject) : List[scala.xml.Node] = n match {
        case nt : NarrativeTerm => List(nt.term.toCML)
        case nn : NarrativeNode => nn.child.flatMap(narrToCML)
        case _ => Nil
    }
    t.getDeclarations foreach {d =>
      d.getComponents.foreach {
         case (comp, tc: AbstractTermContainer) =>
            tc.get.foreach {t =>
               val node = <mws:expr url={CPath(d.path,comp).toPath}>{t.toCML}</mws:expr>
               rh(node.toString + "\n")
            }
         case (comp, no : NarrativeObject) => 
           val exprs = narrToCML(no)
           exprs foreach {cml =>
            val out = <mws:expr url={CPath(d.path, comp).toPath}><content>{cml}</content></mws:expr>
            rh(out.toString + "\n")
           }
         case _ => 
      }
    }
    rh("</mws:harvest>\n")
  }
  
  def exportView(v: DeclaredView, bf: BuildFile) { 
    //excluding expressions from views for now
  }
  
  def exportNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
    //Nothing to do - MathML in namespaces
  }

  def exportDocument(doc : Document, bt: BuildTask) {
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    try {
      doc.components collect {
        case _ =>
      }
    } catch {
      case e : GetError => //doc not found, can ignore 
    }   
    rh("</mws:harvest>\n")
  }
}


class ModuleFlatener(controller : Controller) {
  val memory = controller.memory
  val modules = controller.memory.content.getModules
  
  def flatten : Controller = {
    val newCont = new Controller();
    modules collect {
      case t : DeclaredTheory => 
        
    }
    
    controller
  }
  
  def flatten(t : DeclaredTheory) {
    val tbar = new DeclaredTheory(t.parent, t.name, t.meta)
    val views = modules collect {
      case v : DeclaredView if v.to == t.path => v
    } // all views to T
    
    views foreach { v => 
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : DeclaredTheory if memory.content.imports(s, sprime.toTerm) => 
        // here we have v : s -> t and sprime includes s -- (include relation is transitive, reflexive)
        // therefore we make a structure with sprime^v and add it to tbar
        val s = new DeclaredStructure(tbar.toTerm, LocalName(v.path), sprime.path, false)
        sprime.getDeclarations foreach {d => 
          s.add(rewrite(d))          
        }
        tbar.add(s)
      }
    }
  }
  
  private def makeRules(v : DeclaredView) : HashMap[Path, Term] = {
    val path = v.from.toMPath
    var rules = new HashMap[Path,Term]
    v.components collect {
      case c : Constant =>
        c.df.foreach {t =>
          rules += (path ? c.name -> t)
        }
      case d : DefinedStructure => 
        try {
          controller.get(d.df.toMPath) match {
            case d : DeclaredView => rules ++= makeRules(v)
            case _ => //nothing to do
          }
          
        } catch {
          case e : Error => //nothing to do
          case e : Exception => //nothing to do
        }
    }
    rules
  }
  
  
  
  private def rewrite(d : Declaration)(implicit rules : HashMap[Path, Term]) : Declaration = d match {
    case c : Constant =>
      val newtpC = TermContainer(c.tp.map(rewrite))
      val newdfC = TermContainer(c.df.map(rewrite))
      new Constant(c.home, c.name, c.alias, newtpC, newdfC, c.rl, c.notC)
    case x => x
  }
  
  
  private def rewrite(t : Term)(implicit rules : HashMap[Path, Term]) : Term = t match {
    case OMS(p) => 
      if (rules.isDefinedAt(p)) rules(p) else t
    case OMA(f, args) => OMA(rewrite(f), args.map(rewrite))
    case OMBINDC(b, con, bodies) => OMBINDC(rewrite(b), rewrite(con), bodies.map(rewrite))
    case _ => t
  }
   
  private def rewrite(con : Context)(implicit rules : HashMap[Path, Term]) : Context = {
    val vars = con.variables map {
      case VarDecl(n, tp, df) => VarDecl(n, tp.map(rewrite), df.map(rewrite))
    }
    Context(vars : _*)
  }
}


class FlattenningExporter extends Exporter {
  val outDim = Dim("export", "mws-flat")
  val key = "mws-flat-harvest"
  override val outExt = "harvest"
  
  def exportTheory(t : DeclaredTheory, bf : BuildFile) {
    
    
    
  }
    def exportView(v: DeclaredView, bf: BuildFile) { 
    //excluding expressions from views for now
  }
  
  def exportNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
    //Nothing to do - MathML in namespaces
  }
  
  def exportDocument(doc : Document, bt: BuildTask) {
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    try {
      doc.components collect {
        case _ =>
      }
    } catch {
      case e : GetError => //doc not found, can ignore 
    }   
    rh("</mws:harvest>\n")
  }
  
}
/**
 * a wrapper around a remote MathWebSearch server
 * @param url the URL of the server
 */
class MathWebSearch(url: java.net.URL) {
   val qvarBinder = utils.mmt.mmtcd ? "qvar"
   private val qvarMarkers = List(Delim("$"), Var(1, false, Some(Delim(","))), Delim(":"), Arg(2))
   val qvarNot = new TextNotation(qvarBinder, Mixfix(qvarMarkers), presentation.Precedence.infinite, None)

   private def queryToXML(query: MathWebSearchQuery) = {
      val queryCML = query.term match {
         case OMBIND(OMS(this.qvarBinder), qvars, qBody) => qBody.toCMLQVars(qvars)
         case t => t.toCML
      }
      <mws:query xmlns:mws="http://www.mathweb.org/mws/ns" xmlns:m="http://www.w3.org/1998/Math/MathML"
             limitmin={query.limitmin.toString} answsize={query.answsize.toString} totalreq="yes" output="xml">
            <mws:expr>{queryCML}</mws:expr>
      </mws:query>
   }
   
   /** sends a query
    *  @param query the query, using this.qvarBinder at the toplevel to bind the query variables
    *  @return MathWebSearch's reply
    */
   def apply(query: MathWebSearchQuery): List[MathWebSearchAnswer] = {
      val responseXML = utils.xml.post(url, queryToXML(query))
      val response = responseXML match {
         case <mws:answset>{answs @_*}</mws:answset> =>
            answs.toList.map {
               case n @ <mws:answ>{_*}</mws:answ> =>
                  val p = Path.parseC(utils.xml.attr(n, "uri"), utils.mmt.mmtbase)
                  val xpS = utils.xml.attr(n, "xpath")
                  // xpath has format "/*[Int].../*[Int]
                  val xp = xpS.substring(3, xpS.length-1).split("\\]/\\*\\[").toList.map(_.toInt - 1).tail
                  MathWebSearchAnswer(p, Position(xp))
            }
      }
      response
   }
}

/** an answer in an MathWebSearch answer set */
case class MathWebSearchAnswer(cpath: CPath, pos: Position)

case class MathWebSearchQuery(term: Term, answsize: Int = 1000, limitmin: Int = 0)