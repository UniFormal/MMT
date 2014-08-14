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

    
  def exportTheory(t: DeclaredTheory, bf: BuildTask) { 
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
  
  def exportView(v: DeclaredView, bf: BuildTask) { 
    //excluding expressions from views for now
  }
  
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
    //Nothing to do - MathML in namespaces
  }

  def exportDocument(doc : Document, bt: BuildTask) {
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    try {
      doc.collectModules(controller) collect {
        case _ =>
      }
    } catch {
      case e : GetError => //doc not found, can ignore 
    }   
    rh("</mws:harvest>\n")
  }
}


class ModuleFlattener(controller : Controller) {
  val memory = controller.memory
  val modules = controller.memory.content.getModules
  
  def flatten : Controller = {
    val newCont = new Controller();
    modules collect {
      case t : DeclaredTheory => 
        
    }
    
    controller
  }
  
  def flatten(t : DeclaredTheory) : DeclaredTheory =  {
    println("Flattening: " + t.path)
    val tbar = new DeclaredTheory(t.parent, t.name, t.meta)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    val views = modules collect {
      case v : DeclaredView if v.to == t.toTerm => v
    } // all views to T
    
    views foreach { v => 
      val s = v.from
      implicit val rules = makeRules(v)
      println(memory.content.visible(s).toSet)
      modules collect {
        case sprime : DeclaredTheory if memory.content.visible(sprime.toTerm).toSet.contains(s) =>
          // here we have v : s -> t and sprime includes s -- (include relation is transitive, reflexive)
          // therefore we make a structure with sprime^v and add it to tbar
          /*
          val str = SimpleDeclaredStructure(tbar.toTerm, (LocalName(v.path) / sprime.path.toPath), sprime.path, false)
          sprime.getDeclarations foreach {d => 
            str.add(rewrite(d))
          }
          tbar.add(str)
          */
          //conceptually this should be a structure, but adding the declarations directly is more efficient
          sprime.getDeclarations foreach { 
            case c : Constant => tbar.add(rewrite(c, s.toMPath, tbar.path))
            case _ => //nothing for now //TODO handle structures
          }
      }
    }
    println(t.path + ": " + t.getDeclarations.length + " ->  " + tbar.getDeclarations.length)
    tbar
  }
  //Flattens by generating a new theory for every view, used for flatsearch
  def flattenFineGrained(t : DeclaredTheory) : List[DeclaredTheory] = {
    var thys : List[DeclaredTheory] = Nil
    val tbar = new DeclaredTheory(t.parent, t.name, t.meta)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    thys ::= tbar
    val views = modules collect {
      case v : DeclaredView if v.to == t.toTerm => v
    }
    
    views foreach { v=>
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : DeclaredTheory if memory.content.visible(sprime.toTerm).toSet.contains(s) => 
          val tvw = new DeclaredTheory(t.parent, sprime.name / v.name, t.meta)
          sprime.getDeclarations foreach { 
            case c : Constant => tvw.add(rewrite(c, v.path, tbar.path))
            case _ => //nothing for now //TODO handle structures
          }
          thys ::= tvw
      }
      
    }
    
    thys
  }
  
  
  private def makeRules(v : DeclaredView) : HashMap[Path, Term] = {
    val path = v.from.toMPath
    var rules = new HashMap[Path,Term]
    v.getDeclarations collect {
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
  
  private def rewrite(d : Declaration, vpath : MPath, newhome : MPath)(implicit rules : HashMap[Path, Term]) : Declaration = d match {
    case c : Constant =>
      val newtpC = TermContainer(c.tp.map(rewrite))
      val newdfC = TermContainer(c.df.map(rewrite))
      val newname = LocalName(vpath.toPath) / c.home.toMPath.toPath / c.name
      val newCons = new FinalConstant(OMMOD(newhome), newname, c.alias, newtpC, newdfC, c.rl, c.notC)
      import metadata._
      newCons.metadata.add(new MetaDatum(MetaDatum.keyBase ? LocalName("type"), OMSTR("Induced")))
      newCons.metadata.add(new MetaDatum(MetaDatum.keyBase ? LocalName("origin"), c.home))
      newCons.metadata.add(new MetaDatum(MetaDatum.keyBase ? LocalName("view"), OMID(vpath)))
      newCons
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
      case VarDecl(n, tp, df, not) => VarDecl(n, tp.map(rewrite), df.map(rewrite), not)
    }
    Context(vars : _*)
  }
}


class FlatteningMWSExporter extends Exporter {
  val outDim = Dim("export", "mws-flat")
  val key = "mws-flat-harvest"
  override val outExt = "harvest"
  lazy val mf = new ModuleFlattener(controller)
  def exportTheory(t : DeclaredTheory, bd : BuildTask) {
    val tbar = mf.flatten(t)
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
     def narrToCML(n : NarrativeObject) : List[scala.xml.Node] = n match {
        case nt : NarrativeTerm => List(nt.term.toCML)
        case nn : NarrativeNode => nn.child.flatMap(narrToCML)
        case _ => Nil
    }
    tbar.getDeclarations foreach {
      case d => d.getComponents.foreach {
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
    def exportView(v: DeclaredView, bd: BuildTask) { 
    //excluding expressions from views for now
  }
  
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
    //Nothing to do - MathML in namespaces
  }
  
  def exportDocument(doc : Document, bt: BuildTask) {
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    try {
      doc.collectModules(controller) collect {
        case _ =>
      }
    } catch {
      case e : GetError => //doc not found, can ignore 
    }   
    rh("</mws:harvest>\n")
  }
  
}

import presentation._
import metadata._

class FlatteningPresenter extends Presenter(new MathMLPresenter) {
  def key: String = "flatmws"
  override val outExt = "html"
  def isApplicable(format: String): Boolean = "flatmws" == format
  lazy val mf = new ModuleFlattener(controller)
  def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
    this._rh = rh
    val f = rh match {
      case fw : FileWriter => fw.filename 
    }
    val folder = File(f.toJava.getParentFile())
    s match { 
      case doc : Document => 
        wrapScope(standalone, doc.path)(doDocument(doc))
      case thy : DeclaredTheory => 
        val newThys = if (thy.path.toPath.contains("math")) mf.flattenFineGrained(thy) else List(thy)
        newThys foreach { t => 
          val out = (folder / t.name.toPath).setExtension("html")
          this.outputTo(out) {
            wrapScope(standalone, t.path)(doTheory(t))
          }
        }
      case view : DeclaredView =>
        wrapScope(standalone, view.path)(doView(view))
      case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
    }
    //TODO? reset this._rh 
  }  
  protected val htmlRh = utils.HTML(s => rh(s))
  import htmlRh._
  
  def doDocument(doc : Document) {
    //nothing to do
  }
  
  private def doTheory(thy : DeclaredTheory) {
    div ("theory") {
      thy.getDeclarations foreach {
      case c : Constant => 
        div ("constant") {
          div ("body") {
            text(c.name.last.toPath) 
            c.tp.foreach { o => 
              text(" : ")
              objectLevel(o, None)(rh)
            }
            c.df.foreach { o => 
              text(" = ")
              objectLevel(o, None)(rh)
            }
          }
          if (c.metadata.get(MetaDatum.keyBase ? LocalName("type")).map(_.value).contains(OMSTR("Induced"))) { //induced statement
            val origin = c.metadata.get(MetaDatum.keyBase ? "origin").head.value match {
              case t : Term => t.toMPath
            }
            val view = c.metadata.get(MetaDatum.keyBase ? "view").head.value match {
              case OMID(p : MPath) => p
            }
            val path = c.home.toMPath
            p {
              text {
                " Induced statement found in " + path.toPath + ". "
              }
              text {
                path.last + " is a " + origin.last  + " if we interpret over view " + view.last + ". "
              }
              text {
                origin.last + " contains the statement " + c.name.last + "."
              }
            }
          }
        }
      case _ => //TODO
      }
    }
  }
  
  def doView(view : View) {//nothing to do
    
  }
  
  //utils  
  def mathhubPath(p : Path) : String = {
    val uri = p.doc.uri
    //URI(uri.scheme, uri.authority, uri.path.head :: uri.path.tail.head :: "source" :: uri.path.tail.tail, uri.absolute).toString
    uri.toString
  }
  
  
  def getTitle(uri : Path) : String = uri match {
    case m : MPath => m.name.toPath
    case _ => uri.last
  }
  
  def wrapScope(standalone : Boolean, uri : Path)(content : => Unit) {
    if (standalone) {
      rh("<!DOCTYPE html>")
      html{
        head{
          rh(<meta name="mmturi" content={uri.toPath}></meta>)
          rh(<title> {getTitle(uri)} </title>)
          rh(<meta name="url" content={mathhubPath(uri)}></meta>)
        }
        body{
          div(attributes=List("xmlns" -> utils.xml.namespace("html"),
                              "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
            content
          }
        }
      }
    } else {
      div(attributes=List("xmlns" -> utils.xml.namespace("html"),
                          "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
        content
      }
    }
  }
}
