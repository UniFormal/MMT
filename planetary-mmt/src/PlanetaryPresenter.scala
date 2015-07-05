package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._
import presentation._
import frontend._
import objects._
import utils._
import informal._
import archives._
import notations._
import utils.xml._


class NotationPresenter(contr : Controller, var notations : List[(GlobalName,TextNotation)] = Nil) 
  extends presentation.MathMLPresenter {
  controller = contr
  report = controller.report
  override def getNotation(path: GlobalName): Option[TextNotation] = {
    notations.find(p => p._1 == path) match {
      case None => super.getNotation(path)
      case Some(p) => Some(p._2)
    }
  }
  //this overrides variable presentation to placeholders for notation rendering (currently changing color to red)
  override def doVariable(n: LocalName)(implicit pc : PresentationContext) {
      val vdAtt = pc.context.find(_.decl.name == n) match {
         case None => Nil
         case Some(vd) => List(HTMLAttributes.varref -> vd.declpos.toString)
      }
      val mi = xml.element("mi", ("style" -> "color:red;") :: vdAtt ::: jobadattribs, n.toString)
      pc.out(mi)
   }
}

class InformalMathMLPresenter extends presentation.MathMLPresenter {
   override def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
     implicit val pc = PresentationContext(rh, origin, Nil, None, Position.Init, Nil, None)
     doInfToplevel(o) {
        recurse(o)
     }
   }  
   
   override def doAttributedTerm(t : Term, k : OMID, v : Term)(pc : PresentationContext) = k.path match {
    case Narration.path => 
      doInformal(v,t)(pc : PresentationContext)
      1
    case _ => doDefault(t)(pc)
  }
  
  def doInformal(t : Term, tm : Term)(implicit pc : PresentationContext) : Unit = t match {
    case OMFOREIGN(n) => doInformal(n, tm)(pc)
    case _ => doInfToplevel(t) {
      recurse(t)(pc)
    }
  }
  
  def doInfToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext) = o match {
    case OMATTR(t, k, v) => //nothing to do
      
      val attrs = t.head.map(p => HTMLAttributes.symref -> p.toPath).toList 
      pc.out(openTag("span", attrs))
      body
      pc.out(closeTag("span"))
    case _ => 
      val nsAtts = List("xmlns" -> namespace("mathml"))
      val mmtAtts = pc.owner match {
         case None => Nil
         case Some(cp) => List(HTMLAttributes.owner -> cp.parent.toPath, HTMLAttributes.component -> cp.component.toString, HTMLAttributes.position -> "")
      }
      val idAtt = ( "id" -> o.hashCode.toString)
      // <mstyle displaystyle="true">
      pc.out(openTag("math",  idAtt :: nsAtts ::: mmtAtts))
      pc.out(openTag("semantics", Nil))
      pc.out(openTag("mrow", Nil))
      body
      pc.out(closeTag("mrow"))
      pc.out(openTag("annotation-xml", List("encoding" -> "MathML-Content")))
      pc.out(o.toCML.toString)
      pc.out(closeTag("annotation-xml"))
      pc.out(closeTag("semantics"))
      pc.out(closeTag("math"))
  }
   
  override def doDelimiter(p: GlobalName, d: Delimiter, implicits: List[Cont])(implicit pc : PresentationContext) = d.text match {
    case "&#40;" => super.doDelimiter(p, Delim("("), implicits)
    case "&#41;" => super.doDelimiter(p, Delim(")"), implicits)
    //unescaping things already escaped by latexml
    case "&gt;" => super.doDelimiter(p, Delim(">"), implicits)
    case "&lt;" => super.doDelimiter(p, Delim("<"), implicits)
    case _ => super.doDelimiter(p, d, implicits)
  }
  
  def doInformal(n : scala.xml.Node, tm : Term)(implicit pc : PresentationContext) : Unit = n match {
    case _ if (n.label == "immtref") =>
      val pos = Position.parse(xml.attr(n,"pos"))
      val inPos = Position(pos.indices.tail)
      val term = tm.subobject(inPos)._2
      doInfToplevel(term) {
        recurse(term)(pc)
      }
    case s : scala.xml.SpecialNode => pc.out(s.toString)
    case _ => 
      pc.rh.writeStartTag(n.prefix, n.label, n.attributes, n.scope)
      n.child.map(c => doInformal(c, tm)(pc))
      pc.rh.writeEndTag(n.prefix, n.label)
  }
}

abstract class PlanetaryAbstractPresenter(name : String) extends Presenter(new InformalMathMLPresenter) {
  override val outExt = "html"
  val key = name
  
  protected lazy val notPres = new NotationPresenter(controller)
  
  // easy-to-use HTML markup
  protected val htmlRh = utils.HTML(s => rh(s))
   
  def mathhubPath(p : Path) : String = {
    val uri = p.doc.uri
    URI(uri.scheme, uri.authority, uri.path.head :: uri.path.tail.head :: "source" :: uri.path.tail.tail, uri.absolute).toString
  }
  
  import htmlRh._
  def wrapScope(standalone : Boolean, uri : Path)(content : => Unit) {
    if (standalone) {
      rh("<!DOCTYPE html>")
      html{
        head{
          rh(<meta name="mmturi" content={uri.toPath}></meta>)
          rh(<title> {uri.last} </title>)
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

class PlanetaryPresenter extends PlanetaryAbstractPresenter("planetary") {
   
   def setRh(rh : RenderingHandler) = this._rh = rh 
  
   def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     this._rh = rh
     s match { 
       case doc : Document => 
         wrapScope(standalone, doc.path)(doDocument(doc))
       case thy : DeclaredTheory => 
         wrapScope(standalone, thy.path)(doTheory(thy))
       case view : DeclaredView =>
         wrapScope(standalone, view.path)(doView(view))
       case c : Constant =>
         wrapScope(standalone, c.path)(doConstant(c))
       case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
     }
     //TODO? reset this._rh 
   }   
   
   import htmlRh._
   
   private def doName(p: Path) {
      val s = p.last
      var attrs = p match {
        case g : GlobalName => List("id" -> (g.module.toMPath.name.toPath + "?" + g.name.toPath))
        case m : MPath => List("id" -> m.name.toPath)
        case _ => Nil
      }
      attrs ::= (HTMLAttributes.symref -> p.toPath)
      span(cls="name", attributes=attrs) {text(s)}
   }
   
    def doName(path : Path, loadable : Boolean) : Unit = loadable match {
     case true =>
       span(cls = "name") {
         text(path.last)
       }
     case false => doName(path)
   }
   
   private def doMath(t: Obj) {
        apply(t, None)(rh)
   }
   private def doComponent(comp: DeclarationComponent, t: Obj) {
      td {span {text(comp.toString)}}
      td {doMath(t)}
   }
   private def doNotComponent(comp: NotationComponent, tn: TextNotation) {
      td {span {text(comp.toString)}}
      td {span {text(tn.toText)}}
   }
   private val scriptbase = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/"
   private val cssbase    = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/css/"
   
   def doTheory(t: DeclaredTheory) {
     div("theory") {
       div("theory-header") {doName(t.path)}
       t.getPrimitiveDeclarations.foreach {
         case PlainInclude(from,to) => 
           span("include") {
             table {
               tr {
                 td { span("keyword") {text("includes")} }
                 td {
                   span(attributes = List(HTMLAttributes.symref -> from.toPath)) {
                     text(from.name.toPath)
                   }
                 }
               }
             }
           } 
         case c : Constant =>
           doConstant(c)
//         case f : FlexiformalDeclaration =>  
//           doFlexiformalDeclaration(f)
       }
     }
   }
   
   def doConstant(c : Constant) {
     div(cls = "constant") {
       if (!c.name.toPath.contains('.')) {//TODO hack to check if informal constant
         span("keyword"){text("constant")}
         doName(c.path)
         doNotations(c.notC.getAllNotations.map(c.path -> _), c.path)
       }
       c.df foreach {df => 
         doMath(df)
       }
     }
   }
   
   def encPath(path : GlobalName) : String = {
     path.module.toMPath.name.toString + "_" + path.name.toString
   }
   
   def doNotations(notations : List[(GlobalName, TextNotation)], path : GlobalName, instId : String = "") {
     if (!notations.isEmpty) {
       val onclick = "onclick=\"if (jQuery(this).html() == \'Show Notations\') {jQuery(this).html(\'Hide Notations\')} else {jQuery(this).html(\'Show Notations\')};" +
                     "jQuery(document.getElementById(\'not_" + encPath(path) + "_" + instId + "\')).toggle( \'fold\' );\""
       rh(" <small><a style=\"cursor:pointer;\" " + onclick + ">" + "Show Notations" + "</a></small>")
       table(cls = "table table-striped table-condensed", attributes = ("id" -> ("not_" + encPath(path) + "_" + instId)) :: ("style" -> "display:none;") :: Nil) {
         head {
           tr {
             th { text{"Languages"} } 
             th { text{"Arguments"} }
             th { text{"Rendering"} }
           }
         }
         body {
           notations.foreach(p => doNotation(p._1, p._2))
         }
       }
     }
   }
   
   def doNotationTerm(spath : GlobalName, not : TextNotation) : Term = {
     def getVarName(i : Int) : String = {
       val name = ((i % 26) + 'a'.toInt).toChar
       val indices =  (i / 26) match {
         case 0 => ""
         case n => n.toString
       }
       name.toString + indices
     }
     val arity = not.arity
     val sub = Substitution(arity.subargs.map(sa => Sub(OMV.anonymous, OMV(getVarName(sa.number - 1)))) :_*)
     val con = Context(arity.variables.map(v => VarDecl(LocalName(getVarName(v.number - 1)), None, None, None)) :_*)
     val args = arity.arguments flatMap {
       case s : SeqArg => 
         val baseName = getVarName(s.number - 1)
         val first = OMV(baseName + "1")
         val middle = OMSTR("…")
         val last = OMV(baseName + "n")
         List(first, middle, last)
       case a => List(OMV(getVarName(a.number - 1)))
     }
     ComplexTerm(spath, sub, con, args)
   }
   
   def doNotationRendering(spath : GlobalName, not: TextNotation) {
     val tm = doNotationTerm(spath, not)
     notPres.notations = List(spath -> not)
     notPres(tm, None)(rh)
     notPres.notations = Nil
   }
   
   def doNotation(spath : GlobalName, not : TextNotation)  = {
     tr {
       td { text{not.scope.languages.mkString("/")} }
       td { text{not.arity.length.toString} }
       td {
         doNotationRendering(spath, not)
       }
     }
   }
   
   /*
   def doFlexiformalDeclaration(fd : FlexiformalDeclaration) : Unit = fd match {
     case n : PlainNarration => 
       div("flexiformal plain") {
         doNarrativeObject(fd.df)
       }
     case d : Definition => 
       div(cls = "flexiformal definition", 
           attributes = List(("jobad:defines" -> d.targets.head.toPath))) {
         //span("keyword"){text("definition" )}
         doNarrativeObject(fd.df)
       }
     case d : Example => 
       div(cls = "flexiformal example", 
           attributes = List(("jobad:example" -> d.targets.head.toPath))) {
         //span("keyword"){text("definition" )}
         doNarrativeObject(fd.df)
       }
     case ex : Exercise => 
       div(cls = "flexiformal exercise") {
         //span("keyword"){text("definition" )}
         doNarrativeObject(ex.prob)
         ex.solution.map { sol => 
           rh("<p>Solution : </p>")
           div(cls = "flexiformal solution", id = ex.path.toPath) {
             doNarrativeObject(sol)
           }
         }
       }
     case x => 
       throw ImplementationError("Presentation for " + x.getClass() + " not implemented yet")
   }
   
   def doNarrativeObject(no : FlexiformalObject) : Unit = no match {
     case n : FlexiformalXML => rh(n.node)
     case r : FlexiformalRef => r.self match {
      case false => 
        rh("<span jobad:href=\"" + r.target.toPath + "\">")
        r.objects.foreach(doNarrativeObject)
        rh("</span>")
      case true => 
        rh("<span class=\"definiendum\" jobad:href=\"" + r.target.toPath + "\">")
        r.objects.foreach(doNarrativeObject)
        rh("</span>")
     }
     case tm : FlexiformalTerm =>
       apply(tm.term, None)(rh)
     case n : FlexiformalNode => 
       rh.writeStartTag(n.node.prefix, n.node.label, n.node.attributes, n.node.scope)
       n.child.map(doNarrativeObject)
       rh.writeEndTag(n.node.prefix, n.node.label)
   }
   */
   
   def doView(v: DeclaredView) {}
   override def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
     div("namespace") {
        namespaces.foreach {bt =>
            div("subnamespace") {
               val name = bt.dirName + "/" + bt.outFile.segments.last
               a(name) {
                  text(bt.contentDPath.toPath)
               }
            }
         }
         modules.foreach {bt =>
            div("submodule") {
               a(bt.outFile.segments.last) {
                  text(bt.contentMPath.toPath)
               }
            }
         }
      }
   }
   
   
   def doRef(r : XRef) = r match {
     case d: DRef if d.target.last == OMV.anonymous => //nested doc 
       rh("<div class=\"group\">")
         controller.get(d.target) match {
           case d : Document => doDocument(d)
         }
       rh("</div>")
     case d: DRef => 
       li("dref") {
         span(attributes = List(HTMLAttributes.symref -> d.target.toPath, "data-relative" -> "true")) {
           text(d.target.last)
         }
       }
     case m : MRef =>
       controller.get(m.target) match {
         case t : DeclaredTheory => 
           if (t.name != OMV.anonymous) {
	         li("mref") {
		      doTheory(t)
	         }
           }
         case v : DeclaredView => 
           li("mref") {
	         doView(v)
           }
       }
     case s => throw ImplementationError("Presenting for " + s.getClass() + " not implemented yet ")
   }
   
   def doDocument(doc: Document) {
     div("document") {
       ul("doc-body") { doc.getDeclarations foreach {
         case x : XRef => doRef(x)
         case s => throw ImplementationError("Presenting for " + s.getClass() + " not implemented yet ")
       }}
     }
   }
}

