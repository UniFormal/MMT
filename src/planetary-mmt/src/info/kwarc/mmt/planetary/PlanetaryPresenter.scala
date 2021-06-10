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
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.stex.InformalMathMLPresenter

class NotationPresenter(contr : Controller, var notations : List[(GlobalName,TextNotation)] = Nil)
  extends presentation.PresentationMathMLPresenter {
  controller = contr
  report = controller.report
  override def getNotations(path: GlobalName): List[TextNotation] = {
    notations.find(p => p._1 == path) match {
      case None => super.getNotations(path)
      case Some(p) => List(p._2)
    }
  }
  //this overrides variable presentation to placeholders for notation rendering (currently changing color to red)
  override def doVariable(n: LocalName)(implicit pc : PresentationContext) {
      val vdAtt = pc.context.find(_.decl.name == n) match {
         case None => Nil
         case Some(vd) => List(HTMLAttributes.varref -> vd.declpos.toString)
      }
      val mi = xml.element("mi", ("style" -> "color:red;") :: vdAtt ::: mathmlattribs, n.toString)
      pc.out(mi)
   }
  //TODO duplicate code found also in informal presenter, to fix
  override def doDelimiter(p: GlobalName, d: Delimiter, implicits: List[Cont])(implicit pc : PresentationContext) = d.text match {
    case "&#40;" => super.doDelimiter(p, Delim("("), implicits)
    case "&#41;" => super.doDelimiter(p, Delim(")"), implicits)
    //unescaping things already escaped by latexml
    case "&gt;" => super.doDelimiter(p, Delim(">"), implicits)
    case "&lt;" => super.doDelimiter(p, Delim("<"), implicits)
    case _ => super.doDelimiter(p, d, implicits)
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
    //URI(uri.scheme, uri.authority, uri.path.head :: uri.path.tail.head :: "source" :: uri.path.tail.tail, uri.absolute).toString
    uri.toString
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
          div(attributes=List("xmlns" -> utils.xml.namespace("html"))) {
            content
          }
        }
      }
    } else {
      div(attributes=List("xmlns" -> utils.xml.namespace("html"))) {
        content
      }
    }
  }
}

class PlanetaryPresenter extends PlanetaryAbstractPresenter("planetary") {

   def setRh(rh : RenderingHandler) = this._rh = rh
   override val logPrefix = "PlanetaryPresenter"

   def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     try {
       this._rh = rh
       s match {
         case doc : Document =>
           wrapScope(standalone, doc.path)(doDocument(doc))
         case thy: Theory =>
           wrapScope(standalone, thy.path)(doTheory(thy))
         case view: View =>
           wrapScope(standalone, view.path)(doView(view))
         case c : Constant =>
           wrapScope(standalone, c.path)(doConstant(c))
         case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
       }
     } catch {
       case e : Exception => log(s"Failed Presenting ${s.path} due to unexpected error : ${e.getMessage} \n  ${e.getStackTrace.mkString("\n")}")
     }
     //TODO? reset this._rh
   }

   import htmlRh._

   protected def doName(p: ContentPath) {
      val s = p.name.toPath
      var attrs = p match {
        case g : GlobalName => List("id" -> (g.module.name.toPath + "?" + g.name.toPath))
        case m : MPath => List("id" -> m.name.toPath)
        case _ => Nil
      }
      attrs ::= (HTMLAttributes.symref -> p.toPath)
      span(cls="name", attributes=attrs) {text(s)}
   }

    def doName(path : ContentPath, loadable : Boolean) : Unit = loadable match {
     case true =>
       span(cls = "name") {
         text(path.last)
       }
     case false => doName(path)
   }

   protected def doMath(t: Obj) {
     apply(t, None)(rh)
   }
   private def doComponent(comp: DeclarationComponent, t: Obj) {
      td {span {text(comp.toString)}}
      td {doMath(t)}
   }
   private def doNotComponent(comp: NotationComponentKey, tn: TextNotation) {
      td {span {text(comp.toString)}}
      td {span {text(tn.toText)}}
   }
   private val scriptbase = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/"
   private val cssbase    = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/css/"

   def doTheory(t: Theory) {
     div("theory") {
       div("theory-header") {doName(t.path)}
       t.getPrimitiveDeclarations.foreach {
         case PlainInclude(from,to) =>
           div(cls = "include") {
             span("keyword") {text("includes")}
             text(" ")
             span(attributes = List(HTMLAttributes.symref -> from.toPath)) {
               text(from.name.toPath)
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
         text(" ")
         doName(c.path)
         doNotations(c.notC.getAllNotations.map(c.path -> _), c.path)
       }
       c.df foreach {df =>
         doMath(df)
       }
     }
   }

   def encPath(path : GlobalName) : String = {
     path.module.name.toString + "_" + path.name.toString
   }


   def doShowHideTrigger(name : String, id : String) {
       val onName = name
       rh(" <small><a class='gls_trigger' data-target='" + id + "'>" + onName + "</a></small>")
   }

   def doNotationsTable(notations : List[(GlobalName, TextNotation)], id : String) {
     if (!notations.isEmpty) {
       table(cls = "table table-striped table-condensed", attributes = ("id" -> (id)) :: ("style" -> "display:none;") :: Nil) {
         thead {
           tr {
             th { text{"Languages"} }
             th { text{"Arguments"} }
             th { text{"Rendering"} }
           }
         }
         tbody {
           notations.foreach(p => doNotation(p._1, p._2))
         }
       }
     }
   }

   def doHypernymsList(hypernyms : List[List[TextNotation]], id : String, lang : String) {
     if (!hypernyms.isEmpty) {
       div(attributes = ("id" -> (id)) :: ("style" -> "display:none;") :: Nil) {
         h5 {
           text{"Hypernyms:"}
         }
         ul {
           hypernyms.foreach { h => li {
               text{h.map(v => v.toText).mkString(", ")}
             }
           }
         }
       }
     }
   }

   def doHyponymsList(hyponyms : List[List[TextNotation]], id : String, lang : String) {
     if (!hyponyms.isEmpty) {
       div(attributes = ("id" -> (id)) :: ("style" -> "display:none;") :: Nil) {
         h5 {
           text{"Hyponyms:"}
         }
         ul {
           hyponyms.foreach { h => li {
               text{h.map(v => v.toText).mkString(", ")}
             }
           }
         }
       }
     }
   }

   def doSynonymsList(synonyms : List[TextNotation], id : String, lang : String) {
     if (!synonyms.isEmpty) {
       div(attributes = ("id" -> (id)) :: ("style" -> "display:none;") :: Nil) {
         h5 {
           text{"Synonyms:"}
         }
         ul {
           synonyms.foreach { h => li {
               text{h.toText}
             }
           }
         }
       }
     }
   }

   def doNotations(notations : List[(GlobalName, TextNotation)], path : GlobalName, instId : String = "") {
     if (!notations.isEmpty) {
       val onclick = "onclick=\"if (jQuery(this).html() == \'Show Notations\') {jQuery(this).html(\'Hide Notations\')} else {jQuery(this).html(\'Show Notations\')};" +
                     "jQuery(document.getElementById(\'not_" + encPath(path) + "_" + instId + "\')).toggle( \'fold\' );\""
       rh(" <small><a style=\"cursor:pointer;\" " + onclick + ">" + "Show Notations" + "</a></small>")
       table(cls = "table table-striped table-condensed", attributes = ("id" -> ("not_" + encPath(path) + "_" + instId)) :: ("style" -> "display:none;") :: Nil) {
         thead {
           tr {
             th { text{"Languages"} }
             th { text{"Arguments"} }
             th { text{"Rendering"} }
           }
         }
         tbody {
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
     val con = Context(arity.variables.map(v => VarDecl(LocalName(getVarName(v.number - 1)))) :_*)
     val args = arity.arguments flatMap {
       case s : SeqArg =>
         val baseName = getVarName(s.number - 1)
         val first = OMV(baseName + "1")
         val middle = uom.OMLiteral.OMSTR("…")
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

   def doView(v: View) {}
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

   def doRef(r : NRef) = r match {
     case d: DRef if d.target.name == OMV.anonymous => //nested doc
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
         case t: Theory =>
           if (t.name != OMV.anonymous) {
            li("mref") {
            doTheory(t)
            }
           }
         case v: View =>
           li("mref") {
            doView(v)
           }
       }
     case s =>
       throw ImplementationError("Presenting for " + s.getClass() + " not implemented yet ")
   }

   def doDocument(doc: Document) {
     div("document") {
       ul("doc-body") { doc.getDeclarations foreach {
         case x : NRef => doRef(x)
         case d : Document => doDocument(d)
         case op : OpaqueElement => // div { out(op.raw.toString)}
         case s =>
           throw ImplementationError("Presenting for " + s.getClass() + " not implemented yet ")
       }}
     }
   }
}

