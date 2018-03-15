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

class NotationPresenter(contr : Controller, var notations : List[(GlobalName,TextNotation)] = Nil)
  extends presentation.MathMLPresenter {
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
      val mi = xml.element("mi", ("style" -> "color:red;") :: vdAtt ::: jobadattribs, n.toString)
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

class InformalMathMLPresenter extends presentation.MathMLPresenter {
   def doTermApplication(f : Term, args : List[Term])(implicit pc: PresentationContext) : Int = {
     doDefault(f)
     doBracketedGroup {
       val comps : List[() => Unit] = args.zipWithIndex.map {case (t,i) =>
         () => {
           recurse(t)(pc.child(i))
           ()
         }
       }
       doListWithSeparator(comps, () => pc.out(", "))
     }
     1
   }

   override def doDefault(o: Obj)(implicit pc: PresentationContext): Int = o match {
     case ComplexTerm(op, subs, con, args) =>
       if (subs.isEmpty && con.isEmpty)
         doTermApplication(OMS(op), args)
       else super.doDefault(o)
     case OMA(f, args) => doTermApplication(f, args)
     case _ => super.doDefault(o)
   }

  override def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
     implicit val pc = preparePresentation(o, origin)
     doInfToplevel(o) {
        recurse(o)
     }
   }

   override def doAttributedTerm(t : Term, k : OMID, v : Term)(implicit pc : PresentationContext) = k.path match {
    case Narration.path | MathMLNarration.path =>
      doInformal(v,t)
      1
    case _ => doDefault(t)
  }

  def doInformal(t : Term, tm : Term)(implicit pc : PresentationContext) : Unit = t match {
    case OMFOREIGN(n) => doInformal(n, tm)(pc)
    case _ => doInfToplevel(t) {
      recurse(t)(pc)
    }
  }

  // TODO this should override doToplevel and apply should go
  def doInfToplevel(o: Obj)(body: => Unit)(implicit pc: PresentationContext) = o match {
    case OMATTR(t, k, v) if k.path != MathMLNarration.path => //normal html
      val attrs = t.head.map(p => HTMLAttributes.symref -> p.toPath).toList
      pc.out(openTag("span", attrs))
      body
      pc.out(closeTag("span"))
    case _ => //wrapping as mathml
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


  //Namespaces whose nodes and attributes we ignore
  private val ignoredNamespaces = List("http://omdoc.org/ns",  //omdoc
                                   "http://kwarc.info/ns/sTeX",  //stex
                                   "http://purl.org/dc/elements/1.1/", //dc
                                   "http://www.openmath.org/OpenMath" //openmath
                                   )
  //Namespaces for whose nodes and attributes we remove the prefix because they are in HTML5 (e.g. mathml & svg)
  private val coveredNamespaces = List("http://www.w3.org/1998/Math/MathML" //mathml
                                   )

  def cleanAttribs(attrs : scala.xml.MetaData, scope : scala.xml.NamespaceBinding) : scala.xml.MetaData = {
    var newAttr : scala.xml.MetaData = attrs
    def traverse(att : scala.xml.MetaData) : Unit = att match {
      case scala.xml.Null => scala.xml.Null //nothing to do
      case p : scala.xml.PrefixedAttribute =>
        val uri = scope.getURI(p.pre)
        if (ignoredNamespaces.contains(uri) || coveredNamespaces.contains(uri)) { //removing prefix attr
          newAttr = newAttr.remove(uri, scope, p.key)
        }
        if (coveredNamespaces.contains(uri)) { //adding an unprefixed attr back
          newAttr = new scala.xml.UnprefixedAttribute(p.key, p.value, newAttr)
        }
        traverse(att.next)
      case u : scala.xml.UnprefixedAttribute =>
        if (u.key == "about" || u.key == "id")
          newAttr = newAttr.remove(u.key)
        traverse(att.next)
    }
    traverse(attrs)
    newAttr
  }

  private def cleanScope(scope : scala.xml.NamespaceBinding) : scala.xml.NamespaceBinding = {
    if (scope == scala.xml.TopScope) {
      scope
    } else {
        if (ignoredNamespaces.contains(scope.uri) || coveredNamespaces.contains(scope.uri) || scope.prefix == null) {
          cleanScope(scope.parent)
        } else {
          scala.xml.NamespaceBinding(scope.prefix, scope.uri, cleanScope(scope.parent))
        }
    }
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
      val prefixURI = n.scope.getURI(n.prefix)
      if (n.prefix == null || !ignoredNamespaces.contains(prefixURI)) {
        //removing prefix for html5-convered namespaces (e.g. mathml, svg)
        val prefix = if (coveredNamespaces.contains(prefixURI)) null else n.prefix
        val scope = cleanScope(n.scope)
        val attribs = cleanAttribs(n.attributes, n.scope)
        pc.rh.writeStartTag(prefix, n.label, attribs, scope)
        n.child.map(c => doInformal(c, tm)(pc))
        pc.rh.writeEndTag(prefix, n.label)
      } else {
        //TODO report warning
        //nothing to do
      }
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
         case thy : DeclaredTheory =>
           wrapScope(standalone, thy.path)(doTheory(thy))
         case view : DeclaredView =>
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

   def doTheory(t: DeclaredTheory) {
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

