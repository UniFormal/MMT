package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.opaque._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.opaque.OpaqueHTMLPresenter
import HTMLAttributes._
import File.scala2Java
import info.kwarc.mmt.api.parser.ParseResult

import scala.Range

abstract class HTMLPresenter(val objectPresenter: ObjectPresenter) extends Presenter(objectPresenter) {
   override val outExt = "html"

   def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     this._rh = rh
     controller.simplifier(s)
     s match {
       case doc : Document =>
         doHTMLOrNot(doc.path, standalone) {doDocument(doc)}
       case ne: NarrativeElement => doHTMLOrNot(ne.path, standalone) {doNarrativeElement(ne, (x: NarrativeElement) => _)}
       case thy : Theory =>
         doHTMLOrNot(thy.path.doc, standalone) {doTheory(thy)}
       case view : View =>
         doHTMLOrNot(view.path.doc, standalone) {doView(view)}
       case dm: DerivedModule =>
         doHTMLOrNot(dm.path.doc, standalone) {doTheory(dm)}
       case d: Declaration => doHTMLOrNot(d.path.doc, standalone) {doDeclaration(d)}
     }
     this._rh = null
   }

   // easy-to-use HTML markup
   protected val htmlRh = utils.HTML(s => rh(s))
   import htmlRh._

   private def doNameAsSpanList(p: Path, n: LocalName) {
     val l = n.length - 1
     n.zipWithIndex.foreach {case (step,i) =>
       step match {
         case SimpleStep(s) =>
           span(attributes=List(href -> p.toPath)) {text(s)}
         case ComplexStep(mp) =>
           doPath(mp)
       }
       if (i < l) text("/")
     }
   }

   private def doName(d: Declaration) {
      val (name,_) = d.primaryNameAndAliases
      val (adaptedName, path) = name match {
         case LocalName(ComplexStep(t)::Nil) => (t.name, t) // hardcoding the important case of includes
         case n => (n.simplify, d.path)
      }
      span("name") {
        doNameAsSpanList(d.path, adaptedName)
      }
   }
   /** renders a MMT URI outside a math object */
   private def doPath(p: Path) {
      span("mmturi", attributes=List(href -> p.toPath)) {
         val pS = p match {
            case d: DPath => d.last
            case m: MPath => m.name.toString
            case g: GlobalName => g.name.toString
            case c: CPath => c.parent.name.toString + "?" + c.component.toString
         }
         text {pS}
      }
   }
   private def doMath(t: Obj, owner: Option[CPath]) {
        objectLevel(t match {
          case tm : Term => ParseResult.fromTerm(tm).term
          case o => o
        }, owner)(rh)
   }

   private object cssClasses {
      def compRow(c: ComponentKey) = {
         "component-" + c.toString.toLowerCase
      }
      val compLabel = "constant-component-label"
      val compToggle = "component-toggle"
   }
   import cssClasses._

   private val scriptbase = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/"
   private val cssbase    = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/css/"
   /**
    * @param dpath identifies the directory (needed for relative paths)
    * @param doit if true, wrap HTML header etc. around argument, otherwise, return arguments as a div
    */
   private def doHTMLOrNot(dpath: DPath, doit: Boolean)(b: => Unit) {
      if (! doit) {
        return div(attributes=List("xmlns" -> utils.xml.namespace("html"))) {b}
      }
      val pref = Range(0,dpath.uri.path.length+2).map(_ => "../").mkString("")
      html(attributes=List("xmlns" -> utils.xml.namespace("html"))) {
        head {
          css(cssbase+"mmt.css")
          css(cssbase+"JOBAD.css")
          css(cssbase+"jquery/jquery-ui.css")
          css(pref + "html.css")
          javascript(scriptbase + "jquery/jquery.js")
          javascript(scriptbase + "jquery/jquery-ui.js")
          javascript(scriptbase + "mmt/mmt-js-api.js")
          javascript(scriptbase + "mmt/browser.js")
          javascript(scriptbase + "jobad/deps/underscore-min.js")
          javascript(scriptbase + "jobad/JOBAD.js")
          javascript(scriptbase + "jobad/modules/hovering.js")
          javascript(scriptbase + "jobad/modules/interactive-viewing.js")
          javascript(pref + "html.js")
        }
        body {
           b
        }
      }
   }

   def doDeclaration(d: Declaration) {
      val usedby = controller.depstore.querySet(d.path, -ontology.RefersTo).toList.sortBy(_.toPath)
      val alignmentsServer: AlignmentsServer = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
        val a = new AlignmentsServer
        controller.extman.addExtension(a)
        a
      }
      val alignments = alignmentsServer.getAlignments(d.path)
      val (_,aliases) = d.primaryNameAndAliases

      val basicCss = "constant toggle-root inlineBoxSibling"
      val generatedCss = if (d.isGenerated) " generated " else ""
      div(basicCss + generatedCss, attributes = List(toggleTarget -> "generated")) {
         div("constant-header") {
           span {text({d match {
             case Include(_,_,_) => "include"
             case _ => d.feature
           }} + " ")}
           doName(d)
           def toggleComp(comp: ComponentKey) {
              toggle(compRow(comp), comp.toString.replace("-", " "))
           }
           def toggle(key: String, label: String) {
              button(compToggle, attributes = List(toggleTarget -> key)) {text(label)}
           }
           if (aliases.nonEmpty)
              toggle("aliases", "aliases")
           if (d.getDeclarations.nonEmpty) {
              toggle("inner-body", "body")
           }
           if (usedby.nonEmpty)
              toggle("used-by", "used by")
           if (d.metadata.getTags.nonEmpty)
              toggle("tags", "tags")
           if (d.metadata.getAll.nonEmpty)
              toggle("metadata", "metadata")
           d.getComponents.reverseMap {case DeclarationComponent(comp, tc) =>
              if (tc.isDefined)
                toggleComp(comp)
           }
           if (alignments.nonEmpty) {
              toggle("alignments", "alignments")
           }
         }
         table("constant-body ") {
            d.getComponents.foreach {
               case DeclarationComponent(comp, cc: AbstractObjectContainer) =>
                  tr(compRow(comp)) {
                     cc.get.foreach {t =>
                         doComponent(d.path $ comp, t)
                     }
                  }
               case DeclarationComponent(comp: NotationComponentKey, nc: NotationContainer) =>
                  tr(compRow(comp)) {
                     nc(comp).foreach {n =>
                        doNotComponent(d.path $ comp, n)
                      }
                  }
               case _ => // impossible
            }
            if (aliases.nonEmpty) {
              tr("aliases") {
                td {span(compLabel) {text{"aliases"}}}
                td {aliases foreach {a => doNameAsSpanList(d.path, a)}}
              }
            }
            if (usedby.nonEmpty) {
               tr("used-by") {
                  td {span(compLabel) {text{"used by"}}}
                  td {usedby foreach doPath}
               }
            }
            if (d.metadata.getTags.nonEmpty) {
               tr("tags") {
               td {span(compLabel){text{" ---tags"}}}
               td {d.metadata.getTags.foreach {
                  k => div("tag") {text(k.toPath)}
               }}}
            }
            def doKey(k: GlobalName) {
               td{span("key " + compLabel, title=k.toPath) {text(k.toString)}}
            }
            d.metadata.getAll.foreach {
               case metadata.Link(k,u) => tr("link metadata") {
                  doKey(k)
                  td {a(u.toString) {text(u.toString)}}
               }
               case md: metadata.MetaDatum => tr("metadatum metadata") {
                  doKey(md.key)
                  td {doMath(md.value, None)}
               }
            }
            if (alignments.nonEmpty) {
               tr("alignments", attributes = List("style" -> "display:none;")) {
                 td {span(compLabel){text{"aligned with"}}}
                 td {alignments.foreach {al =>
                    div("align") {al.to match {
                      case LogicalReference(cpath) => doPath(cpath)
                      case PhysicalReference(url) => htmlRh.a(url.toString)(text{url.toString})
                      case ConceptReference(c) => htmlRh.text(c)
                    }}//text(a.link)}
                 }}
               }
            }
            if (d.getDeclarations.nonEmpty) {
              tr {
                td(attributes = List("colspan" -> "2")) {
                   div("inner-body") {
                      d.getDeclarations foreach {b =>
                        doDeclaration(b)
                      }
                   }
                }
              }
            }
         }
      }
   }

   private def doComponent(cpath: CPath, t: Obj) {
      td {span(compLabel) {text(cpath.component.toString)}}
      td {doMath(t, Some(cpath))}
   }
   private def doNotComponent(cpath: CPath, tn: TextNotation) {
      td {span(compLabel) {text(cpath.component.toString)}}
      td {span {
         val firstVar = tn.arity.firstVarNumberIfAny
         val firstArg = tn.arity.firstArgNumberIfAny
         text {tn.markers.map {
            case SimpArg(n,_) =>
               val argNum = n-firstArg
               if (argNum < 5)
                  List("a", "b", "c", "d", "e")(argNum)
               else
                  "a" + argNum.toString
            case ImplicitArg(n,_) =>
               val argNum = n-firstArg
               if (argNum < 3)
                  List("I", "J", "K")(argNum)
               else
                  "I" + argNum.toString
            case SimpSeqArg(n, sep,_) => n.toString + sep.text + "..." + sep.text + n.toString
            case Var(n, typed, sepOpt,_) =>
               val varNum = n-firstVar
               val varname = if (varNum < 3)
                  List("x", "y", "z")(varNum)
               else
                  "x" + varNum.toString
               val typedString = if (typed) ":_" else ""
               sepOpt match {
                  case None => varname + typedString
                  case Some(sep) => varname + typedString + sep.text + "..." + sep.text + varname + typedString
               }
            case Delim(s) => s
            case SymbolName() => cpath.parent.name.toPath
            case m => m.toString
         }.mkString(" ")}
         text {" (precedence " + tn.precedence.toString + ")"}
      }}
   }

   def doTheory(m: ModuleOrLink) {
      div(m.feature) {
         doNarrativeElementInMod(m, m.asDocument)
      }
   }
   def doView(v: View) {
      doTheory(v)
   }
   override def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
      doHTMLOrNot(dpath, true) {div("namespace") {
         namespaces.foreach {case bd =>
            div("subnamespace") {
               val name = bd.dirName + "/" + bd.outFile.segments.last
               a(name) {
                  text(bd.contentDPath.toPath)
               }
            }
         }
         modules.foreach {case bf =>
            div("submodule") {
               a(bf.outFile.segments.last) {
                  text(bf.contentMPath.toPath)
               }
            }
         }
      }}
   }

   def doDocument(doc: Document) {
     div {
        doNarrativeElementInDoc(doc)
        val locOpt = controller.backend.resolveLogical(doc.path.uri)
        val svgOpt = locOpt flatMap {
          case (arch, path) =>
            val fpath = Archive.narrationSegmentsAsFile(FilePath(path), "omdoc")
            val f = (arch.root / "export" / "svg" / "narration" / fpath).setExtension("svg")
            if (f.exists)
               Some(xml.readFile(f))
            else
               None
        }
        svgOpt foreach {src =>
          div("graph toggle-root") {
             div("graph-header", attributes=List(toggleTarget -> "graph-body")) {
                text("diagram")
             }
             div("graph-body", attributes=List("style" -> "display:none")) {
                literal(src)
             }
          }
        }
     }
   }

   // ********************** narrative elements

   /** captures common parts of narrative and content element rendering */
   protected def doNarrativeElement(ne: NarrativeElement, recurse: NarrativeElement => Unit) {ne match {
      case doc: Document =>
        div("document toggle-root inlineBoxSibling") {
          div("document-header", attributes=List(toggleTarget -> "document-body")) {
             val name = doc.path.last
             NarrativeMetadata.title.get(doc) match {
               case Some(t) =>
                 text(t)
               case None =>
                 span("name") {
                   text(name)
                 }
             }
             button("generated-toggle", attributes = List(toggleTarget -> "generated")) {text("generated declarations")}
          }
          div("document-body") {
             doc.getDeclarations foreach recurse
          }
        }
      case oe: OpaqueElement =>
         val oi = controller.extman.get(classOf[OpaqueHTMLPresenter], oe.format)
                  .getOrElse(new DefaultOpaqueElementInterpreter)
         div("opaque-"+oe.format + " inlineBoxSibling") {
            oi.toHTML(objectPresenter, oe)(rh)
         }
      case ii: InterpretationInstruction =>
        div("instruction") {
          span {text(ii.feature)}
          ii.arguments.foreach {s =>
            text(s)
          }
        }
      case r: NRef =>
        val label = r match {
           case _:DRef => "dref"
           case _:MRef => "mref"
           case _:SRef => "sref"
        }
        div("document-"+label + " inlineBoxSibling") {
          span(cls = "name mmturi loadable", attributes=List(load -> r.target.toPath)) {
            val hideName = r.name.steps.forall(_==LNStep.empty) || (r match {
               case r:MRef =>
                  r.nameIsTrivial
               case _ => false
            })
            if (!hideName) {
               text(r.name.toString)
               literal(" &#8594; ")
            }
            text(r.target.toString)
          }
        }
   }}
   /** auxiliary method of doDocument */
   protected def doNarrativeElementInDoc(ne: NarrativeElement) {
      doNarrativeElement(ne, doNarrativeElementInDoc(_))
   }
   /** auxiliary method of doTheory */
   protected def doNarrativeElementInMod(body: ModuleOrLink, ne: NarrativeElement) {ne match {
      case r:SRef =>
         val d = body.get(r.target.name)
         doDeclaration(d)
      case r: NRef => throw ImplementationError("nref in module") // impossible
      case ne =>
        doNarrativeElement(ne, doNarrativeElementInMod(body, _))
   }}
}

/** like HTMLPresenter but without SVG graphs */
class MMTDocExporter extends HTMLPresenter(new MathMLPresenter) {
  val key = "mmtdoc"
  import htmlRh._

  override def doDocument(doc: Document) {
      html {
         body {
            ul {doc.getDeclarations foreach {
               case d: DRef =>
                  li("dref") {
                     a(d.target.toPath) {
                        text(d.target.last)
                     }
                  }
               case m: MRef =>
                  li("mref") {
                     a(m.target.toPath) {
                        text(m.target.last)
                     }
                  }
            }}
         }
      }
   }
}


