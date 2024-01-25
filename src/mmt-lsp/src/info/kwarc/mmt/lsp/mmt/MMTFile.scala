package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.Pragmatics
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lsp.{AnnotatedDocument, ClientWrapper, LSPDocument}
import org.eclipse.lsp4j.SymbolKind

/**
  * TODOs
  *
  * - We cannot have two annotations on the same textual range of an [[MMTFile]] that provide *complementary* information
  *   because [[AnnotatedDocument.Annotations.addReg()]] cannot cope with that.
  *   It - if I'm not mistaken (I didn't write that code) - assumes that annotations on the same textual range are
  *   redundant.
  *
  *   But we would like sometimes to have two complementary annotations on the exact same textual range.
  *   For example, when have an MMT file with code `theory T : ?SomeMeta`, we may want to have a `meta ?SomeMeta`
  *   field in VSCode's outline (which builds on an annotation on `?SomeMeta`) and *complementarily* have a second
  *   annotation on `?SomeMeta` that provides a go-to-by-definition to the source of `SomeMeta`.
  *   Of course, we could change our code such that we only produce one annotation.
  *   However, that would make some of our nice recursive algorithms non-compositional, stateful, and ugly.
  *
  */


/**
  * Language Server Protocol specific functionality for MMT documents and files.
  */
class MMTFile(uri: String, client: ClientWrapper[MMTClient], server: MMTLSPServer) extends LSPDocument(uri, client, server) with AnnotatedDocument[MMTClient, MMTLSPServer] {
  val controller: Controller = server.controller
  private lazy val docuri = this.file match {
    case Some(f) => DPath(URI(f.toJava.toURI))
    case _ => DPath(URI(uri))
  }

  override def onUpdate(changes: List[Delta]): Unit = {
    if (synchronized {
      Annotations.getAll.isEmpty
    }) {
      reparse(None)
    }
    super.onUpdate(changes)
  }

  override def onChange(annotations: List[(Delta, DocAnnotation)]): Unit = {
    print("")
  }

  //override val timercount : Int = 5

  lazy val nsMap = controller.getNamespaceMap

  lazy val errorCont = new ErrorHandler {
    private var errors: List[api.Error] = Nil

    override protected def addError(e: api.Error): Unit = {
      errors ::= e
    }

    def resetErrs = {
      errors = Nil
      this
    }

    def getErrors = errors
  }

  /**
    * Shorthand for acquiring the source region for an element `e` (which may not always be available)
    * and executing a function `f` when it exists.
    *
    * @param e An MMT element with meta data (e.g., a [[StructuralElement]] or a [[info.kwarc.mmt.api.objects.Term Term]])
    * @param f If `e` has source region meta data, then this function will be applied to it.
    * @return If the element had source region meta data, then `Some(f(source region))` is returned, otherwise
    *         [[None]].
    */
  private def forSource[A](e: HasMetaData)(f: SourceRegion => A): Option[A] =
    SourceRef.get(e).map(_.region).map(f)

  /**
    * Uses the instance-specific [[Annotations]] object of this class to add annotations (e.g., hover or
    * go-to-declaration information) to the source regions in this MMT document applicable to the given
    * structural element.
    * @param elem The structural element to dissect into subelements and inspect and to add annotations for.
    * @see [[annotateTerm]] for the [[Traverser]] to which the task is delegated on term level.
    */
  private def annotateElement(elem: StructuralElement): Unit = elem match {
    case d: Document =>
      forSource(d) { reg => Annotations.addReg(d, reg, SymbolKind.Package, d.path.toString) }
      d.getDeclarations.foreach {
        case n: Namespace =>
          forSource(n) { reg =>
            Annotations.addReg(n, reg, SymbolKind.Namespace, n.namespace.toString)
            val urlreg = SourceRegion(reg.start.copy(offset = reg.start.offset + 10, column = reg.start.column + 10), reg.end)
            val a = Annotations.addReg(n.namespace, urlreg)
            a.setHover {
              n.namespace.toString
            }
          }
        case n: NamespaceImport =>
          forSource(n) { reg =>
            Annotations.addReg(n, reg, SymbolKind.Namespace, n.prefix)
            val urlreg = SourceRegion(reg.start.copy(offset = reg.start.offset + 8, column = reg.start.column + 8), reg.end)
            val a = Annotations.addReg(n.namespace, urlreg)
            a.setHover {
              n.namespace.toString
            }
          }
        case n: FixedMeta =>
          forSource(n) { reg =>
            Annotations.addReg(n, reg, SymbolKind.Namespace, n.meta.toString)
            val urlreg = SourceRegion(reg.start.copy(offset = reg.start.offset + 8, column = reg.start.column + 8), reg.end)
            val a = Annotations.addReg(n.meta, urlreg)
            a.setHover {
              n.meta.toString
            }
          }
        case mr: MRef =>
          controller.getO(mr.target).foreach(annotateElement)
        case _ =>
          print("")
      }
    case t: Theory =>
      forSource(t) { reg => Annotations.addReg(t, reg, SymbolKind.Module, t.name.toString, true) }
      t.metaC.get.foreach(mt => {
        annotateTerm(mt, Context.empty)

        /*
        todo: For now we cannot a possibly clashing annotation to possibly the exact same range as `mt`
              because of the issue elaborated on at the beginning of this file.

        forSource(mt) { mtReg =>
          Annotations.addReg(mt, mtReg, SymbolKind.Field, s"meta ${mt.head.map(_.name.toString).getOrElse("??")}")
        }
        */
      })

      t.getPrimitiveDeclarations.foreach(annotateElement)
    case d: DerivedDeclaration =>
      forSource(d) { reg => Annotations.addReg(d, reg, SymbolKind.Struct, d.name.toString, true) }
      d.getPrimitiveDeclarations.foreach(annotateElement)
    case s: Structure =>
      val shortFromName = s.from match {
        case OMMOD(mp) => mp.name.toString
        case OMA(OMMOD(mp), _) => mp.name.toString
        case t => t.toString // default to full-blown stringification
      }
      forSource(s) { reg =>
        Annotations.addReg(
          s,
          reg,
          SymbolKind.Constant,
          if (s.isInclude) s"include $shortFromName" else s"structure $shortFromName",
          !s.isInclude
        )
      }
      /*
      todo: For now we cannot a possibly clashing annotation to possibly the exact same range as `dom`
            to have a `domain: ?from`-style thing in VSCode's outline.
            The reason is described at the beginning of this file.

      forSource(s.from) { tpReg =>
       val a = Annotations.addReg(
          s.from,
          tpReg,
          // for brevity, hide include domains in outline view since they are already reproduced by the upper
          // annotation created above for the whole structure/include
          if (s.isInclude) null else SymbolKind.Field,
          s"domain: $shortFromName"
        )
        a.setHover {
          s.from.toString
        }
      }
      */
      annotateTerm(s.from, Context.empty)

      s.getPrimitiveDeclarations.foreach(annotateElement)
    case c: Constant =>
      forSource(c) { reg => Annotations.addReg(c, reg, SymbolKind.Constant, c.name.toString, true) }

      c.getComponents.collect {
        case x if x.value.isInstanceOf[TermContainer] => (x.key, x.name, x.value.asInstanceOf[TermContainer])
      }.flatMap {
        case (key, name, tc) if tc.get.isDefined => Some((key, name, tc, tc.get.get))
        case _ => None
      }.map {
        case (key, name, tc, t) =>
          forSource(t) { reg =>
            Annotations.addReg(tc, reg, SymbolKind.Object, key.toString, true)
          }
          annotateTerm(t, Context(c.parent))
      }

    case rc: RuleConstant =>
      forSource(rc) { reg =>
        val a = Annotations.addReg(rc, reg, SymbolKind.Constant, s"rule ${rc.name.toString}")
        a.setHover {
          rc.df.map(_.getClass.getCanonicalName).getOrElse("could not determine fully qualified name of instantiated rule class")
        }
      }

    case nm: NestedModule =>
      forSource(nm) { reg => Annotations.addReg(nm, reg, SymbolKind.Module, nm.name.toString, true) }
      nm.module.getPrimitiveDeclarations.foreach(annotateElement)
    case _ =>
      print("")
  }

  private val pragmatics = controller.extman.get(classOf[Pragmatics]).headOption

  /**
    * Uses the instance-specific [[Annotations]] object of this class to add annotations (e.g., hover or
    * go-to-declaration information) to the source regions in this MMT document applicable to the given
    * [[info.kwarc.mmt.api.objects.Term Term]].
    *
    * @see [[annotateElement]] for the same task on the level of [[StructuralElement]]s
    */
  private object annotateTerm extends StatelessTraverser {

    import info.kwarc.mmt.api.objects._

    override def traverse(t: Term)(implicit con: Context, state: Unit): Term = {
      /* def infer(tm : Term) = Try(Solver.infer(controller,Context(state.th.path match {
        case mp : MPath => mp
        case gn : GlobalName => gn.module
      }) ++ con,tm,None)).toOption.flatten.map(i => "Type: " + controller.presenter.asString(i,None)).getOrElse("Unknown type") */
      def headString(tm: Term) = tm.head.map(_.toString).getOrElse("No head")

      t match {
        case o@OMV(_) =>
          forSource(o) { reg =>
            val nreg = reg.copy(end = reg.end + 1) // LSP regions have exclusive ends
            val a = Annotations.addReg(o, nreg, SymbolKind.Property, o.name.toString)
            a.setSemanticHighlightingClass(Colors.termvariable)
            a.setHover({
              headString(o)
            })
          }
          o
        case o: OMLITTrait =>
          forSource(o) { reg =>
            val nreg = reg.copy(end = reg.end + 1) // LSP regions have exclusive ends
            val a = Annotations.addReg(o, nreg)
            a.setSemanticHighlightingClass(Colors.termomlit)
            a.setHover({
              headString(o)
            })
          }
          o
        case o: OML =>
          forSource(o) { reg =>
            val nreg = reg.copy(end = reg.end + 1) // LSP regions have exclusive ends
            val a = Annotations.addReg(o, nreg, SymbolKind.Boolean, headString(o))
            a.setSemanticHighlightingClass(Colors.termoml)
            a.setHover({
              headString(o)
            })
          }
          Traverser(this, t)

        case tm@OMAorAny(dom@OMMOD(mp), args) =>
          forSource(dom) { reg =>
            val a = Annotations.addReg(dom, reg, null, mp.name.toString)
            a.setHover { mp.toString }

            // "GO TO INCLUDE/DOMAIN" functionality on OMMOD terms
            addDefinitionForSourceRef(mp, Seq(a))
          }

          args.foreach(Traverser(this, _))
          tm

        case tm if tm.head.isDefined =>
          lazy val pragma = pragmatics.map(_.mostPragmatic(tm)).getOrElse(tm)

          forSource(tm) { reg =>
            // Create annotations corresponding to the subregions of the head term not governed by
            // any subterms.
            // Example: consider the constant `proof:  prop ⟶ type❘ # ⊦ 1 prec❙` and the case where `tm` is equal to
            // `⊦ a ≐ b`, then we will create annotations that correspond to the subregions `⊦ `, ` `, and ` `,
            // i.e., taking the region of `tm` and subtracting the subregions governed by `a`, `≐`, and `b`.
            //
            // This is useful because with the go-to-definition feature implemented below users will be able to click
            // on on `≐` and go to the definition of that equality instead of being taken to (or also seeing) the
            // definition of `⊦`.
            val annotations: List[DocAnnotation] = {
              val fullRegion = reg

              val subRegions = pragma.subobjects.map(_._2).flatMap(SourceRef.get)
              val headRegions = fullRegion.subtractChildRegions(subRegions.map(_.region))

              headRegions
                .map(reg => reg.copy(end = reg.end + 1)) // LSP regions have exclusive ends
                .map(Annotations.addReg(tm, _, null, tm.head.map(_.name.toString).orNull, false))
            }
            annotations.foreach(_.setHover({
              headString(pragma)
            }))

            val headFromMeta = pragma.head.exists(p => {
              con.getIncludes.exists(mp => controller.library.hasImplicit(p.module, mp))
            })
            if (headFromMeta) {
              annotations.foreach(_.setSemanticHighlightingClass(Colors.termconstantmeta))
            }

            // "GO TO DEFINITION" functionality on terms
            pragma.head.foreach(addDefinitionForSourceRef(_, annotations))
          }

          Traverser(this, t)
          tm match {
            case OMS(_) => tm
            case OMA(_, args) =>
              args.foreach(traverse(_))
              tm
            case OMBINDC(_, ctx, bd) =>
              traverseContext(ctx)
              bd.foreach(traverse(_))
              tm
          }
        case _ =>
          Traverser(this, t)
      }
    }

    override def traverseContext(cont: Context)(implicit con: Context, state: Unit): Context = {
      cont.foreach { vd =>
        forSource(vd) { reg =>
          val nreg = reg.copy(end = reg.start.copy(offset = reg.start.offset + vd.name.length, column = reg.start.column + vd.name.length))
          val a = Annotations.addReg(vd, nreg, SymbolKind.Property, vd.name.toString, true)
          // todo: add parent annotation for tp and df component
          vd.tp.foreach(t => Traverser(this, t))
          vd.df.foreach(t => Traverser(this, t))
          a.setSemanticHighlightingClass(Colors.termvariable)
        }
      }
      super.traverseContext(cont)
    }
  }

  /**
    * Given some [[DocAnnotation annotations]], add go-to-definition functionality to them resolving to the textual
    * range in the file, wherein the MMT element referred to by `p` was declared (if any such file or metadata
    * information exists).
    *
    * For example, if you already have constructed an annotation and you would want it to resolve via go-to-definition
    * to where the theory `http://cds.omdoc.org/mmt?TermsTypesKinds` is declared, call this method with this very
    * path on your annotation.
    * Your annotation will then get added a definition resolving (as of January 2024's state of urtheories) to
    * a certain textual range within the file `lf.mmt` in the urtheories archive.
    */
  private def addDefinitionForSourceRef(p: Path, annotations: Seq[DocAnnotation]): Unit = {
    controller.getO(p).flatMap(SourceRef.get).foreach((src: SourceRef) => {
      server.resolveSourceFilepath(src).foreach(originFile => {
        val originRegion = src.region

        annotations.foreach(_.addDefinitionLC(
          originFile.toURI.getPath,
          (originRegion.start.line, originRegion.start.column),
          (originRegion.end.line, originRegion.end.column)
        ))
      })
    })
  }

  /**
    * (Re)parse the whole MMT document and (re)set all LSP annotations and warnings.
    */
  def reparse(progressCont: Option[MMTTaskProgressListener]): Unit = {
    synchronized {
      errorCont.resetErrs
      val d = try {
        server.parser.apply(errorCont, progressCont, docuri, doctext, nsMap)
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          ???
      }
      annotateElement(d)
    }

    client.documentErrors(this, true, errorCont.getErrors: _*)
    errorCont.reset
  }

}

class IterativeOParser extends NotationBasedParser {}

class IterativeParser(objectParser: IterativeOParser = new IterativeOParser) extends KeywordBasedParser(objectParser) {
  override def start(args: List[String]): Unit = {
    super.start(args)
    controller.extman.addExtension(objectParser)
  }

  def apply(errorCont: ErrorHandler, progressCont: Option[MMTTaskProgressListener], dpath: DPath, docstring: String, nsmap: NamespaceMap = controller.getNamespaceMap.copy()): StructuralElement = {
    val ps = ParsingStream.fromString(docstring, dpath, "mmt", Some(nsmap))
    progressCont foreach ps.addListener

    implicit val spc = new StructureParserContinuations(errorCont) {
      override def onElement(se: StructuralElement): Unit = se match {
        case _: Document | _: Namespace | _: NamespaceImport | _: MRef | _: Theory =>
        case c: Constant =>
          val sr = SourceRef.get(c).get
          val string = docstring.slice(sr.region.start.offset, sr.region.end.offset)
          print("")
        case _ =>
          print("")
      }

      override def onElementEnd(se: ContainerElement[_]): Unit = {
        print("")
      }
    }
    apply(ps)
  }
}