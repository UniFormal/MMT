package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.Pragmatics
import info.kwarc.mmt.api.objects.{OMA, OMMOD, Traverser}
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api._
import info.kwarc.mmt.lsp.{AnnotatedDocument, ClientWrapper, LSPDocument}
import org.eclipse.lsp4j.SymbolKind

class MMTFile(uri : String,client:ClientWrapper[MMTClient],server:MMTLSPServer) extends LSPDocument(uri, client, server) with AnnotatedDocument[MMTClient,MMTLSPServer] {
  val controller = server.controller
  private lazy val docuri = this.file match {
    case Some(f) => DPath(URI(f.toJava.toURI))
    case _ => DPath(URI(uri))
  }

  override def onUpdate(changes: List[Delta]): Unit = {
    if (synchronized{Annotations.getAll.isEmpty}) parseTop
    super.onUpdate(changes)
  }

  override def onChange(annotations: List[(Delta, DocAnnotation)]): Unit = {
    print("")
  }

  //override val timercount : Int = 5

  lazy val nsMap = controller.getNamespaceMap

  lazy val errorCont = new ErrorHandler {
    private var errors : List[api.Error] = Nil
    override protected def addError(e: api.Error): Unit = {
      errors ::= e
    }
    def resetErrs = {
      errors = Nil
      this
    }
    def getErrors = errors
  }

  private def forSource[A](e : HasMetaData)(f : (Int,Int) => A) : Option[A] = SourceRef.get(e).map { src =>
    val start = src.region.start.offset
    val length = src.region.end.offset - start
    f(start,length)
  }

  private def getElems(top : StructuralElement) : Unit = top match {
    case d : Document =>
      forSource(d){(start,length) => Annotations.add(d,start,length,SymbolKind.Package,d.path.toString) }
      d.getDeclarations.foreach {
      case n : Namespace =>
        forSource(n){(start,length) =>
          Annotations.add(n,start,length,SymbolKind.Namespace,n.namespace.toString)
          val urlstart = start + 10
          val urllength = length - 10
          val a = Annotations.add(n.namespace,urlstart,urllength)
          a.setHover{ n.namespace.toString }
        }
      case n : NamespaceImport =>
        forSource(n){(start,length) =>
          Annotations.add(n,start,length,SymbolKind.Namespace,n.prefix)
          val urlstart = start + 8 + n.prefix.length
          val urllength = length - (8 + n.prefix.length)
          val a = Annotations.add(n.namespace,urlstart,urllength)
          a.setHover{ n.namespace.toString }
        }
      case n : FixedMeta =>
        forSource(n){(start,length) =>
          Annotations.add(n,start,length,SymbolKind.Namespace,n.meta.toString)
          val urlstart = start + 8
          val urllength = length - 8
          val a = Annotations.add(n.meta,urlstart,urllength)
          a.setHover{ n.meta.toString }
        }
      case mr : MRef =>
        controller.getO(mr.target).foreach(getElems)
      case _ =>
        print("")
    }
    case t : Theory =>
      forSource(t){(start,length) => Annotations.add(t,start,length,SymbolKind.Module,t.name.toString,true)}
      t.getPrimitiveDeclarations.foreach(getElems)
    case d : DerivedDeclaration =>
      forSource(d){(start,length) => Annotations.add(d,start,length,SymbolKind.Struct,d.name.toString,true)}
      d.getPrimitiveDeclarations.foreach(getElems)
    case s : Structure =>
      forSource(s){(start,length) => Annotations.add(s,start,length,SymbolKind.Constant,s.from match {
        case OMMOD(mp) => mp.toString
        case OMA(OMMOD(mp), _) => mp.toString
        case _ =>
          print("")
          ???
      },!s.isInclude)}
      s.getPrimitiveDeclarations.foreach(getElems)
    case c : Constant =>
      forSource(c){(start,length) => Annotations.add(c,start,length,SymbolKind.Constant,c.name.toString,true)}
      val hl = new HighlightList(c)
      c.tp.foreach(termHighlighter(_,hl))
      c.df.foreach(termHighlighter(_,hl))
    case nm : NestedModule =>
      forSource(nm){(start,length) => Annotations.add(nm,start,length,SymbolKind.Module,nm.name.toString,true)}
      nm.module.getPrimitiveDeclarations.foreach(getElems)
    case _ =>
      print("")
  }

  private class HighlightList(val parent : Constant) {
    lazy val th = controller.get(parent.parent)
    private def cmeta(se : StructuralElement) : Option[MPath] = se match {
      case t : Theory => t.meta
      case d : Declaration => cmeta(controller.get(d.parent))
      case _ =>
        print("")
        None
    }
    lazy val meta = cmeta(th)
  }

  private val pragmatics = controller.extman.get(classOf[Pragmatics]).headOption

  private object termHighlighter extends Traverser[HighlightList] {
    import info.kwarc.mmt.api.objects._
    def traverse(t: Term)(implicit con: Context, state: HighlightList): Term = {
      /* def infer(tm : Term) = Try(Solver.infer(controller,Context(state.th.path match {
        case mp : MPath => mp
        case gn : GlobalName => gn.module
      }) ++ con,tm,None)).toOption.flatten.map(i => "Type: " + controller.presenter.asString(i,None)).getOrElse("Unknown type") */
      def headString(tm : Term) = tm.head.map(_.toString).getOrElse("No head")
      t match {
        case o@OMV(_) =>
          forSource(o){(start,length) =>
            val a = Annotations.add(o,start,length+1)
            a.setSemanticHighlightingClass(Colors.termvariable)
            a.setHover({ headString(o) })
          }
          o
        case o : OMLITTrait =>
          forSource(o){(start,length) =>
            val a = Annotations.add(o,start,length+1)
            a.setSemanticHighlightingClass(Colors.termomlit)
            a.setHover({ headString(o) })
          }
          o
        case o : OML =>
          forSource(o){(start,length) =>
            val a = Annotations.add(o,start,length+1)
            a.setSemanticHighlightingClass(Colors.termoml)
            a.setHover({ headString(o) })
          }
          Traverser(this,t)
        case tm if tm.head.isDefined =>
          lazy val pragma = pragmatics.map{pr =>
            pr.mostPragmatic(tm)
          }.getOrElse(tm)
          def ret = tm match {
            case OMID(_) => tm
            case OMA(_,args) =>
              args.foreach(traverse(_))
              tm
            case OMBINDC(_,ctx,bd) =>
              traverseContext(ctx)
              bd.foreach(traverse(_))
              tm
          }
          state.meta.foreach { mt =>
            controller.library.forDeclarationsInScope(OMMOD(mt)) {
              case (_, _, d) if pragma.head.contains(d.path) =>
                forSource(tm){(start,length) =>
                  val a = Annotations.add(tm,start,length+1)
                  a.setSemanticHighlightingClass(Colors.termconstantmeta)
                  a.setHover({ headString(pragma) })
                }
                return ret
              case _ =>
            }
          }
          forSource(tm){(start,length) =>
            val a = Annotations.add(tm,start,length+1)
            a.setHover({ headString(pragma) })
            // TODO ?
          }
          ret
        case _ =>
          Traverser(this,t)
      }
    }

    override def traverseContext(cont: Context)(implicit con: Context, state: HighlightList): Context = {
      cont.foreach{vd =>
        forSource(vd){(start,_) =>
          val a = Annotations.add(vd,start,vd.name.length)
          a.setSemanticHighlightingClass(Colors.termvariable)
        }
      }
      super.traverseContext(cont)
    }
  }

  //private var annotated : AnnotatedText = null

  //private var _highlights : List[Highlight] = Nil

  //override def getHighlights: List[Highlight] = synchronized { _highlights }

  private def parseTop: Unit = {
    synchronized {
      errorCont.resetErrs
      val d = try {
        server.parser.apply(errorCont, docuri, doctext, nsMap)
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          ???
      }
      getElems(d)
    }

    client.documentErrors(this, errorCont.getErrors:_*)
    errorCont.reset
  }

}

class IterativeOParser extends NotationBasedParser {}

class IterativeParser(objectParser : IterativeOParser = new IterativeOParser) extends KeywordBasedParser(objectParser) {
  override def start(args: List[String]): Unit = {
    super.start(args)
    controller.extman.addExtension(objectParser)
  }

  def apply(errorCont : ErrorHandler,dpath : DPath, docstring : String,nsmap : NamespaceMap = controller.getNamespaceMap.copy()) : StructuralElement = {
    val ps = ParsingStream.fromString(docstring,dpath,"mmt",Some(nsmap))
    implicit val spc = new StructureParserContinuations(errorCont) {
      override def onElement(se: StructuralElement): Unit = se match {
        case _ : Document | _ : Namespace | _ : NamespaceImport | _ : MRef | _ : Theory =>
        case c : Constant =>
          val sr = SourceRef.get(c).get
          val string = docstring.slice(sr.region.start.offset,sr.region.end.offset)
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