package info.kwarc.mmt.api.ontology.rdf

import info.kwarc.mmt.api.{DPath, DeclarationComponent, Path, StructuralElement}
import info.kwarc.mmt.api.documents.{Document, InterpretationInstruction, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{Link, Module, Theory, View}
import info.kwarc.mmt.api.objects.{OMPMOD, TheoryExp}
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.parser.{FileInArchiveSource, SourceInfo, SourceRef}
import info.kwarc.mmt.api.parser.SourceRef.toURI
import info.kwarc.mmt.api.patterns.{Instance, Pattern}
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration, NestedModule, ObjContainer, RuleConstant, Structure}
import info.kwarc.mmt.api.utils.{File, URI}
import org.eclipse.rdf4j.model.Resource
import org.eclipse.rdf4j.repository.sail.SailRepository
import org.eclipse.rdf4j.sail.memory.MemoryStore

object Database {
  def get(controller : Controller) = controller.extman.get(classOf[Database]).head
}

class Database extends Extension {
  import org.eclipse.rdf4j.model.util.Values.iri

  private lazy val repo = new SailRepository(new MemoryStore())
  private lazy val connection = repo.getConnection
  private val mmt_uri = URI.scheme("mmt")
  private val memory_uri = DPath(mmt_uri colon "memory")

  override def start(args: List[String]): Unit = {
    super.start(args)
    connection.add(ULO.model)
    controller.backend.getArchives.foreach(_.id.split('/').toList match {
      case List(id) =>
        add(ULO.library(DPath(mmt_uri colon id)),mmt_uri)
      case List(a,b) =>
        val group = DPath(mmt_uri colon a)
        val arch = DPath(mmt_uri colon (a + "." + b))
        add(ULO.library_group(group),mmt_uri)
        add(ULO.library(arch),mmt_uri)
        add(ULO.contains(group,arch),mmt_uri)
    })
  }

  def add(e : StructuralElement,si : SourceInfo) : Unit = {} /* si match {
    case FileInArchiveSource(a, f) =>
      import info.kwarc.mmt.api.archives._
      val uri = DPath(mmt_uri colon a.id.replace('/', '.'))
      val segments = if ((a / content) <= f) SourceRef.get(e) match {
        case Some(sr) if sr.container.toString.startsWith(a.narrationBase.toString) =>
          print("")
          sr.container.toString.drop(a.narrationBase.toString.length+1).split('/').toList
        case None =>
          return ()
        case _ =>
          ???
      } else {
        a.root.relativize(f).segments.tail
      }
      val file = segments.foldLeft(uri)((p, d) => p / d)
      add(ULO.contains(uri, file), uri.uri)
      add(ULO.declares(file, e.path), uri.uri)
      addElement(e)(uri.uri)
    case _ =>
      print("")
  } */

  def add(e : StructuralElement) : Unit = {
    SourceRef.get(e)
    print("")
  }

  private def add(s : ULOStatement,context : URI) : Unit = s.triples.foreach{case (s,p,o) => connection.add(s,p,o,iri(context.toString))}

  private def addElement(e: StructuralElement)(implicit context : URI): Unit = {
    implicit val elem : StructuralElement = e
    val path = e.path
    SourceRef.get(e) match {
      case Some(sr) =>
        add(ULO.sourceref(path,toURI(sr)),context)
      case _ =>
    }
    e match {
      case d: Document =>
        add(ULO.document(d.path),context)
        //TODO should use getLocalItems but then it wouldn't work for documents created from folders
        d.getDeclarations.foreach {i =>
          val cO = i match {
            case inner: Document =>
              addElement(inner)
              Some(inner.path)
            case oe: OpaqueElement =>
              None
            case nr: NRef =>
              Some(nr.target)
            case ii: InterpretationInstruction =>
              None
          }
          cO foreach {c => add(ULO.declares(d.path,c),context)}
        }
      case n: NRef =>
      case oe: OpaqueElement =>
      case t: Theory =>
        add(ULO.theory(path),context)
        t match {
          case t: Theory =>
            t.meta foreach {p => add(ULO.HasMeta(t.path, p),context)}
          case _ =>
        }
      case v: View =>
        add(ULO.View(v.path,v.from.toMPath,v.to.toMPath),context)
      case _ =>
    }
    e match {
      case t: Module =>
        t.getDeclarations foreach {d => {
          add(ULO.declares(path,d.path),context)
          d match {
            case c: Constant =>
              add(ULO.constant(path),context)
              add(ULO.name(path,c.name),context)
              c.alias foreach {a =>
                add(ULO.name(path,a),context)
              }
              // extract dependencies - this may make the rel files a lot bigger
              c.getComponents foreach {
                case DeclarationComponent(dim, oc: ObjContainer[_]) => doDependencies(c.path $ dim, oc)
                case _ =>
              }
            case s: Structure =>
              val from = s.from match {
                case OMPMOD(p,_) => p
                case f => TheoryExp.simplify(s.from).toMPath
              }
              if (s.isInclude) {
                if (!s.isGenerated) {
                  // we do not export transitive includes
                  add(ULO.Include(from,t.path,Some(s.path)),context)
                }
              } else {
                add(ULO.Structure(s.path,from,TheoryExp.simplify(s.to).toMPath),context)
              }
            case rc: RuleConstant =>
              add(ULO.rule_constant(path),context)
            case dd: DerivedDeclaration =>
              dd.feature match {
                case Pattern.feature => add(ULO.pattern(path),context)
                case Instance.feature =>
                  add(ULO.instance(path),context)
                  dd.tpC.get match {
                    case Some(Instance.Type(p,_)) => add(ULO.instance_of(path,p),context)
                    case _ =>
                  }
                case _ =>
                  add(ULO.derived_declaration(path),context)
              }
            case nm: NestedModule =>
              add(ULO.nested_module(path),context)
              add(ULO.declares(nm.home.toMPath,path),context)
              addElement(nm.module)
          }
        }}
      case _ =>
    }
    e match {
      case l: Link if l.isImplicit =>
        add(ULO.implicit_morphism(path),context)
      case _ =>
    }
  }
  /** extract all dependencies of object containers */
  private def doDependencies(path: Path, oc: ObjContainer[_])(implicit e : StructuralElement,context : URI) {
    oc.dependsOn foreach {p =>
      val r = ULO.depends_on(path,p)
      add(r,context)
    }
  }
}