package info.kwarc.mmt.api.backend

import java.io.BufferedReader

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._
import parser._

// local XML databases or query engines to access local XML files: baseX or Saxon

case class NotApplicable(message: String = "") extends Error(message)

/** An abstraction over physical storage units that hold MMT content */
abstract class Storage {
  /** implementing classes should call this to load an OMDoc XML stream (which this method will close afterwards) */
  protected def loadXML(u: URI, dpath: DPath, reader: BufferedReader)(implicit controller: Controller) {
    val ps = new ParsingStream(u, IsRootDoc(dpath), NamespaceMap(dpath), "omdoc", reader)
    controller.report("storage", "found by " + toString + " at URL " + u)
    try {
      controller.read(ps, interpret = false)(ErrorThrower)
    } finally {
      ps.stream.close
    }
  }

  protected def getSuffix(base: utils.URI, uri: utils.URI): List[String] = {
    val b = base.pathNoTrailingSlash
    val u = uri.pathNoTrailingSlash
    if (uri.scheme == base.scheme && uri.authority == base.authority && u.startsWith(b))
      u.drop(b.length)
    else
      throw NotApplicable()
  }

  /** dereferences a path and adds the content to the controller or throws [[NotApplicable]]
    *  e.g., by sending an XML document to the XML reader
    *
    * a storage may add more/additional content than necessary, e.g., the containing file/theory or a dependency closure
    */
  def load(path: Path)(implicit controller: Controller)

  /** dereferences a path to a fragment of an already loaded StructuralElement and adds only that fragment
   *  empty by default, storages that can retrieve individual fragments should override this
   */
  def loadFragment(neededPath: Path, existingPath: Path)(implicit controller: Controller) {
    throw NotApplicable("")
  }

  //TODO: method for querying
  // def query(q: ???): Iterator[Path]

  /** called to release all held resources, override as needed */
  def destroy {}
}

/** a variant of a [[Storage]] that loads Scala objects from the class path */
trait RealizationStorage {
  /** the class loaded used to load semantic objects
   *
   * By making this a 'def', every load may creates a fresh class loader.
   * That can be helpful if new class files are produced at run time (e.g., with the [[ScalaCompiler]]).   
   */
  def loader: java.lang.ClassLoader
  /**
   * @param p the path to use in error messages
   */
  def loadObject(p: MPath): SemanticObject = {
    val cls = SemanticObject.mmtToJava(p)
    loadSemanticObject(cls, p)
  }

  /** gets the object for a java class name (cls must be in Scala's syntax for java .class files) */
  def loadClass(cls: String, p: Path): Class[_] = {
    try {
      Class.forName(cls, true, loader)
    } catch {
      case e: ClassNotFoundException =>
        throw NotApplicable("class " + cls + " not found")
      case e: ExceptionInInitializerError =>
        throw BackendError(s"class $cls for $p exists, but an error occurred when initializing it", p).setCausedBy(e)
      case e: LinkageError =>
        throw BackendError(s"class $cls for $p exists, but an error occurred when linking it", p).setCausedBy(e)
      case e: Error =>
        throw BackendError(s"class $cls for $p exists, but: " + e.getMessage, p).setCausedBy(e)
    }
  }
  /** gets the object for a java class name (cls must be in Scala's syntax for java .class files) */
  protected def loadSemanticObject(cls: String, p: Path): SemanticObject = {
    val c = loadClass(cls, p)
    val r = try {
      c.getField("MODULE$").get(null)
    } catch {
      case e: Exception =>
        throw BackendError(s"class $cls for $p exists, but an error occurred when accessing the Scala object", p).setCausedBy(e)
    }
    r match {
      case r: SemanticObject =>
        try {
          r.init
        } catch {
          case e: Exception =>
            throw BackendError(s"semantic object $cls for $p exists, but an error occurred when initializing it", p).setCausedBy(e)
        }
        r
      case _ =>
        throw BackendError(s"object $cls for $p exists, but it is not an instance of SemanticObject", p)
    }
  }
}


/** a Storage that retrieves file URIs from the local system */
case class LocalSystem(base: URI) extends Storage {
  val localBase = URI(Some("file"), None, Nil, abs = true, None, None)

  def load(path: Path)(implicit controller: Controller) {
    val uri = base.resolve(path.doc.uri)
    val _ = getSuffix(localBase, uri)
    val file = new java.io.File(uri.toJava)
    loadXML(uri, path.doc, File.Reader(file))
  }
}

/** a Storage that retrieves repository URIs from the local working copy */
class LocalCopy(scheme: String, authority: String, prefix: String, val base: File) extends Storage {
  def localBase = URI(URI(Option(scheme), Option(authority)).toString + prefix)

  /**
   * load delegates to this method if the requested resource is a folder
   * @param uri the logical URI
   * @param suffix the physical location
   */
  def loadFromFolder(uri: URI, suffix: List[String])(implicit controller: Controller) {
    val folder = base / suffix
    val entries = folder.children.filter(x => x.isDirectory || (x.getExtension contains "omdoc"))
    val prefix = if (uri.path.isEmpty) "" else uri.path.last + "/"
    // dref must be unnamed; using name={n} would give the dref the same URI as the referenced document
    val node = <omdoc>{entries.map(n => <dref name="" target={prefix + n.name}/>)}</omdoc>
    val reader = new BufferedReader(new java.io.StringReader(node.toString))
    loadXML(uri, DPath(uri), reader)
  }

  def load(path: Path)(implicit controller: Controller) {
    val uri = path.doc.uri
    val suffix = getSuffix(localBase, uri)
    val target = base / suffix
    if (target.isFile) {
       val reader = File.Reader(target)
       loadXML(uri, path.doc, reader)
    } else if (target.isDirectory) {
       loadFromFolder(uri, suffix)
    } else throw NotApplicable("file/folder " + target + " not found or not accessible: " + path)
  }
}

/**Ï
 * like [[LocalCopy]] but optimized for [[Archive]]s
 *
 * custom HTML snippets are spliced into folder-documents
 * @param a the archive
 * @param folderName file name of folder descriptions in source folder (without .html ending)
 */
class ArchiveNarrationStorage(a: Archive, folderName: String) extends {val nBase = a.narrationBase}
      with LocalCopy(nBase.schemeNull, nBase.authorityNull, nBase.pathAsString, a / narration) {
   override def loadFromFolder(uri: URI, suffix: List[String])(implicit controller: Controller) {
      val narrFolder = base / suffix
      val entries = narrFolder.children.filter(x => x.isDirectory || (x.getExtension contains "omdoc"))
      val prefix = if (uri.path.isEmpty) "" else uri.path.last + "/"
      val descOpt = {
         // TODO test for files with other endings than html, use the ending as the format
         val htmlFile = a / source / suffix / (folderName + ".html")
         if (htmlFile.exists) {
            val htmlString = File.read(htmlFile)
            val htmlDiv = s"""<div class="folder-description">$htmlString</div>"""
            Some(htmlDiv,"html")
         } else None
      }
      val oe = descOpt.map {case (desc,format) =>
         s"""<opaque format="$format">$desc</opaque>"""
      }
      val es = entries.map(n => <dref name={n.name + ".ref"} target={prefix + n.name}/>).mkString("\n")
      val docS = s"""<omdoc level="folder">$oe$es</omdoc>"""
      val reader = new BufferedReader(new java.io.StringReader(docS))
      loadXML(uri, DPath(uri), reader)
   }
}

/** loads a realization from a Java Class Loader and dynamically creates a [[uom.RealizationInScala]] for it */
class RealizationArchive(file: File) extends Storage with RealizationStorage {
  override def toString = "RealizationArchive for " + file

  // this must be a val to make sure all classes are loaded by the same class loader (the same class loaded by different class loaders are distinct)
  val loader: ClassLoader = try {
    val optCl = Option(getClass.getClassLoader)
    // the class loader that loaded this class, may be null for bootstrap class loader
    optCl match {
      case None =>
        new java.net.URLClassLoader(Array(file.toURI.toURL)) // parent defaults to bootstrap class loader
      case Some(cl) =>
        // delegate to the class loader that loaded MMT - needed if classes to be loaded depend on MMT classes
        new java.net.URLClassLoader(Array(file.toURI.toURL), cl)
    }
  } catch {
    case e: Exception =>
      throw  GeneralError("could not create class loader for " + file.toString).setCausedBy(e)
  }

  def load(path: Path)(implicit controller: Controller) {
    val mp = path match {
      case mp: MPath => mp
      case GlobalName(mp1, _) => mp1
      case _ => throw NotApplicable("no module path found")
    }
    val s = uom.GenericScalaExporter.mpathToScala(mp)
    //controller.report("backend", "trying to load class " + s)
    val r = loadSemanticObject(s + "$", mp) match {
      case r: uom.RealizedTheory => r
      case _ => throw BackendError("class for " + mp + " exists but is not a realization", mp)
    }
    try {
      r.init
    }
    catch {
      case e: Error =>
        throw BackendError("body of " + mp + " ill-formed: " + e.getMessage, mp).setCausedBy(e)
    }
    controller.add(r)
  }
}

/** loads a rule from the default classpath */
object DefaultRealizationLoader extends Storage with RealizationStorage {
  def load(path: Path)(implicit controller: Controller) {
    throw NotApplicable("can only load rules")
  }
  def loader: ClassLoader = this.getClass.getClassLoader
}
