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
abstract class Storage extends QueryResolver with OntologyResolver {
  protected def loadXML(u: URI, dpath: DPath, reader: BufferedReader)(implicit controller: Controller) {
    val ps = new ParsingStream(u, IsRootDoc(dpath), NamespaceMap(dpath), "omdoc", reader)
    controller.report("storage", "found by " + toString + " at URL " + u)
    controller.read(ps, interpret = false)(ErrorThrower)
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
   *  empty by default, storage that can retrieve individual fragments should override this
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
  val loader: java.lang.ClassLoader
  /**
   * @param cls the class name
   * @param p the path to use in error messages
   */
  def loadRule(cls: String, p: Path): Rule = {
    reflect(cls, p) match {
      case r: Rule => r
      case _ => throw BackendError("class for " + cls + " exists but is not a rule", p)
    }
  }

  /** gets the Scala object for a class name */
  def reflect(s: String, p: Path): AnyRef = {
    val cls = s + "$"
    val c = try {
      Class.forName(cls, true, loader)
    }
    catch {
      case e: ClassNotFoundException =>
        throw NotApplicable("class " + cls + " not found")
      case e: ExceptionInInitializerError =>
        throw BackendError("class for " + p + " exists, but an error occurred when initializing it", p).setCausedBy(e)
      case e: LinkageError =>
        throw BackendError("class for " + p + " exists, but an error occurred when linking it", p).setCausedBy(e)
      case e: Error =>
        throw BackendError("class for " + p + " exists, but: " + e.getMessage, p).setCausedBy(e)
    }
    try {
      c.getField("MODULE$").get(null)
    }
    catch {
      case e: java.lang.Exception =>
        throw BackendError("java class for " + p + " exists, but an error occurred when accessing the Scala object", p).setCausedBy(e)
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
    val entries = folder.list.toList.sorted.diff(List(".svn"))
    val prefix = if (uri.path.isEmpty) "" else uri.path.last + "/"
    // dref must be unnamed; using name={n} would give the dref the same URI as the referenced document
    val node = <omdoc>{entries.map(n => <dref name="" target={prefix + n}/>)}</omdoc>
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
    } else throw BackendError("file/folder " + target + " not found or not accessible", path)
  }
}

/**
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
      val entries = narrFolder.list.toList.sorted.diff(List(".svn"))
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
      val es = entries.map(n => <dref name="" target={prefix + n}/>).mkString("\n")
      val docS = s"""<omdoc>$oe$es</omdoc>"""
      val reader = new BufferedReader(new java.io.StringReader(docS))
      loadXML(uri, DPath(uri), reader)
   }
}

/** loads a realization from a Java Class Loader and dynamically creates a [[uom.RealizationInScala]] for it */
class RealizationArchive(file: File, val loader: java.net.URLClassLoader) extends Storage with RealizationStorage {
  override def toString = "RealizationArchive for " + file

  def load(path: Path)(implicit controller: Controller) {
    val mp = path match {
      case mp: MPath => mp
      case GlobalName(mp1, _) => mp1
      case _ => throw NotApplicable("no module path found")
    }
    val s = uom.GenericScalaExporter.mpathToScala(mp)
    //controller.report("backend", "trying to load class " + s)
    val r = reflect(s, mp) match {
      case r: uom.RealizationInScala => r
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
  val loader = this.getClass().getClassLoader()
}
