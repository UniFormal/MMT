package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.archives.{Archive, MathHub, SimpleStatistics}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.mathhub.library.Context.Builders.Special.{SmglomTree, TestTree}
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IReferencable, IReference, IStatistic}

import scala.collection.mutable
import scala.util.Try

trait Builder
  extends Logger
    with Cache
    with Getters
    with Statistics
    with GroupsBuilder
    with GroupBuilder
    with TagBuilder
    with ArchiveBuilder
    with DocumentBuilder
    with OpaqueBuilder
    with ModuleWrap
    with TheoryBuilder
    with ViewBuilder
    with DeclarationWrap
    with StructureBuilder
    with ConstantBuilder
    with RuleBuilder
    with NestedModuleBuilder
{ this: MathHubAPIContext =>

  protected val controller: Controller

  /** opaque element builders */
  protected val pseudos: List[PseudoBuilder] =
    List(
      // new TestTree(controller, mathHub)
      new SmglomTree(controller, mathHub)
    ).map(e =>new PseudoBuilder(e))

  /** logs something for debugging the mathhub extensiion */
  protected def logDebug(s: => String): Unit = log(s, Some("debug"))

  protected def mathHub: MathHub = controller.getMathHub.getOrElse(throw GeneralError("No MathHub configured"))

  /** gets a reference to an object, either from the cache or newly built */
  def getReference(id: String): Option[IReference] = if (id.contains(":")) {
    getPseudoReference(id) match {
      case Some(ref) => return Some(ref)
      case _ =>
    }
    getLibraryReference(
      Try(Path.parse(id)).getOrElse(return buildFailure(id, "Path.parse(ref.id)")),
      id
    )
  } else {
    getHubReference(id)
  }

  /** gets a reference to a hub element */
  def getHubReference(id: String): Option[IReference] = memoizeReference(id) (logGroup {
    utils.firstDefined(
      {_ => tryGroup(id).flatMap(buildGroupReference)},
      {_ => tryTag(id).flatMap(buildTagReference)},
      {_ => tryArchive(id).flatMap(buildArchiveReference)}
    )
  })

  /** gets a reference to a library element */
  def getLibraryReference(path: Path, id: String): Option[IReference] = memoizeReference(path.toPath) (logGroup {
    // try to get the path or
    controller.getO(path).getOrElse(return buildFailure(id, "controller.getO(ref.path)")) match {
      case view: View => buildViewReference(view)
      case theory: Theory => buildTheoryReference(theory)

      case s: Structure => buildStructureReference(s)
      case c: Constant => buildConstantReference(c)
      case rc: RuleConstant => buildRuleReference(rc)
      case nm: NestedModule => buildNestedModuleReference(nm)

      case opaque: OpaqueElement => buildOpaqueReference(opaque)
      case document: Document => buildDocumentReference(document)

      case _ => buildFailure(id, "controller.get(ref.path) match")
    }
  })

  def getLibraryReference(path: Path): Option[IReference] = getLibraryReference(path, path.toPath)

  /** attempts to build a pseudo object for the given type */
  def getPseudoReference(id: String): Option[IReference] = {
    pseudos.foreach(pseudo => {
      pseudo.buildTreeRef(this, id) match {
        case Some(r) => return Some(r)
        case _ =>
      }
      pseudo.buildContentRef(this, id) match {
        case Some(r) => return Some(r)
        case _ =>
      }
    })
    None
  }


  /** gets an object, either from the cache or newly built */
  def getObject(id: String): Option[IReferencable] = if (id.contains(":")) {
    getPseudoObject(id) match {
      case Some(o) => return Some(o)
      case _ =>
    }
    getLibraryObject(
      Try(Path.parse(id)).getOrElse(return buildFailure(id, "Path.parse(ref.id)")),
      id
    )
  } else {
    getHubObject(id)
  }

  /** gets a hub object (i.e. archive, tag, group) */
  def getHubObject(id: String): Option[IReferencable] = memoizeObject(id) (logGroup {
    utils.firstDefined(
      {_ => tryGroup(id).flatMap(buildGroup)},
      {_ => tryTag(id).flatMap(buildTag)},
      {_ => tryArchive(id).flatMap(buildArchive)}
    )
  })

  /** gets an object from the library */
  def getLibraryObject(path: Path, id: String): Option[IReferencable] = memoizeObject(id) (logGroup {
    controller.getO(path).getOrElse(return buildFailure(id, "controller.getO(object.path)")) match {
      case view: View => buildView(view)
      case theory: Theory => buildTheory(theory)

      case s: Structure => buildStructure(s)
      case c: Constant => buildConstant(c)
      case rc: RuleConstant => buildRule(rc)
      case nm: NestedModule => buildNestedModule(nm)

      case opaque: OpaqueElement => buildOpaque(opaque)
      case document: Document => buildDocument(document)

      case _ => buildFailure(id, "controller.get(object.path) match")
    }
  })

  /** attempts to build a pseudo object for the given id */
  def getPseudoObject(id: String): Option[IReferencable] = {
    pseudos.foreach(pseudo => {
      pseudo.buildTreeDoc(this, id) match {
        case Some(o) => return Some(o)
        case _ =>
      }
      pseudo.buildContentObj(this, id) match {
        case Some(o) => return Some(o)
        case _ =>
      }
    })
    None
  }

  /** indicates that a failure has occured during then build */
  protected def buildFailure(uri: String, during: String): Option[Nothing] ={
    logDebug(s"failed to build $uri: $during failed ")
    None
  }
}

trait Cache { this: MathHubAPIContext =>
  /** runs a transaction that blocks clearing the cache */
  def transaction[T](name: String, t: MathHubAPIContext => T): T = {
    // block clearing
    synchronized {
      blockClear += 1
    }

    try {
      logGroup {
        val start = System.currentTimeMillis()
        log(s"starting transaction $name")
        val r = t(this)
        val end = System.currentTimeMillis()
        log(s"ending transaction $name, took ${end - start} ms")
        r
      }
    } finally {
      synchronized {
        // if we requested clearing, run it
        if (blockClear == 1 && requestedClear){
          requestedClear = false
          clearInternal()
        }

        // release the clearing lock
        blockClear -= 1

        // and trim the cache
        trimCache()
      }
    }
  }


  /** a map of ids to references to objects */
  private val refCache = mutable.LinkedHashMap[String, IReference]()
  protected def memoizeReference(id: String)(fill: => Option[IReference]): Option[IReference] = refCache.get(id) match {
    case Some(x) =>
      logDebug(s"Cache Hit for Reference: $id")
      Some(x)
    case None =>
      logDebug(s"Cache Miss for Reference: $id")
      fill match {
        case Some(x) => synchronized {
          refCache(id) = x
          Some(x)
        }
        case None => None
      }
  }

  /** a map of ids to objects */
  private val objCache = mutable.LinkedHashMap[String, IReferencable]()
  protected def memoizeObject(id: String)(fill: => Option[IReferencable]): Option[IReferencable] = objCache.get(id) match {
    case Some(x) =>
      logDebug(s"Cache Hit for Object: $id")
      Some(x)
    case None =>
      logDebug(s"Cache Miss for Object: $id")
      fill match {
        case Some(x) => synchronized {

          objCache(id) = x
          Some(x)
        }
        case None => None
      }
  }

  private val maxCacheSize = 10000
  private var requestedClear: Boolean = false
  private var blockClear: Integer = 0

  /** trims the cache to be of a certain size */
  private def trimCache(): Unit = synchronized {
    if (blockClear == 0) {
      objCache.keys.dropRight(maxCacheSize).foreach(objCache.remove)
      refCache.keys.view.filter(objCache.contains).dropRight(maxCacheSize).foreach(refCache.remove)
    }
  }


  /** removes all items from the cache */
  def clearCache(): Unit = synchronized {
    if (blockClear == 0) {
      clearInternal()
    } else {
      requestedClear = true
    }
  }

  private def clearInternal(): Unit = synchronized {
    refCache.keys.foreach(refCache.remove)
    objCache.keys.foreach(objCache.remove)
  }

  /** called after adding the element */
  def onAdd(c: StructuralElement): Unit = {}
  /** called after deleting the element
    *  @param old the now-deleted element
    */
  def onDelete(old: StructuralElement): Unit = {
    logDebug(s"deleted an element, clearing cache")
    clearCache()
  }
  /** called after clearing the Constant */
  def onClear(): Unit = {
    logDebug(s"cleared an element, clearing cache")
    clearCache()
  }
  /** called when a new archive is added */
  def onArchiveOpen(a: Archive): Unit = {
    logDebug(s"opened an archive, clearing cache")
    clearCache()
  }
  /** called when an archive is removed */
  def onArchiveClose(a: Archive): Unit = {
    logDebug(s"closed an archive, clearing cache")
    clearCache()
  }

}

trait Getters { this: Builder =>
  /** gets an object of a specific type */
  protected def getObjectOf[T <: IReferencable](cls: Class[T], id: String): Option[T] = logGroup {
    logDebug(s"trying to get object of kind ${cls.getName} $id")
    getObject(id) match {
      case Some(t: T@unchecked) if cls.isInstance(t) => {
        logDebug(s"got object for for $id")
        Some(t)
      }
      case _ => {
        logDebug(s"did not get object for $id")
        None
      }
    }
  }

  /**
    * Gets a reference of a given type or stores default as the type
    */
  protected def getObjectOrElse[T <: IReferencable](cls: Class[T], id: String)(default: => Option[T]): Option[T] = memoizeObject(id)(default) match {
    case None => None
    case Some(theRef) =>
      if (cls.isInstance(theRef)) {
        Some(theRef.asInstanceOf[T])
      } else {
        None
      }
  }

  /** gets a reference of a specific type */
  protected def getReferenceOf[T <: IReference](cls: Class[T], id: String): Option[T] = logGroup {
    logDebug(s"trying to get reference of kind ${cls.getName} $id")
    getReference(id) match {
      case Some(t: T@unchecked) if cls.isInstance(t) => {
        logDebug(s"got reference for $id")
        Some(t)
      }
      case rv => {
        logDebug(s"did not get reference for $id")
        None
      }
    }
  }

  /**
    * Gets a reference of a given type or stores default as the type
    */
  protected def getReferenceOrElse[T <: IReference](cls: Class[T], id: String)(default: => Option[T]): Option[T] = memoizeReference(id)(default) match {
    case None => None
    case Some(theRef) =>
      if (cls.isInstance(theRef)) {
        Some(theRef.asInstanceOf[T])
      } else {
        None
      }
    }
  }


trait Statistics { this: Builder =>
  // TODO: Build statistics in a cached form
  protected def getStats(path: String): Option[List[IStatistic]] = None
  protected def getStats(stats: Option[SimpleStatistics]): Option[List[IStatistic]] = stats.map {s =>
    List(IStatistic("any_con", s.constantCount), IStatistic("theo", s.theoryCount))
  }
}