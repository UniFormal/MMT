package info.kwarc.mmt.mathhub.library

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.lmh.{LMHHubGroupEntry, _}
import info.kwarc.mmt.api.documents.{Document, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.modules.{DeclaredTheory, Theory, View}
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.presentation.{HTMLExporter, StringBuilder}
import info.kwarc.mmt.api.utils.{File, mmt}

import scala.collection.mutable

/** A caching API that can resolve api items */
class MathHubAPIContext(val controller: Controller, val report: Report) extends Logger {
  def logPrefix: String = "mathhub"

  private def mathHub: MathHub = controller.getMathHub.getOrElse(throw GeneralError("No MathHub configured"))

  // region "Cache"

  /** a map of ids to references to objects */
  private val refCache = mutable.Map[String, IReference]()

  /** a map of ids to objects */
  private val objCache = mutable.Map[String, IReferencable]()

  /** removes a specific item from the cache */
  def clearCacheFor(id: String): Unit = {
    refCache.remove(id)
    objCache.remove(id)
  }

  /** removes all items from the cache */
  def clearCache(): Unit = {
    refCache.keys.foreach(refCache.remove)
    objCache.keys.foreach(objCache.remove)
  }

  // endregion


  // region "Getters"

  /** gets a reference to an object, either from the cache or newly built */
  def getReference(id: String): Option[IReference] = {
    refCache.get(id) match {
      // item exists in cache => return it
      case Some(ref) => Some(ref)

      // item does not exist in cache => build it
      case None =>  buildReference(id) match {

        // it doesn't exist => return Nothing
        case None => None

        // it exists => store it in cache and return
        case Some(ref) => {
          refCache(id) = ref
          Some(ref)
        }
      }
    }
  }

  def getGroups() : List[IGroupRef] = {
    val mh = mathHub
    mh.entries_
      .collect({case ae: mh.MathHubGroupEntry => ae.group }).distinct
      .flatMap(e => getGroupRef(e))
  }

  /** gets a reference of a specific type */
  private def getReferenceOf[T <: IReference](cls: Class[T], id: String): Option[T] = logGroup {
    log(s"trying to get reference of kind ${cls.getName} $id")
    getReference(id) match {
      case Some(t: T@unchecked) if cls.isInstance(t) => {
        log(s"got reference for $id")
        Some(t)
      }
      case rv => {
        log(s"did not get reference for $id")
        None
      }
    }
  }

  /** gets a reference to a group */
  def getGroupRef(id: String): Option[IGroupRef] = getReferenceOf(classOf[IGroupRef], id)

  /** gets a reference to an archive */
  def getArchiveRef(id: String): Option[IArchiveRef] = getReferenceOf(classOf[IArchiveRef], id)

  /** gets a reference to a narrative parent */
  def getNarrativeParentRef(id: String): Option[IDocumentParentRef] = {
    getArchiveRef(id).map(Some(_)).getOrElse(getDocumentRef(id)) // ArchiveRef | DocumentRef
  }

  /** gets a reference to a document */
  def getDocumentRef(id: String): Option[IDocumentRef] = getReferenceOf(classOf[IDocumentRef], id)

  /** gets a reference to an opaque element */
  def getOpaqueElementRef(id: String): Option[IOpaqueElementRef] = getReferenceOf(classOf[IOpaqueElementRef], id)

  /** gets a reference to a module */
  def getModuleRef(id: String): Option[IModuleRef] = {
    getViewRef(id).map(Some(_)).getOrElse(getTheoryRef(id)) // View | Theory
  }

  /** gets a reference to a view */
  def getViewRef(id: String): Option[IViewRef] = getReferenceOf(classOf[IViewRef], id)

  /** gets a reference to a theory */
  def getTheoryRef(id: String): Option[ITheoryRef] = getReferenceOf(classOf[ITheoryRef], id)

  /** gets an object, either from the cache or newly built */
  def getObject(id: String): Option[IReferencable] = {
    objCache.get(id) match {
      // item exists in cache => return it
      case Some(obj) => Some(obj)

      // item does not exist in cache => build it
      case None =>  buildObject(id) match {

        // it doesn't exist => return Nothing
        case None => None

        // it exists => store it in cache and return
        case Some(obj) => {
          objCache(id) = obj
          Some(obj)
        }
      }
    }
  }

  /** gets an object of a specific type */
  private def getObjectOf[T <: IReferencable](cls: Class[T], id: String): Option[T] = logGroup {
    log(s"trying to get object of kind ${cls.getName} $id")
    getObject(id) match {
      case Some(t: T@unchecked) if cls.isInstance(t) => {
        log(s"got object for for $id")
        Some(t)
      }
      case _ => {
        log(s"did not get object for $id")
        None
      }
    }
  }

  /** gets a group */
  def getGroup(id: String): Option[IGroup] = getObjectOf(classOf[IGroup], id)

  /** gets an archive */
  def getArchive(id: String): Option[IArchive] = getObjectOf(classOf[IArchive], id)

  /** gets a narrative element */
  def getNarrativeElement(id: String): Option[INarrativeElement] = {
    // IOpaqueElement | IDocument | IDocumentRef | IModuleRef

    // TODO: This always sends a full document and not a reference
    getOpaqueElement(id).map(Some(_))
      .getOrElse(getDocument(id)).map(Some(_))
      .getOrElse(getModuleRef(id))
  }

  /** gets a document */
  def getDocument(id: String): Option[IDocument] = getObjectOf(classOf[IDocument], id)

  /** gets an opaque element */
  def getOpaqueElement(id: String): Option[IOpaqueElement] = getObjectOf(classOf[IOpaqueElement], id)

  /** gets a module */
  def getModule(id: String): Option[IModule] = {
    // View | Theory
    getView(id).map(Some(_))
      .getOrElse(getTheory(id))
  }

  /** gets a theory */
  def getView(id: String): Option[IView] = getObjectOf(classOf[IView], id)

  /** gets a theory */
  def getTheory(id: String): Option[ITheory] = getObjectOf(classOf[ITheory], id)

  // endregion

  // region "Builders"

  /** indicates that a failure has occured during then build */
  private def buildFailure(uri: String, during: String): Option[Nothing] ={
    log(s"failed to build $uri: $during failed ")
    None
  }

  /** tries to find a group with the given id */
  private def tryGroup(id: String) : Option[LMHHubGroupEntry] = {
    log(s"trying $id as group")

    val optEntry = mathHub.entries_.collectFirst({
      case e: LMHHubGroupEntry if e.group == id => e
    })

    if(optEntry.isEmpty){
      log(s"$id is not a group")
    }

    optEntry
  }

  /** tries to find an archive with a given id */
  private def tryArchive(id: String) : Option[LMHHubArchiveEntry] = {
    log(s"trying $id as archive")
    val optEntry = mathHub.entries_.collectFirst({
      case e: LMHHubArchiveEntry if e.id == id => e
    })

    if(optEntry.isEmpty){
      log(s"$id is not an archive")
    }

    optEntry
  }

  /** builds a reference to an item, if it exists in the controller */
  private def buildReference(id: String): Option[IReference] = logGroup {

    // try to build a group reference
    tryGroup(id).map(e => return buildGroupReference(e))

    // try to build an archive reference
    tryArchive(id).map(e => return buildArchiveReference(e))


    // try to parse the path or fail
    val path = try{
      Path.parse(id, controller.getNamespaceMap)
    } catch {
      case _: Exception => return buildFailure(id, "Path.parse(ref.id)")
    }

    // try to get the path or

    controller.getO(path).getOrElse(return buildFailure(id, "controller.getO(ref.path)")) match {
      case view: View => buildViewReference(view)
      case theory: Theory => buildTheoryReference(theory)

      case opaque: OpaqueElement => buildOpaqueReference(opaque)
      case document: Document => buildDocumentReference(document)

      case _ => buildFailure(id, "controller.get(ref.path) match")
    }
  }

  /** builds a reference to a group */
  private def buildGroupReference(entry: LMHHubGroupEntry) : Option[IGroupRef] = Some(
    IGroupRef(
      entry.group, /* id */
      entry.group, /* name */
      entry.properties.getOrElse("title", return buildFailure(entry.id, "group.title")), /* title */
      entry.properties.getOrElse("teaser", return buildFailure(entry.id, "group.teaser")) /* teaser */
    )
  )

  /** builds a reference to an archive */
  private def buildArchiveReference(entry: LMHHubArchiveEntry) : Option[IArchiveRef] = Some(
    IArchiveRef(
      getGroupRef(entry.group).map(Some(_)).getOrElse(return buildFailure(entry.id, "archive.parent")), /* parent */
      entry.id, /* id */
      entry.name, /* name */
      entry.archive.properties.getOrElse("title", return buildFailure(entry.id, "archive.title")), /* title */
      entry.archive.properties.getOrElse("teaser", return buildFailure(entry.id, "archive.teaser")) /* teaser */
    )
  )

  /** builds a reference to a document */
  private def buildDocumentReference(document: Document) : Option[IDocumentRef] = {

    log(s"parent of ${document} is ${document.parent.toPath}")
    // if we are the narrationBase of an archive, that is our parent
    val parent = mathHub.entries_.find({
      case ae: LMHHubArchiveEntry => ae.archive.narrationBase.toString == document.path.toPath
      case _ => false
    }) match {
      case Some(ae) => getArchiveRef(ae.id)
      // if not, the parent is simply a document
      case None => getDocumentRef(document.path.^.toPath) // TODO: is parent working properly?
    }


    Some(
      IDocumentRef(
        parent.map(Some(_))
          .getOrElse(return buildFailure(document.path.toPath, "getRef(document.parent)")),/* parent */
        document.path.toPath, /* id */
        document.name.last.toPath /* name */
      )
    )
  }

  /** builds a reference to an opaque element */
  private def buildOpaqueReference(opaque: OpaqueElement) : Option[IOpaqueElementRef] = Some(
    IOpaqueElementRef(
      getDocumentRef(opaque.path.^.toPath).map(Some(_)) // TODO: is parent working properly?
        .getOrElse(return buildFailure(opaque.path.toPath, "getDocumentRef(opaque.parent)")),/* parent */
      opaque.path.toPath, /* id */
      opaque.name.last.toPath /* name */
    )
  )

  /** builds a reference to a view */
  private def buildViewReference(view: View) : Option[IViewRef] = Some(
    IViewRef(
      view.path.toPath, /* id */
      view.name.toPath /* name */
    )
  )

  /** builds a reference to a theory */
  private def buildTheoryReference(theory: Theory) : Option[ITheoryRef] = Some(
    ITheoryRef(
      theory.path.toPath, /* id */
      theory.name.toPath /* name */
    )
  )


  /** builds a reference to an object, if it exists in the controller */
  private def buildObject(id: String): Option[IReferencable] = logGroup {

    // try to build a group reference
    tryGroup(id).map(e => return buildGroup(e))

    // try to build an archive reference
    tryArchive(id).map(e => return buildArchive(e))


    // try to parse the path or fail
    val path = try{
      Path.parse(id, controller.getNamespaceMap)
    } catch {
      case _: Exception => return buildFailure(id, "Path.parse(object.id)")
    }

    // try to get the path or

    controller.getO(path).getOrElse(return buildFailure(id, "controller.getO(object.path)")) match {
      case view: View => buildView(view)
      case theory: Theory => buildTheory(theory)

      case opaque: OpaqueElement => buildOpaque(opaque)
      case document: Document => buildDocument(document)

      case _ => buildFailure(id, "controller.get(object.path) match")
    }
  }

  /** builds a group representation */
  private def buildGroup(entry: LMHHubGroupEntry): Option[IGroup] = {
    val ref = getGroupRef(entry.group).getOrElse(return buildFailure(entry.group, "getGroupRef(group.id)"))

    // get the description file
    val file = entry.root / entry.properties.getOrElse("description", "desc.html")
    val description = if(entry.root <= file && file.exists()) {
      File.read(file)
    } else { "No description provided" }

    val responsible = entry.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

    val archives = mathHub.entries_.collect({case archive: LMHHubArchiveEntry if archive.group == ref.id => archive.id })

    Some(IGroup(
      ref.id, ref.name,
      getStats(ref.id),
      ref.title, ref.teaser,

      description,
      responsible,
      archives.flatMap(getArchiveRef)
    ))
  }

  /** builds an archive representation */
  private def buildArchive(entry: LMHHubArchiveEntry): Option[IArchive] = {
    val ref = getArchiveRef(entry.id).getOrElse(return buildFailure(entry.id, "getArchiveRef(archive.id)"))

    // get the description file
    val file = entry.root / entry.archive.properties.getOrElse("description", "desc.html")
    val description = if(entry.root <= file && file.exists()) {
      File.read(file)
    } else { "No description provided" }

    val responsible = entry.archive.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

    val narrativeRoot = getDocument(entry.archive.narrationBase.toString)
        .getOrElse(return buildFailure(entry.id, "getDocument(archive.narrativeRoot)"))

    Some(IArchive(
      ref.parent, ref.id, ref.name,
      getStats(ref.id),
      ref.title, ref.teaser,

      description,
      responsible,

      narrativeRoot
    ))
  }

  /** builds a document representation */
  private def buildDocument(document: Document): Option[IDocument] = {
    val ref = getDocumentRef(document.path.toPath)
      .getOrElse(return buildFailure(document.path.toPath, "getDocumentRef(document.id)"))

    // find all the declarations
    // TODO: Implement other types and section references
    val decls = document.getDeclarations.flatMap({

      // IOpaqueElement
      case o: OpaqueElement => getOpaqueElement(o.path.toPath).map(Some(_))
          .getOrElse(return buildFailure(document.path.toPath, s"getOpaqueElement(document.decls[${o.path.toPath}])"))

      // IDocument
      case d: Document => getDocument(d.path.toPath).map(Some(_))
          .getOrElse(return buildFailure(document.path.toPath, s"getDocument(document.decls[${d.path.toPath}])"))

      // Reference, currently support IDocumentRef and IModuleRef
      case n: NRef => {
        val path = n.target.toPath
        getReference(path) match {
          case Some(mr: IModuleRef) => Some(mr)
          case Some(dr: IDocumentRef) => Some(dr)

          // TODO: Support other types once they have been implemented
          case _ => {
            log(s"buildDocument: ignoring unknown reference child ${path} of ${document.path.toPath} (not Module, Document)")
            None
          }
        }
      }
      // TODO: Implement URIs
      case d => {
        log(s"buildDocument: ignoring unknown child ${d.path.toPath} of ${document.path.toPath} (not OpaqueElement, Document or MRef)")
        None
      }
    })

    Some(IDocument(
      ref.parent, ref.id, ref.name,
      getStats(ref.id),

      decls
    ))
  }

  /** builds an opaque representation */
  private def buildOpaque(opaque: OpaqueElement): Option[IOpaqueElement] = {
    val ref = getOpaqueElementRef(opaque.path.toPath)
      .getOrElse(return buildFailure(opaque.path.toPath, "getOpaqueElementRef(opaque.id)"))

    Some(IOpaqueElement(
      ref.parent, ref.id, ref.name,
      getStats(ref.id),

      opaque.format,
      opaque.raw.toString
    ))
  }

  /** presents any element for the MathHub API */
  private def getPresentationOf(se: StructuralElement): String = {
    val exporter = controller.extman.get(classOf[HTMLExporter]).head // TODO: Build a custom presenter
    val sb = new StringBuilder
    exporter(se, standalone = false)(sb)
    sb.get
  }

  /** gets a string representing the source code of a specific object */
  private def getSourceOf(md: HasMetaData): Option[String] = {
    // get the source ref string
    val sourceRef = md.metadata.getLinks(mmt.mmtbase ? "metadata" ? "sourceRef")
        .headOption.map(SourceRef.fromURI).getOrElse(return None)

    // try to resolve the physical path to the source in the source Dimension
    val (archive, path) = controller.backend.resolveLogical(sourceRef.container).getOrElse(return None)
    val sourcePath = path.foldLeft[File](archive.root / archive.resolveDimension(archives.source))((p, c) => p / c)
    if(!sourcePath.isFile){ return None }

    // take the appropriate lines from the source file
    val sourceLines = File.read(sourcePath).split("\n")
      .slice(sourceRef.region.start.line, sourceRef.region.end.line + 1)

    if(sourceLines.length > 1){
      // we have > 1 source line
      val firstLine = sourceLines.headOption.map(_.drop(sourceRef.region.start.column))
      val middleLines = sourceLines.drop(1).dropRight(1).toList
      val lastLine = sourceLines.lastOption.map(l => l.dropRight(l.length - sourceRef.region.end.column))

      Some((firstLine :: middleLines :: (lastLine :: Nil)).mkString("\n"))
    } else {
      // we have exactly one line
      Some(sourceLines.head.slice(sourceRef.region.start.column, sourceRef.region.end.column + 1))
    }
  }

  /** builds a theory representation */
  private def buildTheory(theory: Theory): Option[ITheory] = {
    val ref = getTheoryRef(theory.path.toPath)
      .getOrElse(return buildFailure(theory.path.toPath, "getTheoryRef(theory.id)"))

    val meta = theory match {
      case dc: DeclaredTheory =>
        dc.meta.map(mt =>
          getTheoryRef(mt.toPath)
            .getOrElse(return buildFailure(theory.path.toPath, "getTheoryRef(theory.meta)")
            )
        )
      case _ => None
    }

    val presentation: String = getPresentationOf(theory)
    val source: Option[String] = getSourceOf(theory)

    Some(ITheory(
      ref.id, ref.name,
      getStats(ref.id),

      presentation,
      source,

      meta
    ))
  }

  /** builds a view representation */
  private def buildView(view: View): Option[IView] = {
    val ref = getViewRef(view.path.toPath)
      .getOrElse(return buildFailure(view.path.toPath, "getViewRef(view.id)"))

    val domain = getTheoryRef(view.from.toMPath.toPath)
      .getOrElse(return buildFailure(view.path.toPath, "getTheoryRef(view.domain)"))

    val codomain = getTheoryRef(view.to.toMPath.toPath)
      .getOrElse(return buildFailure(view.path.toPath, "getTheoryRef(view.codomain)"))

    val presentation: String = getPresentationOf(view)
    val source: Option[String] = getSourceOf(view)

    Some(IView(
      ref.id, ref.name,
      getStats(ref.id),

      presentation,
      source,

      domain,
      codomain
    ))
  }


  // endregion

  // region "Statistics"

  // TODO: Build statistics in a cached form
  private def getStats(path: String): Option[List[IStatistic]] = None

  // endregion
}
