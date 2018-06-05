package info.kwarc.mmt.mathhub

import info.kwarc.mmt.api.{GeneralError, Path}
import info.kwarc.mmt.api.archives.lmh.{LMHHubGroupEntry, _}
import info.kwarc.mmt.api.documents.{Document, MRef, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView, Theory, View}
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.utils.{File, URI}

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
  private def getReferenceOf[T <: IReference](cls: Class[T], id: String): Option[T] = {
    log(s"trying to get reference $id")
    getReference(id) match {
      case Some(t: T@unchecked) if cls.isInstance(t) => Some(t)
      case _ => None
    }
  }

  /** gets a reference to a group */
  def getGroupRef(id: String): Option[IGroupRef] = getReferenceOf(classOf[IGroupRef], id)

  /** gets a reference to an archive */
  def getArchiveRef(id: String): Option[IArchiveRef] = getReferenceOf(classOf[IArchiveRef], id)

  /** gets a reference to a narrative parent */
  def getNarrativeParentRef(id: String): Option[INarrativeParentRef] = {
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
  private def getObjectOf[T <: IReferencable](cls: Class[T], id: String): Option[T] = {
    log(s"trying to get object $id")
    getObject(id) match {
      case Some(t: T@unchecked) if cls.isInstance(t) => Some(t)
      case _ => None
    }
  }

  /** gets a group */
  def getGroup(id: String): Option[IGroup] = getObjectOf(classOf[IGroup], id)

  /** gets an archive */
  def getArchive(id: String): Option[IArchive] = getObjectOf(classOf[IArchive], id)

  /** gets a narrative element */
  def getNarrativeElement(id: String): Option[INarrativeElement] = {
    // IOpaqueElement | IDocument | IModuleRef
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
    mathHub.entries_.collectFirst({
      case e: LMHHubGroupEntry if e.group == id => e
    })
  }

  /** tries to find an archive with a given id */
  private def tryArchive(id: String) : Option[LMHHubArchiveEntry] = {
    mathHub.entries_.collectFirst({
      case e: LMHHubArchiveEntry if e.group == id => e
    })
  }

  /** builds a reference to an item, if it exists in the controller */
  private def buildReference(id: String): Option[IReference] = {

    // try to build a group reference
    tryGroup(id).map(e => return buildGroupReference(e))

    // try to build an archive reference
    tryArchive(id).map(e => return buildArchiveReference(e))


    // try to parse the path or fail
    val path = try{
      Path.parse(id, controller.getNamespaceMap)
    } catch {
      case _: Exception => return buildFailure(id, "Path.parse(id)")
    }

    // try to get the path or

    controller.getO(path).getOrElse(return buildFailure(id, "controller.getO(path)")) match {
      case view: DeclaredView => buildViewReference(view)
      case theory: DeclaredTheory => buildTheoryReference(theory)

      case opaque: OpaqueElement => buildOpaqueReference(opaque)
      case document: Document => buildDocumentReference(document)

      case _ => buildFailure(id, "controller.get(path) match")
    }
  }

  /** helper function to resolve the narrative parent element reference */
  private def buildGetNarrativeParentRef(path: Path): Option[INarrativeParentRef] = {
    // try to find an archive with the narration root of the path in question
    mathHub.entries_.find({
      case ae: LMHHubArchiveEntry => ae.archive.narrationBase == URI(path.toPath)
      case _ => false
    }) match {
      case Some(ae) => getArchiveRef(ae.id)
      // fallback to a document
      case None => getDocumentRef(path.toPath)
    }
  }

  /** builds a reference to a group */
  private def buildGroupReference(entry: LMHHubGroupEntry) : Option[IGroupRef] = Some(
    IGroupRef(
      entry.id, /* id */
      entry.id, /* name */
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
  private def buildDocumentReference(document: Document) : Option[IDocumentRef] = Some(
    IDocumentRef(
      buildGetNarrativeParentRef(document.parent).map(Some(_)).getOrElse(return buildFailure(document.path.toPath, "document.parent")),/* parent */
      document.path.toPath, /* id */
      document.name.toString /* name */
    )
  )

  /** builds a reference to an opaque element */
  private def buildOpaqueReference(opaque: OpaqueElement) : Option[IOpaqueElementRef] = Some(
    IOpaqueElementRef(
      buildGetNarrativeParentRef(opaque.parent).map(Some(_)).getOrElse(return buildFailure(opaque.path.toPath, "opaque.parent")),/* parent */
      opaque.path.toPath, /* id */
      opaque.name.toString /* name */
    )
  )

  /** builds a reference to a view */
  private def buildViewReference(view: DeclaredView) : Option[IViewRef] = Some(
    IViewRef(
      buildGetNarrativeParentRef(view.parent).map(Some(_)).getOrElse(return buildFailure(view.path.toPath, "view.parent")), /* parent */
      view.path.toPath, /* id */
      view.name.toString /* name */
    )
  )

  /** builds a reference to a theory */
  private def buildTheoryReference(theory: DeclaredTheory) : Option[ITheoryRef] = Some(
    ITheoryRef(
      buildGetNarrativeParentRef(theory.parent).map(Some(_)).getOrElse(return buildFailure(theory.path.toPath, "theory.parent")), /* parent */
      theory.path.toPath, /* id */
      theory.name.toString /* name */
    )
  )


  /** builds a reference to an object, if it exists in the controller */
  private def buildObject(id: String): Option[IReferencable] = {

    // try to build a group reference
    tryGroup(id).map(e => return buildGroup(e))

    // try to build an archive reference
    tryArchive(id).map(e => return buildArchive(e))


    // try to parse the path or fail
    val path = try{
      Path.parse(id, controller.getNamespaceMap)
    } catch {
      case _: Exception => return buildFailure(id, "Path.parse(id)")
    }

    // try to get the path or

    controller.getO(path).getOrElse(return buildFailure(id, "controller.getO(path)")) match {
      case view: DeclaredView => buildView(view)
      case theory: DeclaredTheory => buildTheory(theory)

      case opaque: OpaqueElement => buildOpaque(opaque)
      case document: Document => buildDocument(document)

      case _ => buildFailure(id, "controller.get(path) match")
    }
  }

  /** builds a group representation */
  private def buildGroup(entry: LMHHubGroupEntry): Option[IGroup] = {
    val ref = getGroupRef(entry.id).getOrElse(return  buildFailure(entry.id, "getGroupRef(id)"))

    // get the description file
    val file = entry.root / entry.properties.getOrElse("description", "desc.html")
    val description = if(entry.root <= file && file.exists()) {
      File.read(file)
    } else { "No description provided" }

    val responsible = entry.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

    val archives = mathHub.entries_.collect({case archive: LMHHubArchiveEntry if archive.group == ref.id => archive.id })

    Some(IGroup(
      ref.id, ref.name, ref.title, ref.teaser,

      description,
      responsible,
      archives.flatMap(getArchiveRef)
    ))
  }

  /** builds an archive representation */
  private def buildArchive(entry: LMHHubArchiveEntry): Option[IArchive] = {
    val ref = getArchiveRef(entry.group).getOrElse(return buildFailure(entry.id, "getArchiveRef(id)"))

    // get the description file
    val file = entry.root / entry.archive.properties.getOrElse("description", "desc.html")
    val description = if(entry.root <= file && file.exists()) {
      File.read(file)
    } else { "No description provided" }

    val responsible = entry.archive.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

    val narrativeRoot = getDocument(entry.archive.narrationBase.toString)
      .getOrElse(return buildFailure(entry.id, "getDocument(archive.narrativeRoot)")) // TODO: Check if this works

    Some(IArchive(
      ref.parent, ref.id, ref.name, ref.title, ref.teaser,

      description,
      responsible,

      narrativeRoot
    ))
  }

  /** builds a document representation */
  private def buildDocument(document: Document): Option[IDocument] = {
    val ref = getDocumentRef(document.path.toPath)
      .getOrElse(return buildFailure(document.path.toPath, "getDocumentRef(id)"))

    // find all the declarations
    val decls = document.getDeclarations.flatMap({

      // IOpaqueElement
      case o: OpaqueElement => getOpaqueElement(o.path.toPath).map(Some(_))
          .getOrElse(return buildFailure(document.path.toPath, s"getOpaqueElement(declaration[${o.path.toPath}])"))

      // IDocument
      case d: Document => getDocument(d.path.toPath).map(Some(_))
          .getOrElse(return buildFailure(document.path.toPath, s"getOpaqueElement(declaration[${d.path.toPath}])"))

      // IModuleRef
      case m: MRef => getModuleRef(m.target.toPath).map(Some(_))
          .getOrElse(return buildFailure(document.path.toPath, s"getOpaqueElement(declaration[${m.target.toPath}])"))


      // TODO: Implement URIs
      case d => {
        log(s"buildDocument: ignoring unknown child ${d.path.toPath} of ${document.path.toPath} (not OpaqueElement, Document or MRef)")
        None
      }
    })

    Some(IDocument(
      ref.parent, ref.id, ref.name,

      decls
    ))
  }

  /** builds an opaque representation */
  private def buildOpaque(opaque: OpaqueElement): Option[IOpaqueElement] = {
    val ref = getOpaqueElementRef(opaque.path.toPath)
      .getOrElse(return buildFailure(opaque.path.toPath, "getOpaqueElementRef(id)"))

    Some(IOpaqueElement(
      ref.parent, ref.id, ref.name,

      opaque.format,
      opaque.raw.toString
    ))
  }

  /** builds a theory representation */
  private def buildTheory(theory: DeclaredTheory): Option[ITheory] = {
    val ref = getTheoryRef(theory.path.toPath)
      .getOrElse(return buildFailure(theory.path.toPath, "getTheoryRef(id)"))

    val meta = theory.meta.map(mt =>
      getTheoryRef(mt.toPath)
        .getOrElse(return buildFailure(theory.path.toPath, "getTheoryRef(theory.meta)")
        )
    )

    val presentation: String = "" // TODO: html presentation of `theory`
    val source: Option[String] = None // TODO: source code of `theory` if any

    Some(ITheory(
      ref.parent, ref.id, ref.name,

      presentation,
      source,

      meta
    ))
  }

  /** builds a view representation */
  private def buildView(view: DeclaredView): Option[IView] = {
    val ref = getViewRef(view.path.toPath)
      .getOrElse(return buildFailure(view.path.toPath, "getViewRef(id)"))

    val domain = view.fromC.get.getOrElse("view.get(domain)") match {
      case OMS(p: Path) => getTheoryRef(p.toPath).getOrElse(return buildFailure(view.path.toPath, "getTheoryRef(view.domain)"))
      case None => return buildFailure(view.path.toPath, "view.domain match")
    }

    val codomain = view.toC.get.getOrElse("view.get(codomain)") match {
      case OMS(p: Path) => getTheoryRef(p.toPath).getOrElse(return buildFailure(view.path.toPath, "getTheoryRef(view.codomain)"))
      case None => return buildFailure(view.path.toPath, "view.codomain match")
    }

    val presentation: String = "" // TODO: html presentation of `view`
    val source: Option[String] = None // TODO: source code of `view` if any

    Some(IView(
      ref.parent, ref.id, ref.name,

      presentation,
      source,

      domain,
      codomain
    ))
  }


  // endregion
}
