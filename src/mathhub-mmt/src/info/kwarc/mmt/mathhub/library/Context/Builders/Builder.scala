package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.{GeneralError, Path}
import info.kwarc.mmt.api.archives.lmh.MathHub
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IReferencable, IReference, IStatistic}

import scala.collection.mutable

trait Builder
  extends Logger
    with Cache
    with ObjectGetters
    with ReferenceGetters
    with Statistics
    with GroupsBuilder
    with GroupBuilder
    with TagBuilder
    with ArchiveBuilder
    with NarrativeWrap
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

  protected def mathHub: MathHub = controller.getMathHub.getOrElse(throw GeneralError("No MathHub configured"))

  /** builds a reference to an item, if it exists in the controller */
  protected def buildReference(id: String): Option[IReference] = logGroup {

    // try to build a group reference
    tryGroup(id).map(e => return buildGroupReference(e))

    // try to build a tag reference
    tryTag(id).map(e => return buildTagReference(e))

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

      case s: Structure => buildStructureReference(s)
      case c: Constant => buildConstantReference(c)
      case rc: RuleConstant => buildRuleReference(rc)
      case nm: NestedModule => buildNestedModuleReference(nm)

      case opaque: OpaqueElement => buildOpaqueReference(opaque)
      case document: Document => buildDocumentReference(document)

      case _ => buildFailure(id, "controller.get(ref.path) match")
    }
  }

  /** builds a reference to an object, if it exists in the controller */
  protected def buildObject(id: String): Option[IReferencable] = logGroup {

    // try to build a group reference
    tryGroup(id).map(e => return buildGroup(e))

    // try to build a tag reference
    tryTag(id).map(e => return buildTag(e))

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

      case s: Structure => buildStructure(s)
      case c: Constant => buildConstant(c)
      case rc: RuleConstant => buildRule(rc)
      case nm: NestedModule => buildNestedModule(nm)

      case opaque: OpaqueElement => buildOpaque(opaque)
      case document: Document => buildDocument(document)

      case _ => buildFailure(id, "controller.get(object.path) match")
    }
  }

  /** indicates that a failure has occured during then build */
  protected def buildFailure(uri: String, during: String): Option[Nothing] ={
    log(s"failed to build $uri: $during failed ")
    None
  }

}


trait Cache {
  /** a map of ids to references to objects */
  protected val refCache = mutable.Map[String, IReference]()

  /** a map of ids to objects */
  protected val objCache = mutable.Map[String, IReferencable]()

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
}

trait ObjectGetters { this: Builder =>
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
  protected def getObjectOf[T <: IReferencable](cls: Class[T], id: String): Option[T] = logGroup {
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
}

trait ReferenceGetters { this: Builder =>
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


  /** gets a reference of a specific type */
  protected def getReferenceOf[T <: IReference](cls: Class[T], id: String): Option[T] = logGroup {
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
}

trait Statistics { this: Builder =>
  // TODO: Build statistics in a cached form
  protected def getStats(path: String): Option[List[IStatistic]] = None

}