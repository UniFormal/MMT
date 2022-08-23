package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.{IsTheory, Paths, SetResult}
import info.kwarc.mmt.api.utils.FilePath

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import info.kwarc.mmt.api.ontology.QueryResultConversion._

abstract class FullArchive {
  val archive : Archive
  require(archive.ns.isDefined)
  val path = archive.ns.get
  val name = archive.id
  def theories : List[MPath]

  def declares(p : Path) : Boolean
  def read : Unit
}

class ArchiveStore extends Extension {
  override def logPrefix = "ArchiveStore"

  private case class InternalFullArchive(archive : Archive) extends FullArchive {
    private var contains : List[MPath] = Nil
    private var isread = false
    private var readfoundation = false
    private var foundations : List[Theory] = Nil
    def theories = {
      if (!isread) read
      contains ::: foundations.map(_.path)
    }

    def read = {
      group {
        if (!readfoundation) readfounds
        log("Read relational " + name + "...")
        archive.readRelational(FilePath(""), controller, "rel")
        log("Loading Theories...")
        val theories = controller.evaluator(Paths(IsTheory)).asInstanceOf[SetResult].s.view.toList.map(s =>
          Path.parse(s.toString))
        logGroup {
          log("Namespace: " + path)
          contains = theories.filter(path <= _).collect {
            case t:MPath => t
          }.distinct
          log("Theories: ")
          logGroup {
            contains.foreach(t => log(" - " + t.toString.drop(archive.ns.get.toString.length)))
          }
        }
        isread = true
      }
    }

    def group[A](f : => A) : A = {
      log("Archive " + name)
      logGroup {f}
    }

    private def readfounds: Unit = {
      val fnd = Try(controller.get(archive.foundation.get).asInstanceOf[Theory]).map(Some(_)).getOrElse(None)
      log("Foundation: " + fnd.map(_.path.toString).getOrElse("None"))

      foundations = fnd.map(flattened).getOrElse(Nil).distinct
      /*
      try { fnd.foreach(controller.simplifier.apply(_)) } catch {
        case e : Exception =>
      }
      foundations = fnd.map(t => Success(t) :: t.getIncludes.map(p => Try(controller.get(p).asInstanceOf[DeclaredTheory]))).getOrElse(Nil).collect{
        case Success(t) => t
      }
      */
      if (fnd.isDefined) log("Implied: " + foundations.tail.map(_.name.toString).mkString(", "))
      readfoundation = true
    }

    def declares(p : Path) : Boolean = p match {
      case d:DPath =>
        path <= d
      case m : MPath =>
        path <= m || {
          if(!readfoundation) group { readfounds }
          foundations.exists(t => t.path == m)
        }
      case s : GlobalName =>
        declares(s.module)
      case e => throw ImplementationError("Invalid path: Not declared")
    }
  }


  private val stored : mutable.HashMap[String,FullArchive] =  mutable.HashMap()

  // partially mirrors alignmentfinder; TODO clean up

  private val flatteneds : mutable.HashMap[MPath,List[Theory]] = mutable.HashMap()

  private def flattened(th : Theory) : List[Theory] = flatteneds.getOrElse(th.path,{
    //Try(controller.simplifier.flatten(th))
    val theories = //th ::
      th.getIncludes.map(p => Try(controller.get(p).asInstanceOf[Theory]))
    val flats = theories.map({
      case Success(t) => Try(flattened(t))
      case Failure(e) => Failure(e)
    })
    val successes = flats.collect {
      case Success(t) => t
    }.flatten.distinct
    flatteneds += ((th.path,th :: successes))
    th :: successes
  })

  override def start(args: List[String]) = Try {
    val archs = controller.backend.getArchives
    log("Archives found: " + archs.map(_.id).mkString(","))
    log("Reading archives:")
    logGroup {
      archs foreach { a =>
        log(a.id + "...")
        logGroup {
          if (a.ns.isDefined) {
            log("Namespace: " + a.ns.get)
            stored += ((a.id, InternalFullArchive(a)))
          } else {
            log("No namespace given")
          }
        }
      }
    }
  }

  def getArchive(a : Archive) : Option[FullArchive] = stored.get(a.id)
  def getArchive(s : String) : Option[FullArchive] = stored.get(s)
  def getArchives : List[FullArchive] = stored.values.toList

  def find(c : Path) : List[FullArchive] = stored.filter(p => p._2.declares(c)).toList.map(_._2)
  def find(ls : List[GlobalName]) : List[FullArchive] = stored.filter(a => ls.forall(a._2.declares)).toList.map(_._2)
  def find(t : Term) : List[FullArchive] = find(ArchiveStore.getSymbols(t))
}

object ArchiveStore {
  def getSymbols(t : Term) : List[GlobalName] = {
    var symbols : List[GlobalName] = Nil
    object traverser extends StatelessTraverser {
      def traverse(t:Term)(implicit con : Context, init : State) = t match {
        case OMS(p) =>
          symbols ::= p
          t
        case _ => Traverser(this,t)
      }
    }
    traverser(t,())
    symbols
  }
}
