package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import utils._
import frontend._
import web._

import scala.collection.mutable.{HashMap}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** an [[Error]] as reconstructed from an error file */
case class BuildError(archive: String, target: String, path: List[String], tp: String, level: Level.Level,
                      sourceRef: parser.SourceRef, shortMsg: String, longMsg: String, stackTrace: String)

/**
 * maintains the errors of an archive, mapping (target,path) to error list
 */
class ErrorMap(val archive: Archive) extends HashMap[(String,List[String]), List[BuildError]]

/**
 * maintains all errors produced while running [[BuildTarget]]s on [[Archive]]s
 */
class ErrorManager extends Extension {
   
   /** the mutable data: one ErrorMap per open Archive */
   private var errorMaps: List[ErrorMap] = Nil

   /**
    * @param id the archive
    * @return the [[ErrorMap]] of the archive
    */
   def apply(id: String): ErrorMap =
      errorMaps.find(_.archive.id == id).getOrElse(throw GeneralError("archive does not exist: " + id))
      
   /**
    * @param a the archive
    * load all errors of this archive
    */
   def loadAllErrors(a: Archive) {
      a.traverse(errors, Nil, _ => true, false) {case Current(_, target :: path) =>
         loadErrors(a, target, path)
      }
   }
   
   /**
    * @param a the archive
    * @param target the build target
    * @param path the file
    * load all errors of a build target applied to a file
    */
   def loadErrors(a: Archive, target: String, path: List[String]) {
      val node = xml.readFile(a/errors/target/path)
      var bes : List[BuildError] = Nil
      node.child.foreach {child =>
         // TODO parse child
         val be = BuildError(a.id, target, path, ???, ???, ???, ???, ???, ???)
         bes ::= be
      }
      val em = apply(a.id)
      em((target,path)) = bes.reverse
   }
   
   /** iterator over all errors given as (archive, target, path, error) */
   def iterator: Iterator[BuildError] = {
      errorMaps.iterator.flatMap {em =>
         em.values.iterator.flatten
      }
   }

   /** registers a [[ChangeListener]] and a [[ServerExtension]] */
   override def start(args: List[String]) {
      controller.extman.addExtension(cl)
      controller.extman.addExtension(serve)
   }
   
   /** adds/deletes [[ErrorMap]]s for each opened/closed [[Archive]] */
   private val cl = new ChangeListener {
      /** creates an [[ErrorMap]] for the archive and asynchronously loads its errors */
      override def onArchiveOpen(a: Archive) {
         errorMaps ::= new ErrorMap(a)
         Future {
            loadAllErrors(a)
         }
      }
      /** deletes the [[ErrorMap]] */
      override def onArchiveClose(a: Archive) {
         errorMaps = errorMaps.filter(_.archive != a)
      }
   }
   
   /** serves lists of [[Error]]s */
   private val serve = new ServerExtension("errors") {
      def apply(path: List[String], query: String, body: Body) = {
         val wq = WebQuery.parse(query)
         val result = iterator.filter {be =>
            true // TODO select BuildErrors according to query 
         }
         Server.XmlResponse(<todo/>)
      }
   }
}