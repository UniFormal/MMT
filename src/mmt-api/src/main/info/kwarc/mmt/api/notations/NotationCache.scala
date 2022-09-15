package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import frontend._
import archives._
import utils._

import scala.concurrent.ExecutionContext.Implicits.global

/* TODO
 * this does not speed up presentation yet because notations are not the only reason why dependent theories are loaded:
 *  - HTMLPresenter and MathMLPresenter call strucutre simplifier
 *  - MathMLPresenter loads the aliases of every identifier it renders
 */

/* TODO we actually have to index all notations and then collect them into NotationContainer */

/** caches notations for objects that are not in memory by reading them when an archive is opened
 *  
 *  When loading an object into in Controller.add, cache entries are deleted so that future lookup are made via the libary
 */
class NotationCache extends ArchiveChangeListener {
  override val logPrefix = "notation-cache"
  
  private val cache = new scala.collection.mutable.HashMap[ContentPath,Option[TextNotation]]
  
  def get(c: ContentPath) = cache.get(c)
  
  def delete(c: ContentPath): Unit = {
    cache -= c
  }
  
  private val nsMap = NamespaceMap.empty
  def oncePerArchive(a: Archive): Unit = {
    scala.concurrent.Future {
      log("reading archive " + a.id)
      if ((a / notational).exists) {
        a.traverse(notational, FilePath(Nil), Archive.traverseIf("not"), sendLog = false) {case Current(inFile, inPath) =>
          File.ReadLineWise(inFile) {line =>
            try {
              val i = line.indexOf(" ")
              val (p,n) = if (i == -1) (line, null) else line.splitAt(i)
              val path = Path.parseMS(p, nsMap)
              val not = Option(n).map(s => TextNotation.parse(s, nsMap))
              cache(path) = not
            } catch {
              case e : Error => log(e.getMessage)
            }
          }
        }
      }
      log("done reading archive " + a.id)
    }
  }
}