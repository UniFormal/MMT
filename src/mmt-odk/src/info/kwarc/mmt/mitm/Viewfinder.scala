package info.kwarc.mmt.mitm

import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.time.Time
import info.kwarc.mmt.lf._

import scala.collection.mutable

class Viewfinder extends Extension {
  override val logPrefix = "viewfinding"

  private val theories : mutable.HashMap[String,(List[DeclaredTheory],Option[GlobalName])] = mutable.HashMap()

  lazy val alignmentFinder : AlignmentFinder = controller.extman.get(classOf[AlignmentFinder]).headOption.getOrElse {
    val af = new AlignmentFinder
    controller.extman.addExtension(af)
    af
  }

  // lazy val mitm : Archive = controller.backend.getArchive("MitM/smglom").getOrElse(???)

  override def start(args: List[String]): Unit = {
    val as = List("MitM/Foundation","MitM/smglom","MMT/LFX","PVS/Prelude","HOLLight/basic" /*,"PVS/NASA" */).map(controller.backend.getArchive(_).get) //controller.backend.getArchives
    log("Getting Archives...")
    val (t,_) = Time.measure {
      as.filterNot(_.id.startsWith("ODK")).foreach(a => try { a.id match {
        case s if s.startsWith("MitM") =>
          val (ths,judg) = alignmentFinder.getArchive(a)
          theories(a.id) = (ths.map(mitmpreproc.apply),judg)
        case s if s.startsWith("PVS") =>
          val (ths,judg) = alignmentFinder.getArchive(a)
          theories(a.id) = (ths.map(ParameterPreprocessor.apply),judg)
        case _ => theories(a.id) = alignmentFinder.getArchive(a)
      }
      } catch {
        case e: api.Error => log(e.shortMsg)
      })
    }
    log("Finished after " + t)
  }

  def find(mp : MPath, to : String) = {
    val hasher = alignmentFinder.getHasher
    val froms = alignmentFinder.getFlat(List(mp)).map(ParameterPreprocessor.apply) // TODO!
    val meta = froms.view.find(_.path == mp).get.meta.get
    val metas = alignmentFinder.getFlat(List(meta))
    val (tos,judg2) = theories(to)
    val judg1 = alignmentFinder.getJudgment(froms.map(_.path))
    val commons = tos.filter(t => metas.exists(_.path == t.path))
    judg1.foreach(hasher.cfg.addJudgmentFrom)
    judg2.foreach(hasher.cfg.addJudgmentTo)
    val (t,_) = Time.measure {
      log("Commons")
      commons.indices.foreach({i =>
        print("\r" + (i+1) + " of " + commons.length)
        hasher.add(commons(i), Hasher.COMMON)
      })
      println("")
      log("Froms")
      val nfrom = froms.filterNot(t => commons.exists(_.path == t.path))
      nfrom.indices.foreach({ i =>
        print("\r" + (i+1) + " of " + nfrom.length)
        hasher.add(nfrom(i), Hasher.FROM)
      })
      println("")
      log("Tos")
      val ntos = tos.filterNot(commons.contains)
      ntos.indices.foreach({ i =>
        print("\r" + (i+1) + " of " + ntos.length)
        hasher.add(ntos(i), Hasher.TO)
      })
    }
    println("")
    log("Hashing done after " + t)
    val proc = new FindingProcess(this.report,hasher)
    val ret = proc.run(from = List(hasher.get(mp).get))
    val (t1,ret1) = Time.measure {
      log("Making views...")
      proc.makeviews(Path.parseM("http://test.test/test?test",NamespaceMap.empty),ret)
    }
    log("Done after " + t1 + " - " + ret1.length + " Views found")
    ret1
  }

  /*

  def find(mp : MPath, to : Archive) = {
    val hasher = default(mp, to) // TODO
    val previous = hasher.common ::: hasher.from.map(_.path)
    val news = alignmentFinder.getFlat(List(mp)).filterNot(p => previous.contains(p.path))
    news foreach (t => hasher.add(t,Hasher.FROM))
    val proc = new FindingProcess(this.report,hasher)
    proc.run(from = List(hasher.get(mp).get))
  }

  private def default(mp : MPath,to : Archive) = {
    val hasher = alignmentFinder.getHasher
    hasher.cfg.setDoDefs(false)
    hasher.cfg.setMultithreaded(false)
    // alignmentFinder.addArchives(mitm,to,hasher)
    hasher
  }

  */

  // val key = "viewfinding"
  // override val outExt = "viewfinding"

  def exportTheory(t: DeclaredTheory, bf: BuildTask) { /*
    rh("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    rh("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
    t.getDeclarations foreach {d =>
      d.getComponents.foreach {
        case DeclarationComponent(comp, tc: AbstractTermContainer) =>
          tc.get.foreach {t =>
            val node = <mws:expr url={CPath(d.path,comp).toPath}>{t.toCML}</mws:expr>
            rh(node.toString + "\n")
          }
        case _ =>
      }
    }
    rh("</mws:harvest>\n") */
  }

  def exportView(v: DeclaredView, bf: BuildTask) {
    //excluding expressions from views for now
  }


  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
    //Nothing to do - MathML in namespaces
  }

  def exportDocument(doc : Document, bt: BuildTask) {
    //Nothing to do - no MathML at document level
  }

  val mitmpreproc = ParameterPreprocessor + new LFClassicHOLPreprocessor(
    ded = MitM.ded,
    and = MitM.and,
    not = MitM.not,
    or = Some(MitM.or),
    implies = Some(MitM.implies),
    equiv = Some(MitM.equiv),
    forall = Some(MitM.forall),
    exists = Some(MitM.exists)
  )

}

