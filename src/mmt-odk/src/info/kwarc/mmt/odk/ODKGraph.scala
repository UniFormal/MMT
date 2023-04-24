package info.kwarc.mmt.odk

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils.{DotEdge, DotGraph, DotNode, URI}

/**
  * Created by jazzpirate on 19.04.17.
  */
class ODKGraph extends RelationGraphExporter {
  val key = "odkgraph"
  override val logPrefix = "odkgraph"
  val description = "ODK graph"

  private lazy val tg: ontology.TheoryGraph = new ontology.TheoryGraph(controller.depstore)

  val mitm = DPath(URI.http colon "mathhub.info") / "MitM"
  val gap = DPath(URI.http colon "www.gap-system.org")
  val lmfdb = DPath(URI.http colon "www.lmfdb.org")
  val sage = DPath(URI.http colon "www.sagemath.org")

  lazy val alignmentserver = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  def alignments = alignmentserver.getAll.collect {
    case fa : FormalAlignment if gap <= fa.from.mmturi || lmfdb <= fa.from.mmturi || sage <= fa.from.mmturi
      => (fa.from.mmturi.module,fa.to.mmturi.module)
  }.distinct

  private def odkpath(p : Path) = mitm <= p || gap <= p || lmfdb <= p || sage <= p

  private def alltheories = {
    log("Loading theories...")
    val ret = (controller.depstore.getInds(IsTheory) collect {
      case mp: MPath => mp
    }).toList
    log("Done.")
    ret
  }
  private def allviews = {
    log("Loading views...")
    val ret = (controller.depstore.getInds(IsView) collect {
      case mp : MPath => mp
    }).toList
    log("Done.")
    ret
  }

  def buildGraph(se: StructuralElement): DotGraph = {
    log("Doing ODKGraph. Theories/Views...")
    val (theories,views) = (alltheories.filter(odkpath),allviews.filter(odkpath))
    log("theory graph...")
    val tgf = new ontology.TheoryGraphFragment(theories, views, tg, report)
    val basedot = tgf.toDot
    log("Eliminating Metas and adding Alignments...")
    val nedges : Iterable[DotEdge] = basedot.edges.filterNot(_.cls == "graphmeta").toList ::: alignments.flatMap(pq => try {
      Some(new DotEdge {
        val from: DotNode = basedot.nodes.find(_.id == pq._1).getOrElse {
          log("Node " + pq._1 + " not found")
          throw GeneralError("Node " + pq._1 + " not found")
        }
        val to: DotNode = basedot.nodes.find(_.id == pq._2).getOrElse {
          log("Node " + pq._2 + " not found")
          throw GeneralError("Node " + pq._2 + " not found")
        }
        val id = None
        val label = None
        val cls = "alignment"
      })
    } catch {
      case t : GeneralError => None
    })
    log("Building DotGraph...")
    new DotGraph {
      val nodes = basedot.nodes
      val edges: Iterable[DotEdge] = nedges
      val title = "ODK Graph"
      val externalNodes: Option[Iterable[DotNode]] = None
    }
  }

}
