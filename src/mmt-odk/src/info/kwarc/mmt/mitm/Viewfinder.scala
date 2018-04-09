package info.kwarc.mmt.mitm

import info.kwarc.mmt.api.{DPath, GlobalName, MPath}
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.refactoring._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.utils.FilePath
import info.kwarc.mmt.lf.ApplySpine

class Viewfinder extends Exporter {

  lazy val alignmentFinder : AlignmentFinder = controller.extman.get(classOf[AlignmentFinder]).headOption.getOrElse {
    val af = new AlignmentFinder
    controller.extman.addExtension(af)
    af
  }

  lazy val mitm : Archive = controller.backend.getArchive("MitM/smglom").getOrElse(???)

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
    alignmentFinder.addArchives(mitm,to,hasher)
    hasher
  }

  val key = "viewfinding"
  override val outExt = "viewfinding"

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

}

class LFClassicHOLPreprocessor(ded : GlobalName, and : GlobalName, not : GlobalName,
                        or : Option[GlobalName],
                        implies : Option[GlobalName],
                        equiv : Option[GlobalName],
                        equals : Option[GlobalName],
                        forall : Option[GlobalName],
                        exists : Option[GlobalName],
                        hoasapply : Option[GlobalName]
                       ) extends Preprocessor {

  private object Apply {
    def apply(f : Term, args : List[Term]) = if (hoasapply.isDefined) {
      args.foldLeft(f)((nf,a) => ApplySpine(OMS(hoasapply.get), nf,a))
    } else ApplySpine(f,args :_*)
    def unapply(tm : Term) : Option[(Term,List[Term])] = if (hoasapply.isDefined) {
      val hoas = hoasapply.get
      tm match {
        case ApplySpine(OMS(`hoas`),List(nf,a)) => Some(unapply(nf).map(p => (p._1, p._2 ::: a :: Nil)).getOrElse((nf,List(a))))
      }
    } else ApplySpine.unapply(tm)
  }

  private object Ded {
    ???
  }

  private object And {
    ???
  }

  private object Not {
    ???
  }

  private object Or {
    ???
  }

  private object Implies {
    ???
  }

  private object Equiv {
    ???
  }

  private object Equals {
    ???
  }

  private object Forall {
    ???
  }

  private object Exists {
    ???
  }

  def apply(c : FinalConstant): FinalConstant = ???

  private def leq(t1 : Term, t2 : Term) : Boolean = t1.subobjects.length <= t2.subobjects.length
}