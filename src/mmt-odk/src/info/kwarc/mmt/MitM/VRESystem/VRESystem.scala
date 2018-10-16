package info.kwarc.mmt.MitM.VRESystem

import info.kwarc.mmt.MitM.Config.MitMSystemLocation
import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.QueryEvaluator.QuerySubstitution
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.refactoring._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.Plugin

import scala.util.Try

/** a single VRESystem */
abstract class VRESystem(val id : String, val sym : GlobalName) extends QueryExtension(id) {
  override val logPrefix: String = id

  /** the odk plugin, so that we can use it */
  protected lazy val odkPlugin: Plugin = controller.extman.get(classOf[Plugin]).head

  /** loads up the cache used by this VRESystem */
  def warmup(): Unit

  /** calls the system using a given term */
  def call(t : Term) : Term

  /** evaluates a given query inside of this system */
  def evaluate(q: Query, e: QueryEvaluator)(implicit substitution: QuerySubstitution): scala.collection.mutable.HashSet[List[BaseType]] = {
    // evaluate the qiery normally
    val result = e.evalSet(q)

    // and return the map
    result.map({
      case List(t: Term) => List(call(t))
      case _ => throw ImplementationError("Failed to evaluate Query with VRE")
    })
  }
}

abstract class VREWithAlignmentAndSCSCP(id : String, sym : GlobalName, val head: OMSymbol, archiveId : String)
  extends VRESystem(id, sym) with AlignmentBasedMitMTranslation with UsesSCSCP {
  def location: MitMSystemLocation = odkPlugin.config.get(id)

  lazy val archive: Archive = controller.backend.getArchive(archiveId).getOrElse(throw GeneralError(s"Missing archive $archiveId"))
  def call(t : Term): Term = translateToMitM(scscpcall(translateToSystem(t)))
}

object Translations {
  val lftoOMA = new AcrossLibraryTranslation {
    override def applicable(tm: Term)(implicit translator: AcrossLibraryTranslator): Boolean = tm match {
      case ApplySpine(f,ls) =>
        true
      case _ => false
    }

    override def apply(tm: Term)(implicit translator: AcrossLibraryTranslator): Term = tm match {
      case ApplySpine(f,ls) =>
        OMA(f,ls)
    }
  }
}

trait AlignmentBasedMitMTranslation { this : VRESystem =>

  /** the mitm/smglom archive */
  lazy val mitm : Archive = controller.backend.getArchive("MitM/smglom").getOrElse(throw GeneralError("Missing archive MiTM/smglom"))
  /** the archive belonging to this system */
  val archive : Archive

  def complexTranslations : List[AcrossLibraryTranslation] = Nil
  def toTranslations : List[AcrossLibraryTranslation] = Translations.lftoOMA :: Nil
  def fromTranslations : List[AcrossLibraryTranslation] = Nil

  lazy protected val alignmentserver: AlignmentsServer = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  private lazy val links : List[DeclaredLink] = (archive.allContent ::: mitm.allContent).map(p => Try(controller.get(p)).toOption).collect {
    case Some(th : DeclaredTheory) => th.getNamedStructures collect {
      case s : DeclaredStructure => s
    }
    case Some(v : DeclaredView) => List(v)
  }.flatten

  private def translator(to : Archive,trls : List[AcrossLibraryTranslation]) = {
    val aligns = alignmentserver.getAll.collect {
      case fa : FormalAlignment if fa.props.contains(("type","VRE" + this.id)) => AlignmentTranslation(fa)(controller)
    }
    val linktrs : List[TranslationGroup]= links.map(l => LinkTranslation(l))

    new AcrossLibraryTranslator(controller,aligns ::: complexTranslations ::: trls,linktrs,to)
  }

  private lazy val translator_to = translator(archive,toTranslations)
  private lazy val translator_from = translator(mitm,fromTranslations)

  def translateToSystem(t : Term) : Term = {
    val (res,succ) = translator_to.translate(t)
    succ.foreach(s => throw BackendError("could not translate symbol",s))
    res
  }
  def translateToMitM(t : Term) : Term = {
    val (res,succ) = translator_from.translate(t)
    succ.foreach(s => throw BackendError("could not translate symbol", s))
    res
  }

  def warmup(): Unit = {
    // initialize the lazy vals
    alignmentserver
    translator_to
    translator_from
    print("")
  }
}



