package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.frontend.ChangeListener
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.ontology.QueryEvaluator.QuerySubstitution
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.refactoring.{AcrossLibraryTranslation, AcrossLibraryTranslator, AlignmentTranslation, LinkTranslation}
import info.kwarc.mmt.api.symbols._
import objects._
import uom._
import utils._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mitm.MitM
import info.kwarc.mmt.odk.OpenMath.{CodingServer, OMInteger, OMString}
import info.kwarc.mmt.odk.SCSCP.Server.MitMServer
import info.kwarc.mmt.odk.Sage.Sage
import info.kwarc.mmt.odk.Singular.SingularImporter
import info.kwarc.mmt.sequences.Sequences

import scala.util.Try

class Plugin extends ChangeListener {
  val theory = MitM.mathpath
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new LMFDB.Plugin)
    controller.extman.addExtension(new GAP.Plugin)
    controller.extman.addExtension(new Sage.Plugin)
    controller.extman.addExtension(new SingularImporter)
    controller.extman.addExtension(new activecomp.Plugin)
    controller.extman.addExtension(new ODKGraph)
    controller.extman.addExtension(new MitMServer)
    controller.extman.addExtension(new UniverseInference)
    controller.extman.addExtension(new CodingServer)
    controller.extman.addExtension(new SubtypeGenerator)
    controller.extman.addExtension(MitM.preproc)
  }

  def callVRE(t : Term, system : GlobalName) = {
    val systems = controller.extman.get(classOf[VRESystem])
    systems.find(_.sym == system) match {
      case Some(s) => s.call(t)
      case _ => ???
    }
  }

  def simplify(tm : Term, conO : Option[Context]) = {
    val con = conO.getOrElse {
      controller.getTheory(MitM.mathpath).getInnerContext
    }
    val rs = RuleSet.collectRules(controller,con)
    rs.add(getRule(con))
    controller.simplifier.apply(tm,con,rs,expDef=true)
  }

  def getRule(con : Context) : ComputationRule = new ComputationRule(Systems.evalSymbol) {
    override def alternativeHeads: List[GlobalName] = List(Apply.path)

    override def applicable(tm: Term): Boolean = tm match {
      case OMA(OMS(Systems.evalSymbol),List(OMS(_),_)) | ApplySpine(OMS(Systems.evalSymbol),List(OMS(_),_)) => true
      case _ => false
    }

    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      val (sys, subtm) = tm match {
        case OMA(OMS(Systems.evalSymbol),List(OMS(s),t)) => (s,t)
        case ApplySpine(OMS(Systems.evalSymbol),List(OMS(s),t)) => (s,t)
        case _ => return Simplifiability.NoRecurse
      }
      Simplify(callVRE(simplify(subtm,Some(con)),sys))
    }
  }


}

object Systems {
  val _basepath = DPath(URI("http","opendreamkit.org"))
  val vretheory = _basepath ? "Systems"

  val evalSymbol = vretheory ? "Eval"

  val gapsym = vretheory ? "GAPEval"
  val sagesym = vretheory ? "SageEval"
  val singularsym = vretheory ? "SingularEval"
  val lmfdbsym = vretheory ? "LMFDBEval"
}


abstract class VRESystem(val id : String, val sym : GlobalName) extends QueryExtension(id) {
  override val logPrefix = id

  /** has this system evaluate t and returns the result as an MitM expression **/
  def call(t : Term) : Term

  def evaluate(q: Query, e: QueryEvaluator)(implicit substiution: QuerySubstitution): scala.collection.mutable.HashSet[List[BaseType]] = {
    // evaluate the qiery normally
    val result = e.evalSet(q)

    // and return the map
    result.map({
      case List(t: Term) => List(call(t))
      case _ => throw ImplementationError("Failed to evaluate Query with VRE")
    })
  }
}

class VREWithAlignmentAndSCSCP(id : String, sym : GlobalName, archiveId : String, val serverurl : String, override val port : Int = 26133)
  extends VRESystem(id,sym) with AlignmentBasedMitMTranslation with UsesSCSCP {
  lazy val archive = controller.backend.getArchive(archiveId).getOrElse(???)

  def call(t : Term) = translateToMitM(scscpcall(translateToSystem(t)))
}

trait AlignmentBasedMitMTranslation { this : VRESystem =>
  val archive : Archive
  lazy val mitm : Archive = controller.backend.getArchive("MitM/smglom").getOrElse( ??? )

  val complexTranslations : List[AcrossLibraryTranslation] = Nil

  lazy protected val alignmentserver = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }
/*
  val trgract = (DPath(URI.http colon "mathhub.info") / "MitM" / "smglom" / "algebra" / "permutationgroup") ? "transitive_group_action"
  val transitivegrouprec =  trgract ? "from_record"
  val transitivegroupcons = trgract ? "transitive_group"

  private val mitmToSystem = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case ApplySpine(OMS(`transitivegrouprec`),List(LFX.RecExp(ls))) => // TODO implement in general
        val tr = Try(Traverser(this,OMA(OMS(transitivegroupcons),List(ls.fields.find(_.name == LocalName("n")).get.df.get,ls.fields.find(_.name == LocalName("t")).get.df.get))))
        tr.getOrElse(t)
      case ApplySpine(fun,args) => Traverser(this,OMA(fun,args))
      case OMS(pth) if VRESystem.MitM <= pth =>
        val trg = alignmentserver.getFormalAlignments(pth).filter(_.props contains (("type","VRE"))).collect{
          case a if namespace <= a.to.mmturi => a.to.mmturi
        }
        trg.headOption match {
          case Some(gn : GlobalName) => OMS(gn)
          case _ => throw GeneralError("No alignment to " + t)
        }
      case _ => Traverser(this,t)
    }
  }

  private val systemToMitM = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case OMA(fun,args) if fun != Apply.term => ApplySpine(Traverser(this,fun),args.map(Traverser(this,_)):_*)
      case OMS(pth) if namespace <= pth =>
        val trg = alignmentserver.getFormalAlignments(pth).filter(_.props contains (("type","VRE"))).collect{
          case a if VRESystem.MitM <= a.to.mmturi => a.to.mmturi
        }
        trg.headOption match {
          case Some(gn : GlobalName) => OMS(gn)
          case _ => throw GeneralError("No alignment to")
        }
      case _ => Traverser(this,t)
    }
  }
  */

  lazy val links : List[DeclaredLink] = (archive.allContent ::: mitm.allContent).map(controller.get).collect {
    case th : DeclaredTheory => th.getNamedStructures collect {
      case s : DeclaredStructure => s
    }
    case v : DeclaredView => List(v)
  }.flatten

  private def translator(to : Archive) = {
    val aligns = alignmentserver.getAll.collect {
      case fa : FormalAlignment => AlignmentTranslation(fa)(controller)
    }
    val linktrs = links.map(l => LinkTranslation(l)(controller))

    new AcrossLibraryTranslator(controller,aligns ::: complexTranslations,linktrs,to)
  }

  def translateToSystem(t : Term) : Term = {
    val trl = translator(archive)
    val (res,succ) = trl.translate(t)
    succ.foreach(s => throw BackendError("could not translate symbol",s))
    res
  }
  def translateToMitM(t : Term) : Term = {
    val trl = translator(mitm)
    val (res,succ) = trl.translate(t)
    succ.foreach(s => throw BackendError("could not translate symbol",s))
    res
  }
}


class GAPSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("GAP",Systems.gapsym,"ODK/GAP",serverurl,port)
class SingularSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Singular",Systems.singularsym,"ODK/Singular",serverurl,port)
class SageSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Sage",Systems.sagesym,"ODK/Sage",serverurl,port) {
  object NonTrivials extends StatelessTraverser {
    val nf = Sage.Sage.docpath ? """sage.rings.number_field.number_field""" ? "NumberField"
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case ApplySpine(OMS(MitM.polycons),List(_,r,_,lst)) =>
        val ls = Sequences.flatseq.unapplySeq(lst).getOrElse(return t)
        OMA(r,ls.toList)
      case OMA(OMS(MitM.polycons),List(_,r,_,lst)) =>
        val ls = Sequences.flatseq.unapplySeq(lst).getOrElse(return t)
        OMA(r,ls.toList)
      case ApplySpine(OMS(`nf`),a :: Nil) =>
        OMA(OMS(`nf`),List(a,StringLiterals("x")))
      case OMA(OMS(`nf`),a :: Nil) =>
        OMA(OMS(`nf`),List(a,StringLiterals("x")))
      case _ => Traverser(this,t)
    }
  }

  override def translateToSystem(t: Term): Term = NonTrivials(super.translateToSystem(t),Context.empty)
}

object RemoteGAPSystem extends GAPSystem("neptune.eecs.jacobs-university.de")

