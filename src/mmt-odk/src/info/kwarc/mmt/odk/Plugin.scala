package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{History, MMTStructureChecker, Solver, SubtypingRule}
import info.kwarc.mmt.api.frontend.ChangeListener
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory}
import info.kwarc.mmt.api.symbols.{DeclaredStructure, FinalConstant, Structure}
import objects._
import uom._
import utils._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.odk.SCSCP.Server.MitMServer
import info.kwarc.mmt.odk.Singular.SingularImporter

class Plugin extends frontend.Extension {
  val theory = Math.path
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
  }
}

class GAPSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("GAP",GAP.GAP._base,serverurl,port)
class SingularSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Singular",Singular.Singular.dpath,serverurl,port)
class SageSystem(serverurl : String, port : Int = 26133) extends VREWithAlignmentAndSCSCP("Sage",Sage.Sage._base,serverurl,port)

object RemoteGAPSystem extends GAPSystem("neptune.eecs.jacobs-university.de")

object MitM {
   val path = DPath(URI("http","mathhub.info") / "MitM" / "Foundation")
}

class UniverseInference extends ChangeListener {

  private lazy val checker = controller.extman.get(classOf[MMTStructureChecker])
  private object TypeLevel {
    val path = (DPath(URI.http colon "gl.mathhub.info") / "MMT" / "LFX" / "TypedHierarchy") ? "Symbols" ? "TypeLevel"
    val term = OMS(path)
    def apply(i : BigInt) : Term = i match {
      case _ if i == BigInt(1) => OMS(Typed.ktype)
      case _ if i == BigInt(2) => OMS(Typed.kind)
      case _ if i < 1 =>
        require(i>=1)
        ???
      case _ =>
        OMA(this.term,List(NatLiterals.of(i)))
    }
    def unapply(t:Term) : Option[BigInt] = t match {
      case OMA(this.term,List(NatLiterals(i))) => i match {
        case i:BigInt => Some(i)
        case _ => None
      }
      case OMS(Typed.ktype) => Some(1)
      case OMS(Typed.kind) => Some(2)
      case _ => None
    }
  }

  override def onCheck(c: StructuralElement) : Unit = c match {
    case c : FinalConstant =>
      c.tp match {
        case Some(tp) if c.df.isEmpty =>
          val parent = controller.get(c.parent)
          val parentcurrent = parent.metadata.get(TypeLevel.path).map(_.value)
          val context = parent match {
            case th : DeclaredTheory => th.getInnerContext
            case _ => return ()
          }
          val univ = Solver.infer(controller, context, tp, None)
          val previous : BigInt = parentcurrent.headOption match {
            case Some(TypeLevel(j)) => j
            case _ => 1
          }
          val ret = univ match {
            case Some(TypeLevel(i)) =>
              c.metadata.update(new MetaDatum(TypeLevel.path, TypeLevel(i)))
              i max previous
            case _ => previous
          }
          parent.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(ret)))
        case _ =>
      }
    case ds : Structure =>
      // TODO
      val parent = controller.get(c.parent) match {
        case dm : DeclaredModule => dm
        case _ => return ()
      }
      val parentV : BigInt = parent.metadata.get(TypeLevel.path).map(_.value).headOption match {
        case Some(TypeLevel(j)) =>
          j
        case _ => 1
      }
      val dom : Option[DeclaredTheory] = ds.from match {
        case OMPMOD(mp,_) =>
          Some(controller.getAs(classOf[DeclaredTheory],mp))
        case _ => None
      }
      val structV : BigInt = dom.flatMap(_.metadata.get(TypeLevel.path).map(_.value).headOption) match {
        case Some(TypeLevel(j)) =>
          j
        case _ => 1
      }
      parent.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(parentV max structV)))
    case th : DeclaredTheory if th.parameters.nonEmpty =>
      /* This is actually wrong
      var current : BigInt = th.metadata.get(TypeLevel.path).map(_.value).headOption match {
        case Some(TypeLevel(j)) => j
        case _ => 1
      }
      th.parameters.foreach {
        case VarDecl(_,_,Some(tp),_,_) =>
          val univ = Solver.infer(controller, th.getInnerContext, tp, None)
          univ match {
            case Some(TypeLevel(i)) =>
              current = i max current
            case _ =>
          }
        case _ =>
      }
      th.metadata.update(new MetaDatum(TypeLevel.path,TypeLevel(current))) */
    case _ =>
  }
}