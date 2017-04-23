package info.kwarc.mmt.odk

import info.kwarc.mmt.api.backend.Storage
import info.kwarc.mmt.api.{DPath, GeneralError, GlobalName}
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.{AlignmentsServer, QueryExtension}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{Apply, ApplySpine, LF}
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{OpenMathError, SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

/**
  * Created by jazzpirate on 21.04.17.
  */
class VREServer {

}

abstract class VRESystem(val id : String) extends QueryExtension(id) {
  override val logPrefix = id
  val namespace : DPath
}

object VRESystem {
  val MitM = DPath(URI.http colon "mathhub.info") / "MitM"
}

trait AlignmentBasedMitMTranslation { this : VRESystem =>

  lazy protected val alignmentserver = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  private val mitmToSystem = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case ApplySpine(fun,args) => Traverser(this,OMA(fun,args))
      case OMS(pth) if VRESystem.MitM <= pth =>
        val trg = alignmentserver.getFormalAlignments(pth).filter(_.props contains (("type","VRE"))).collect{
          case a if namespace <= a.to.mmturi => a.to.mmturi
        }
        trg.headOption match {
          case Some(gn : GlobalName) => OMS(gn)
          case _ => throw GeneralError("No alignment to")
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

  def translateToSystem(t : Term) = mitmToSystem(t,Context.empty)
  def translateToMitM(t : Term) = systemToMitM(t,Context.empty)
}

trait UsesSCSCP{ this : VRESystem =>
  val serverurl : String
  val port : Int = 26133

  lazy val client = SCSCPClient(serverurl,port)

  def newCallId = {
    val time = java.lang.System.currentTimeMillis().toString

    // and just to be safe, add a random integer
    val rn = scala.util.Random.nextInt().toString

    // and concatinate with the service id
    "GAP:" + time + ":" + rn
  }

  def scscpcall(t : Term) : Term = translateToOM(t) match {
    case ap : OMApplication =>
      val req = new SCSCPCall(ap.elem.asInstanceOf[OMSymbol], SCSCPCallArguments(newCallId, Some(SCSCPReturnObject), null), ap.arguments: _*)
      log("Request: " + req.toOMObject)
      val ret = client(ap).fetch().get
      log("Return: " + ret)
      translateFromOM(ret)
    case ot => t
  }

  private def translateToOM(t : Term) : OMExpression = t match {
    case OMA(f,args) => OMApplication(translateToOM(f),args.map(translateToOM),None,None)
    case OMS(gn) => OMSymbol(gn.name.toString,gn.module.name.toString,None,Some(gn.module.parent.uri))
    case OMV(nm) => OMVariable(nm.toString,None)
    case OMLIT(value : BigInt,_) => OMInteger(value,None)
    case OMLIT(value : String,_) => OMString(value,None)
    case OMLIT(value : Double,_) => OMFloat(value,None)
    case Math.ff => OMSymbol("false","logic1",None,None)
    case Math.tt => OMSymbol("true","logic1",None,None)
    case _ =>
      println(t.getClass)
      ???
  }
  private def translateFromOM(t : OMExpression) : Term = t match {
    case OMApplication(f,args,_,_) => OMA(translateFromOM(f),args.map(translateFromOM))
    case OMSymbol(nm,cd,_,Some(base)) => OMS(DPath(base) ? cd ? nm)
    case OMSymbol("false","logic1",_,_) => Math.ff
    case OMSymbol("true","logic1",_,_) => Math.tt
    case OMVariable(nm,_) => OMV(nm)
    case OMInteger(i,_) => IntegerLiterals.of(i)
    case OMString(s,_) => StringLiterals.of(s)
    case OMFloat(f,_) => ???
    case OMError(nm,params,tid,cdbase) =>
      throw OpenMathError(nm.name + ": " + params)
    case _ =>
      println(t.getClass)
      ???
  }
}