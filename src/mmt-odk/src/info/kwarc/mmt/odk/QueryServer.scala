package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.QueryEvaluator.QuerySubstitution
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.mitm.MitM
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{OpenMathError, SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

import scala.util.Try


trait UsesSCSCP { this : VRESystem =>
  val serverurl : String
  val port : Int // = 26133

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
    case MitM.ff => OMSymbol("false","logic1",None,None)
    case MitM.tt => OMSymbol("true","logic1",None,None)
    case _ =>
      println(t.getClass)
      ???
  }
  private def translateFromOM(t : OMExpression) : Term = t match {
    case OMApplication(f,args,_,_) => OMA(translateFromOM(f),args.map(translateFromOM))
    case OMSymbol(nm,cd,_,Some(base)) => OMS(DPath(base) ? cd ? nm)
    case OMSymbol("false","logic1",_,_) => MitM.ff
    case OMSymbol("true","logic1",_,_) => MitM.tt
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
