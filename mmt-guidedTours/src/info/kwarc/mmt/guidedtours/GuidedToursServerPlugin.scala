package info.kwarc.mmt.guidedtours
import java.lang.String
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.modules.DeclaredTheory
import objects._
import libraries._
import scala.concurrent._
import tiscaf._
import scala.collection.mutable.HashMap._
import info.kwarc.mmt.api.web._
import scala.util.parsing.json._
import scala.util._
import scala.annotation.tailrec



class GuidedToursServerPlugin extends ServerExtension("guided-tours") with Logger {
  
  def error(msg : String) : HLet = {
    log("ERROR: " + msg)
    Server.errorResponse(msg)
  }
  
  def apply(uriComps: List[String], query: String, body : Body): HLet = {
    try {
      uriComps match {
        case "gethtml" :: _ => getHtmlResponse
        case _ => error("Invalid request: " + uriComps.mkString("/"))
      }
    } catch {
      case e : Error => 
        log(e.shortMsg) 
        Server.errorResponse(e.shortMsg)
      case e : Exception => 
        error("Exception occured : " + e.getStackTrace())
    }
  }
  
  private def getImmediateChildren(path: Path) : List[Path] = {
    controller.depstore.queryList(path, ToObject(Includes))
  }
  
  private def getAllChildren(name: String) : List[Path] = {
    val path = Path.parseM(name, mmt.mmtbase)
    controller.depstore.queryList(path, Transitive(ToObject(Includes)))
  }
  
  private def check(list1: List[Path], list2: List[Path]) : Path = {
    list1 match{
      case Nil => null
      case head :: tail => 
        if(list2.contains(head)) 
        {
          head
        }
        else
        {
          check(tail, list2)
        }
    }
  }
  private def sort(topics: List[Path]) : List[Path] = {
    
    @tailrec
    def sortWithResult(list1: List[Path], acc:List[Path]) : List[Path] = {
      list1 match{
        case Nil => acc
        case head :: tail => 
          val children = getImmediateChildren(head)
          val res = check(children, tail)
          if(res == null)
          {
            sortWithResult(tail, acc :+ head )
          }
          else
          {
            val arr = res :: head :: tail
            sortWithResult(arr.distinct, acc)
          }
        case _ => acc
      }
    }
    sortWithResult(topics, Nil)
  }
  
   private def getHtmlResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      //val params = reqBody.asJSON.obj
      val topicName = tk.req.param("topic").getOrElse(throw ServerError("No topic name found")).toString
      //val symbol = params.get("symbol").getOrElse(throw ServerError("No symbol found")).toString
      //val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val l = controller.depstore.getInds(ontology.IsTheory)
      //val children = getAllChildren(topicName)
      val pathS = "http://mathhub.info/MiKoMH/GenCS/dmath/en/sets-operations.omdoc?sets-operations"
      val path = Path.parseM(pathS, mmt.mmtbase)
      //val l2 = getImmediateChildren(path)
      val l3 = getAllChildren(pathS)
      val l2 = sort(l3)
      
      
      val response = "potato" + l2.mkString("(",",",")") + "\n\n\n\n\n\n" + l3.mkString("(",",",")") + "\n\n\n\n\n" + l.mkString("(",",",")")
      
      Server.TextResponse(response).aact(tk)
    } catch {
      case e : Error => error(e.getLongMessage).aact(tk)
      case e : Exception => error(e.getMessage).aact(tk)
    }
  }
}

