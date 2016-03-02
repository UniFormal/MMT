package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import utils._
import web._

import scala.collection.mutable.HashSet

case class Alignment(kind: String, from: GlobalName, to: GlobalName) {
  override def toString = s"$kind $from $to"
}

class AlignmentsServer extends ServerExtension("align") {

  override val logPrefix = "align"
  
  private val alignments = HashSet[Alignment]()
  
  override def start(args:List[String]) {
    val file = File(args(0))
    readAlignments(file)
  }

  private val nsMap = NamespaceMap.empty
  
  def apply(uriComps: List[String], query: String, body: Body) = {
      val from = Path.parseS(query, nsMap)
      val toS = getAlignments(from)
      Server.TextResponse(toS.mkString("\n"))
  }
  
  private def getAlignments(from: GlobalName) = alignments.filter(_.from == from)
  private def readAlignments(file: File) {
    val json = JSON.parse(File.read(file))
    json match {
      case obj: JSONObject => obj.map foreach {
         case (jsonstring, alignmentList:JSONArray) =>
            alignmentList.values foreach {
               case alignmentObject: JSONObject =>
                 val alignmentMap = alignmentObject.toList.toMap
                 val from = Path.parseS(alignmentMap(JSONString("from")).toString, nsMap)
                 val to = Path.parseS(alignmentMap(JSONString("to")).toString, nsMap)
                 alignments += Alignment(jsonstring.toString, from, to)
            }
       case _ =>
      }
    }
    
    alignments foreach println
  }
}

