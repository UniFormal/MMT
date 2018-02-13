package info.kwarc.mmt.api.documents

import info.kwarc.mmt.api._
import metadata._
import utils._
import objects._

object NarrativeMetadata {
   val keyBase = DPath(URI("http", "purl.org") / "dc" / "terms") ? "_"

   def allKeys = List("creator", "created", "description", "abstract", "subject", "title")
   def allAnnotators = allKeys map {k => new NarrativeMetadata(k)}

   val title = new NarrativeMetadata("title")
   val description = new NarrativeMetadata("description")
}

/** this class should be generalized to allow any semi-formal values  */
class NarrativeMetadata(name: String) extends Annotator[String](NarrativeMetadata.keyBase ? name) {
  def keyword = "@" + name
  def fromObject(o: Obj) = {
      o match {
         case OMSemiFormal(List(Text("", s))) => s
         case _ => throw ImplementationError("not a string " + o)
      }

   }
   def toObject(s: String) = OMSemiFormal(List(Text("", s)))
}


