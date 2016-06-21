package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import archives._
import documents._
import frontend._
import utils.File

trait QueryTransformer extends FormatBasedExtension {
   /** true iff this can handle input of type 'src' */
   def isApplicable(src: String): Boolean
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) : List[scala.xml.Node]
}

object TrivialQueryTransformer extends QueryTransformer {
   def isApplicable(src : String) = true
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) = List(n)
}