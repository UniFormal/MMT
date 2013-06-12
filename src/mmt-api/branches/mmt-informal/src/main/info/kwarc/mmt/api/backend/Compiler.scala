package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import archives._
import frontend._
import utils.File

/**
 * A Compiler transforms source files into OMDoc files
 */
trait Compiler extends archives.TraversingBuildTarget {
   val inDim = "source"
   val outDim = "compiled"
   override val outExt = "omdoc"
}

trait QueryTransformer extends Extension {
   /** true iff this Importer can handle input of type 'src' */
   def isApplicable(src: String): Boolean
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) : List[scala.xml.Node]
}

object TrivialQueryTransformer extends QueryTransformer {
   def isApplicable(src : String) = true
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) = List(n)
}