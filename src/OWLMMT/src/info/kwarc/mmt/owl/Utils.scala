package info.kwarc.mmt.owl
import org.semanticweb.owlapi.model._
import info.kwarc.mmt.api._

object Utils {
	def IRILast(i: IRI) : String = {
	   val uri = utils.URI.fromJava(i.toURI)
	   uri.fragment match {
	     case Some(s) => s
         case None => uri.path.last
	   }
	}
}