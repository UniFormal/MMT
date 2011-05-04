package info.kwarc.mmt.web.controller
import info.kwarc.mmt.web.snippet._
import info.kwarc.mmt.api._

object PathTree {
	private def item(p : Path, state : String) =
		<item id={p.toPath} state={state}>
		  <content><name href="#" onclick={Get.navigate(p)}>{p.last}</name></content>
		</item>
	def apply(q: String) : scala.xml.Node = {
		if (q == ":root")
             <root>{item(Manager.basepath, "open")}</root>
          else {
        	  val path = Path.parse(q, Manager.basepath)
        	  Manager.controller.get(path)
        	  val children = Manager.controller.depstore.query(path, + ontology.Declares) 
        	  <root>{children.map{c => item(c, "closed")}}</root>
          }
	}
}