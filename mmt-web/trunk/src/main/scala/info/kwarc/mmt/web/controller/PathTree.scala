package info.kwarc.mmt.web.controller

import info.kwarc.mmt.api._

object PathTree {
	private def item(p : Path, state : String) =
		<item id={p.toPath} state={state}>
		  <content><name href="#" onclick={info.kwarc.mmt.web.snippet.Get.navigate(p)}>{p.last}</name></content>
		</item>
	def apply(q: String) : scala.xml.Node = {
		if (q == ":root")
             <root>{item(Controller.basepath, "open")}</root>
          else {
        	  val path = Path.parse(q, Controller.basepath)
        	  Controller.get(path)
        	  val children = Controller.depstore.query(path, + ontology.Declares) 
        	  <root>{children.map{c => item(c, "closed")}}</root>
          }
	}
}