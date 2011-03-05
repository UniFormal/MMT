package ombase.controller

object PathTree {
	private def item(p : jomdoc.Path, state : String) =
		<item id={p.toPath} state={state}>
		  <content><name href="#" onclick={ombase.snippet.Get.navigate(p)}>{p.last}</name></content>
		</item>
	def apply(q: String) : scala.xml.Node = {
		if (q == ":root")
             <root>{item(Controller.basepath, "open")}</root>
          else {
        	  val path = jomdoc.Path.parse(q, Controller.basepath)
        	  Controller.get(path)
        	  val children = Controller.depstore.query(path, + jomdoc.ontology.Declares) 
        	  <root>{children.map{c => item(c, "closed")}}</root>
          }
	}
}