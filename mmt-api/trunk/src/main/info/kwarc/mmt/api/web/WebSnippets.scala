package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import ontology._
import documents._
import Server._

/**
 * serves a tree-based navigation component for an HTML page
 */
class TreeView extends ServerExtension("tree") {
   private def item(p : Path, state : String, label : Option[String] = None) = 
      <item id={p.toPath} state={state}>
        <content><name href="#" onclick={"mmt.sideBarClick(event, '" + p.toPath + "')"}>{label.getOrElse(p.last)}</name></content>
        </item>
   def apply(path: List[String], query: String, body: Body) = {
       val q = query
       val node = if (q == ":root")
         <root>{
           controller.backend.getArchives.sortBy(_.id.toLowerCase) map {a =>
              item(DPath(a.narrationBase), "closed", Some(a.id))
           }
         }</root>
       else {
         val path = Path.parse(q, controller.getNamespaceMap)
         val role = controller.depstore.getType(path)
         path match {
           case p: DPath =>
             val doc = controller.getDocument(p)
             val docitems = doc.getRefs
             <root>{ docitems.map { i => item(i.target, "closed") } }</root>
           case p: MPath =>
             val rels: List[(String, RelationExp)] = role match {
               case Some(ontology.IsTheory) =>
                 List(("meta for", -HasMeta), ("included into", -Includes),
                   ("instantiated in", -RelationExp.HasStructureFrom),
                   ("views out of", -HasDomain * HasType(IsView)), ("views into", -HasCodomain * HasType(IsView)))
               case Some(IsView) => List(("included into", -Includes), ("domain", +HasDomain), ("codomain", +HasCodomain))
               case _ => Nil // should be impossible
             }
             val results = rels map { case (desc, rel) => (desc, controller.depstore.queryList(path, rel)) }
             val resultsNonNil = results.filterNot(_._2.isEmpty)
             <root>{
               resultsNonNil map {
                 case (desc, res) =>
                   <item state="closed">
                  <content><name class="treerelation">{ desc }</name></content>
                  { res.map(item(_, "closed")) }
                </item>
               }
             }</root>
           case _ => throw ImplementationError("only children of documents and modules can be taken")
         }
       }
       XmlResponse(node)
   }
}

/**
 * serves a bread crumbs-style navigation component for an HTML page
 */
class BreadcrumbsServer extends ServerExtension("breadcrumbs") {
   def apply(path: List[String], query: String, body: Body) = {
      val mmtpath = Path.parse(query, controller.getNamespaceMap)
      val ancs = mmtpath.ancestors.reverse
      var mpathfound = false
      var spathfound = false
      val res = utils.HTML.build {h =>
         import h._
         def gsep = span {text {"?"}}
         def lsep = span {text {"/"}}
         // strangely, the client somehow does not handle this right if the XML is given literally, might be due to namespaces
         div(attributes = List("xmlns" -> utils.xml.namespace("xhtml"), "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
            ancs.foreach {p =>
               p match {
                  case p : MPath if ! mpathfound => mpathfound = true; gsep
                  case p : GlobalName if ! spathfound => spathfound = true; gsep
                  case p if p.^! == p => Nil
                  case _ => lsep
               }
               span("mmturi", attributes=List("jobad:href" -> p.toPath)) {text {p.last}}
            }
         }
      }
      Server.XmlResponse(res)
   }
}
