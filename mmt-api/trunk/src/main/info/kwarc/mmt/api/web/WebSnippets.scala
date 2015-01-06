package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import ontology._
import documents._
import Server._

class TreeView extends ServerExtension("tree") {
   def apply(path: List[String], query: String, body: Body) = {
       val q = query
       val node = if (q == ":root")
         <root>{
           controller.backend.getArchives.sortBy(_.id.toLowerCase) map {a =>
              Util.item(DPath(a.narrationBase), "closed", Some(a.id))
           }
         }</root>
       else {
         val path = Path.parse(q, controller.getBase)
         val role = controller.depstore.getType(path)
         path match {
           case p: DPath =>
             val doc = controller.getDocument(p)
             val docitems = doc.getItems.sortWith {
                case (r: DRef, s: MRef) => false
                case (r: MRef, s: DRef) => true
                case (r,s)  => r.target.last <= s.target.last
             } 
             <root>{ docitems.map { i => Util.item(i.target, "closed") } }</root>
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
                  { res.map(Util.item(_, "closed")) }
                </item>
               }
             }</root>
           case _ => throw ImplementationError("only children of documents and modules can be taken")
         }
       }
       XmlResponse(node)
   }
}

/** part of web browser */
class BreadcrumbsServer extends ServerExtension("breadcrumbs") {
   def apply(path: List[String], query: String, body: Body) = {
      val mmtpath = Path.parse(query, controller.getBase)
      val ancs = mmtpath.ancestors.reverse
      var mpathfound = false
      var spathfound = false
      val html = utils.HTML.builder
      import html._
      def gsep() = span {text {"?"}}
      def lsep() = span {text {"/"}}
      // strangely, the client somehow does not handle this right if the XML is given literally, might be due to namespaces
      div(attributes = List("xmlns" -> utils.xml.namespace("xhtml"), "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
         ancs.foreach {p =>
            p match {
               case p : MPath if ! mpathfound => mpathfound = true; gsep()
               case p : GlobalName if ! spathfound => spathfound = true; gsep()
               case p if p.^! == p => Nil
               case _ => lsep()
            }
            span("mmturi", attributes=List("jobad:href" -> p.toPath)) {text {p.last}}
         }
      }
      Server.XmlResponse(html.result)
   }
}