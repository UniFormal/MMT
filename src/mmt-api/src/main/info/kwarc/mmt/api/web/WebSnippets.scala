package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import ontology._
import documents._
import Server._

/**
 * serves a tree-based navigation component for an HTML page
 */
class TreeView extends ServerExtension("tree") {
   /** item for a knowledge item (including archives) */
   private def item(p : Path, state : String, label : Option[String] = None) = 
      <item id={p.toPath} state={state}>
        <content><name href="#" onclick={"mmt.sideBarClick(event, '" + p.toPath + "')"}>{label.getOrElse(p.last)}</name></content>
      </item>
   /** item for an archive group */
   private def groupItem(name: List[String], state: String) = {
      <item id={(":root"::name).mkString("/")} state={state}>
         <content><name>{name.last}</name></content>
      </item>
   }
   /** items for all archives/groups whose id starts with prefix */
   def archivesIn(prefix: List[String]) = {
      val archIds = controller.backend.getArchives.map(a => (a,utils.stringToList(a.id,"/")))
      val archSuffs = archIds.filter(_._2.startsWith(prefix)) map {case (a,s) => (a,s.drop(prefix.length))}
      val children = archSuffs.map {
         case (a,s) => (s.head, if (s.tail.isEmpty) Some(a) else None)
      }.distinct.sortBy(_._1.toLowerCase)
      <root>{children map {case (name, archOpt) =>
         archOpt match {
            case Some(a) => item(DPath(a.narrationBase), "closed", Some(name))
            case None => groupItem(prefix ::: List(name), "closed")
         }
      }}</root>
   }
   def apply(path: List[String], query: String, body: Body, session: Session) = {
       val q = query
       val node = if (q.startsWith(":root")) {
         val prefix = utils.stringToList(q, "/").tail
         archivesIn(prefix)
       } else {
         val path = Path.parse(q, controller.getNamespaceMap)
         val role = controller.depstore.getType(path)
         path match {
           case p: DPath =>
             val doc = controller.getDocument(p)
             val docitems = doc.getDeclarations.collect {
                case r: NRef => r.target
                case doc: Document => doc.path
             }
             <root>{ docitems.map { i => item(i, "closed") } }</root>
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
   def apply(path: List[String], query: String, body: Body, session: Session) = {
      val mmtpath = Path.parse(query, controller.getNamespaceMap)
      val ancs = mmtpath.ancestors.reverse
      var mpathfound = false
      var spathfound = false
      val res = utils.HTML.build {h =>
         import h._
         def gsep = span {text {"?"}}
         def lsep = span {text {"/"}}
         // strangely, the client somehow does not handle this right if the XML is given literally, might be due to namespaces
         div(attributes = List("xmlns" -> utils.xml.namespace("xhtml"))) {
            ancs.foreach {p =>
               p match {
                  case p : MPath if ! mpathfound => mpathfound = true; gsep
                  case p : GlobalName if ! spathfound => spathfound = true; gsep
                  case p if p.^! == p => Nil
                  case _ => lsep
               }
               span("mmturi", attributes=List(presentation.HTMLAttributes.href -> p.toPath)) {text {p.last}}
            }
         }
      }
      Server.XmlResponse(res)
   }
}
