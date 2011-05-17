package info.kwarc.mmt.web.snippet

import scala.xml._
import net.liftweb._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.BindHelpers._

import info.kwarc.mmt.web.controller._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api._
import modules._

/*
/** A class encapsulating various snippets that are called from the templates. */
class Get {
   private def log(msg : String) = Manager.report("get", msg)
   private val doc = S.param("document").getOrElse("")
   private val mod = S.param("module").getOrElse("")
   private val sym = S.param("symbol").getOrElse("")
   private val act = S.param("action").getOrElse("")
   private val basepath = Manager.basepath
   private val path = Path.parse(new info.kwarc.mmt.api.utils.xml.URI(doc), mod, sym, basepath)
   /** the main snippet retrieving the requested object and applying the requested action */
   /*def get : Node = {
      try {Manager.doGet(doc, mod, sym, act)}
      catch {case e @ backend.NotFound(p) => scala.xml.Text(e.getMessage)}
   }*/

   /** yields the requested MMT-URI (without action) */
   //def title : Node = scala.xml.Text(path.toString)

   //def linkOMDoc(xhtml : NodeSeq) = <span jobad:href={path.toPathLong + "?xml"}>as OMDoc</span>

   /*def incoming(xhtml : NodeSeq) : NodeSeq = {
      val deps = Manager.controller.depstore
      var result : NodeSeq = Nil
      val meta = deps.query(path, ToObject(HasMeta)).toList
      result ++= meta.flatMap(p =>
        BindHelpers.bind("i", xhtml,
          "href" -> p.toPath,
          "description" -> "meta"
        )
      )
      val imps = deps.query(path, + HasOccurrenceOfInImport).toList
      result ++= imps.flatMap(p => BindHelpers.bind("i", Get.link(xhtml, p), "description" -> ""))
      result
   }*/
}
*/

object Get {
   /*
   def link(xhtml : NodeSeq, p : Path) = {
	  val text = BindHelpers.bind("i", xhtml, "last" -> p.last, "full" -> p.toPath)
	  <span jobad:href={p.toPath}>{text}</span>
   }*/
   def ahref(p: Path) =
	   <a href="#" onclick={navigate(p)}>{p.last}</a>
   def navigate(p: Path) = 
	   "latin_navigate('" + p.toPath + "')"
   def incoming(path: Path) : Node = {
      val deps = Manager.controller.depstore
      val meta = deps.query(path, - HasMeta)
      val imps = deps.query(path, - HasOccurrenceOfInImport)
      val strs = deps.query(path, - Query.HasStructureFrom)
      val doms = deps.query(path, - HasDomain * HasType(IsView))
      val cods = deps.query(path, - HasCodomain * HasType(IsView))
      def refs(rel : String, subjs: List[Path]) : NodeSeq = {
        val lis = subjs map {p =>
          <li class="jstree-leaf">{Get.ahref(p)}</li>
        }
        <li class="jstree-leaf"><a href="">{rel}</a>{if (lis == Nil) Nil else <ul>{lis}</ul>}</li>
      }
      <ul xmlns={utils.mmt.namespace("xhtml")}>
        <li class="jstree-open">
          <a href="">known references</a>
          <ul>
           {refs("meta for", meta)}
           {refs("imported in",imps)}
           {refs("instantiated in",strs)}
           {refs("domain of",doms)}
           {refs("codomain of",cods)}
          </ul>
        </li>
      </ul>
   }
   /** yields a breadcrumb navigation bar as a sequence of links */
   def breadcrumbs(path : Path) : Node = {
      val ancs = path.ancestors.reverse
      val gsep = <span>?</span>
      val lsep = <span>/</span>
      var mpathfound = false
      var spathfound = false
      val crumbs = ancs.flatMap {p =>
         val sep = p match {
         case p : MPath if ! mpathfound => mpathfound = true; gsep
         case p : GlobalName if ! spathfound => spathfound = true; gsep
         case p if p.^! == p => Nil
         case _ => lsep
       }
       sep ++ <span jobad:href={p.toPath} class="crumb">{p.last}</span>
      }
      // strangely, the client somehow does not handle this right if the XML is given literally, might be due to namespaces
      //<div xmlns={utils.xml.namespace("xhtml")} xmlns:jobad={utils.xml.namespace("jobad")}>{crumbs}</div>
      Elem(null, "div", Null, NamespaceBinding(null, utils.xml.namespace("xhtml"),
                              NamespaceBinding("jobad", utils.xml.namespace("jobad"), TopScope)), crumbs : _*)
   }
   private val deps = Manager.controller.depstore
   private def item(p : Path, state : String) = 
      <item id={p.toPath} state={state}>
        <content><name href="#" onclick={navigate(p)}>{p.last}</name></content>
      </item>
   def tree(q: String) : scala.xml.Node = {
      if (q == ":root")
          <root>{item(Manager.basepath, "closed")}</root>
      else {
           val path = Path.parse(q, Manager.basepath)
           val elem = Manager.controller.get(path)
           path match {
              case p: DPath => 
                 val children = deps.query(path, + ontology.Declares) 
                 <root>{children.map{c => item(c, "closed")}}</root>
              case p:MPath =>
                 val rels : List[(String,Query)] = elem match {
                    case t: Theory =>
                       List(("meta for",  - HasMeta), ("included into",  - HasOccurrenceOfInImport),
                            ("instantiated in",  - Query.HasStructureFrom),
                            ("views out of", - HasDomain * HasType(IsView)), ("views into",  - HasCodomain * HasType(IsView)))
                    case v: View => List(("included into", - HasOccurrenceOfInImport), ("domain", + HasDomain), ("codomain", + HasCodomain))
                 }
                 val results = rels map {case (desc, rel) => (desc, deps.query(path, rel))}
                 val resultsNonNil = results.filterNot(_._2.isEmpty) 
                 <root>{
                    resultsNonNil map {case (desc,res) =>
                       <item state="closed">
                          <content><name class="treerelation">{desc}</name></content>
                          {res.map(item(_, "closed"))}
                       </item>
                    }
                 }</root>
          }
      }
   }
}