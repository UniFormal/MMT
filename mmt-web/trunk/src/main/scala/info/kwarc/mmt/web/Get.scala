package info.kwarc.mmt.web

import scala.xml._
import net.liftweb._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.BindHelpers._
import net.liftweb.json.JsonAST._

import info.kwarc.mmt.api._
import libraries._
import ontology._
import modules._

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
      val meta = deps.queryList(path, - HasMeta)
      val imps = deps.queryList(path, - Includes)
      val strs = deps.queryList(path, - Query.HasStructureFrom)
      val doms = deps.queryList(path, - HasDomain * HasType(IsView))
      val cods = deps.queryList(path, - HasCodomain * HasType(IsView))
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
   private val lib = Manager.controller.library
   private def item(p : Path, state : String, label : Option[String] = None) = 
      <item id={p.toPath} state={state}>
        <content><name href="#" onclick={navigate(p)}>{label.getOrElse(p.last)}</name></content>
      </item>
   def tree(q: String) : scala.xml.Node = {
      if (q == ":root")
         <root>{
            Manager.controller.backend.getArchives map {a => item(DPath(a.narrationBase), "closed", Some(a.id))}
         }</root>
      else {
           val path = Path.parse(q, Manager.basepath)
           val elem = Manager.controller.get(path)
           path match {
              case p: DPath => 
                 val children = deps.queryList(path, + Declares) 
                 <root>{children.map{c => item(c, "closed")}}</root>
              case p:MPath =>
                 val rels : List[(String,Query)] = elem match {
                    case t: Theory =>
                       List(("meta for",  - HasMeta), ("included into",  - Includes),
                            ("instantiated in",  - Query.HasStructureFrom),
                            ("views out of", - HasDomain * HasType(IsView)), ("views into",  - HasCodomain * HasType(IsView)))
                    case v: View => List(("included into", - Includes), ("domain", + HasDomain), ("codomain", + HasCodomain))
                 }
                 val results = rels map {case (desc, rel) => (desc, deps.queryList(path, rel))}
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
   /**
    * returns the Json object representing a graph
    * @param p the MMT URI that is currently focused (ignore for now)
    */
   def graph(p: Path) : JValue = {
      val nodes : Iterator[Path] = deps.getInds(IsTheory)
      val views : Iterator[Path] = deps.getInds(IsView)
      val structures : Iterator[Path] = deps.getInds(IsStructure)
      // dummy return value to make the code compile 
      JNothing
   }
}