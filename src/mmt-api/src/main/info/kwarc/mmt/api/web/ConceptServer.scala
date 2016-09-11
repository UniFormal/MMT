package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.ontology._

/**
  * Created by raupi on 11.09.16.
  */
class ConceptServer extends ServerExtension("concepts") {
  override val logPrefix = "concepts"

  lazy val alignments = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  private def item(name : String) =
    <item id={name} state="open">
      <content><name onclick={"conceptmenu.sideBarClick('" + name + "')"}>{name}</name></content>
    </item>

  private def groupItem(name : String,s : xml.Node*) =
  <item state="closed"> <content><name>{name}</name></content>{s}</item>

  def apply(path: List[String], query: String, body: Body, session: Session) = {
    log("CALL: path  = " + path +
      "\n      query = " + query)
    val q = query
    val node = if (q.startsWith(":menu")) {
      log("Constructing menu...")
      val conc = alignments.getConcepts
      // TODO ugly quick hack to get a not massively overflowing menu
      val alphabet = List('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t',
        'u','v','w','x','y','z','#')
      val list = alphabet collect {
        case '#' if conc.exists(c => !alphabet.init.contains(c.toLowerCase.head)) => ('#',conc.filter(c => !alphabet.init.contains(c.toLowerCase.head)).distinct)
        case a if conc.exists(c => c.toLowerCase.head == a) => (a,conc.filter(c => c.toLowerCase.head == a).distinct)
      }
      log("Done.")
      <root> {list.map(p => groupItem(p._1.toString,p._2.map(item):_*)).toArray }</root>
    } else if (q.startsWith(":concept=")) {
      val con = q.replace(":concept=","").trim
      log("Concept call: \"" + con + "\"...")
      val allaligs = alignments.getConceptAlignments(con)//.map(a => a.ref.toString)
      log(allaligs.length.toString + " Alignments found.")
      val altnames = allaligs.collect {
        case cp : ConceptPair => List(cp.from.con,cp.to.con)
        case ca : ConceptAlignment => List(ca.concept)
      }.flatten.distinct.filterNot(_ == con)
      val aligs = allaligs collect {case ca : ConceptAlignment => ca.ref}
      val formals = aligs collect {
        case LogicalReference(pat) => <li><a href="???">{pat.toString}</a></li>
      }
      val informals = aligs collect {
        case PhysicalReference(uri) => <li><a href={uri.toString} target="_blank">{uri.toString}</a></li>
      }
      val ret=  <div>
        <h1>{con}</h1>
        {if (altnames.nonEmpty) {
          <div>
            <span>{"(See also: "}</span>
            {altnames.init.map(n => <span><a href={"#:concepts?:concept=" + n}>{n}</a>{", "}</span>)}
            <span><a href={"#:concepts?:concept=" + altnames.last}>{altnames.last}</a>{")"}</span>
          </div><br/>
        }}
        {if (formals.nonEmpty) <div>{"Formal Resources: "}<ul>{formals}</ul></div> else ""}
        {if (informals.nonEmpty) <div>{"External links: "}<ul>{informals}</ul></div> else ""}
      </div>
      log("Call returned.")
      ret
    } else {
      <root></root>
    }
    Server.XmlResponse(node)
  }
}
