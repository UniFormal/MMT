package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.checking.{History, HistoryEntry}
import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMFOREIGN, OMS}
import info.kwarc.mmt.api.presentation.{Presenter, RenderingHandler}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, Include, NestedModule, Structure}
import info.kwarc.mmt.api.{CPath, DPath, DefComponent, GlobalName, MPath, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.utils.{FilePath, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.rules.ModelsOf
import info.kwarc.mmt.stex.xhtml.HTMLParser
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.{SHTML, STeXPresenterML, STeXPresenterTex, STeXServer}

import scala.xml.{Node, NodeSeq}

trait OMDocHTML { this : STeXServer =>

  protected def omdocRequest(request: ServerRequest): ServerResponse = {
    request.path.lastOption match {
      case Some("omdoccss") =>
        ServerResponse(MMTSystem.getResourceAsString("mmt-web/stex/omdoc.css"),"text/css")
      case Some("omdoc") =>
        val qr = request.query.replace("&amp;","&")
        qr match {
          case "" =>
            ServerResponse("Empty query", "txt")
          case s =>
            if (s startsWith "archive=") {
              val (id, path,lang) = {
                s.drop(8).split('&') match {
                  case Array(s) => (s, FilePath(Nil),"")
                  case Array(s, r) if r.startsWith("filepath=") =>
                    (s, FilePath(r.drop(9).split('/').toList),"")
                  case Array(s, r, l) if r.startsWith("filepath=") && l.startsWith("language=") =>
                    (s, FilePath(r.drop(9).split('/').toList), l.drop(9))
                  case _ =>
                    return ServerResponse(s"Malformed query string ${s}","txt")
                }
              }
              val ns = controller.backend.getArchive(id) match {
                case None => return ServerResponse(s"unknown archive id ${id}","txt")
                case Some(a) => a.narrationBase / (if (path.toString endsWith ".xhtml") path.toString.dropRight(5) + "omdoc" else path.toString)
              }
              var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
              html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/omdocfrag?" + ns + "&language=" + lang)
              html = html.replace("BASE_URL_PLACEHOLDER", "")
              html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/omdoccss")
              ServerResponse(html, "text/html")
            } else {
              var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
              html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/omdocfrag?" + s)
              html = html.replace("BASE_URL_PLACEHOLDER", "")
              html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/omdoccss")
              ServerResponse(html, "text/html")
            }
        }
      case Some("omdocfrag") =>
        val (comp, lang) = request.query.split('&') match {
          case Array(a) => (a, None)
          case Array(a, l) if l.startsWith("language=") => (a, if (l.drop(9).isEmpty) None else Some(l.drop(9)))
          case _ => (request.query, None)
        }
        val path = Path.parse(comp)
        implicit val state = new State(lang.getOrElse("en"))
        val ret = path match {
          case d:DPath =>
            documentTop(d).toString()
          case mp:MPath => moduleTop(mp).toString()
          case gn:GlobalName => declTop(gn).toString()
          case _ => "<div>Invalid: " + path.toString + "</div>"
        }
        val (doc, body) = this.emptydoc
        body.add(ret)
        ServerResponse(doc.get("body")()().head.toString,"text/html")
      case _ => ServerResponse(s"Malformed URL path ${request.path}", "txt")
    }
  }

  private class State(val language: String) {
    private var id = 0
    var lastSection : Option[Node] = None
    def getId = {
      id += 1
      "omdocid"+ id.toString
    }
    def withTitle[A](ttl:Node)(f : Option[Node] => A): A = {
      if (lastSection.contains(ttl)) {
        f(None)
      } else {
        val lasttitle = lastSection
        lastSection = Some(ttl)
        try { f(Some(ttl)) } finally { lastSection = lasttitle }
      }
    }
  }

  private def documentTop(path:DPath)(implicit state:State): NodeSeq = {
    controller.getO(path) match {
      case Some(d: Document) => documentTop(d)
      case None =>
        <div><h1>Document {path.toString}</h1><hr/><div>Not found</div></div>
    }
  }

  private def documentTop(doc:Document)(implicit state: State): NodeSeq = {
    val title = getTitle(doc) match {
      case Some(node) => node
      case None => <span>Document {doc.path}</span>
    }
    state.withTitle(title){
      case Some(title) =>
        <div><h1>{present(title)}</h1><hr/>{documentBody(doc)}</div>
      case _ => documentBody(doc)
    }
  }

  private def documentSection(path: DPath)(implicit state: State): NodeSeq = {
    controller.getO(path) match {
      case Some(d: Document) =>
        documentSection(d)
      case None =>
        <div>
          <div style="font-weight:bold">Document
            {path.toString}
          </div>
          <div>Not found</div>
        </div>
    }
  }

  private def documentSection(doc: Document)(implicit state: State): NodeSeq = {
    val title = getTitle(doc) match {
      case Some(node) => node
      case None => <span>Document
        {doc.path}
      </span>
    }
    state.withTitle(title) {
      case Some(title) =>
        collapsible() { present(title) } { documentBody(doc) }
      case _ => documentBody(doc)
    }
  }

  private def documentBody(d: Document)(implicit state: State): NodeSeq = {
    d.getDeclarations.flatMap {
      case mr: MRef if mr.target.parent == d.path =>
        <span></span> // TODO
      case dr: DRef =>
        //doInputref(dr.target,lang)
        documentSection(dr.target)
      case m: MRef =>
        moduleInner(m.target)
      case o =>
        <div style="width:100%">TODO: {o.getClass} </div>
    }
  }

  private def moduleTop(path: MPath)(implicit state: State): NodeSeq = {
    controller.getO(path) match {
      case Some(d: Theory) => moduleTop(d)
      case None =>
        <div><h1>Module {path.toString}</h1> <hr/>
          <div>Not found</div>
        </div>
    }
  }

  private def moduleTop(t:Theory)(implicit state: State): NodeSeq = {
    <div><h1>Module {t.path.toString}</h1><hr/>{doModuleBody(t)}</div>

    /*
    def doDecl(d: Declaration): NodeSeq = d match {
      case Include(i) => Nil
      case c: Constant =>
        doBlock(<span>
          <b>Term</b>{c.df.map(df => xhtmlPresenter.asXML(df, Some(c.path $ DefComponent))).getOrElse()}
        </span>) {
          <span>Inferred Type
            {c.tp.map(tp => xhtmlPresenter.asXML(tp, Some(c.path $ TypeComponent))).getOrElse("(None)")}
          </span>
        }
      case nm: NestedModule =>
        nm.module.getPrimitiveDeclarations.flatMap(doDecl)
      case o =>
        Seq(<div style="width:100%">TODO:
          {o.getClass}
        </div>)
    }

    controller.getO(mp) match {
      case Some(t: Theory) if this.getLanguage(t).isDefined => doBlock(
        <b>
          <span>Language Module
            {t.path.name}
            for</span> <a href={doLink(t.path, lang)} style="color:blue">?
          {t.path.parent.last}
        </a>
        </b>
      ) {
        <div style="width:100%">
          {def getIncls(t: Theory): List[MPath] =
          t.getIncludesWithoutMeta.tail ::: t.getPrimitiveDeclarations.collect { case nm: NestedModule => getIncls(nm.module.asInstanceOf[Theory]) }.flatten

        val includes = getIncls(t)
        if (includes.nonEmpty) {
          <div style="width:100%">
            <b>Uses</b>{includes.map(p => <span style="display:inline">
            <a href={doLink(p, lang)} style="color:blue">?
              {p.name.toString}
            </a>
          </span>)}
          </div>
        } else <span/>}{t.getPrimitiveDeclarations.flatMap(doDecl)}
        </div>
      }
      case Some(t: Theory) => doBarBlock(<span>Module
        <a href={doLink(mp, lang)} style="color:blue">
          {mp.toString}
        </a>
      </span>) {
        doModuleBody(t, lang)
      }
      case _ => doBarBlock(<span>Module
        {mp.toString}
      </span>)(<span>Theory not found</span>)
    }
     */
  }

  private def moduleInner(path : MPath)(implicit state: State): NodeSeq = {
    controller.getO(path) match {
      case Some(d: Theory) => moduleInner(d)
      case None =>
        collapsible() { doLink(path) {
          <b>Module {path.name.toString}</b>
        }} {<div>Not found</div>}
    }
  }
  private def moduleInner(t: Theory)(implicit state: State): NodeSeq = {
    if (getLanguage(t).isDefined) return {<span>TODO language module</span>}
    collapsible() { doLink(t.path) {
        <b>Module {t.name.toString}</b>
    }}{doModuleBody(t)}
  }

  private def doModuleBody(theory: Theory)(implicit state: State): NodeSeq = <div>
    <div>{theory.getIncludesWithoutMeta match {
      case Nil =>
      case ls => <span style="display:inline;"><b>Includes </b>{
        val ils = ls.map(p => doLink(p){<span>{p.name.toString}</span>})
        ils.flatMap(n => List(n,{<span>, </span>})).init
      }<hr/></span>
    }}</div>
    <div>{theory.getPrimitiveDeclarations.flatMap {
      case Include(_) => Nil
      case nm:NestedModule if nm.metadata.getValues(ModelsOf.tp).nonEmpty => Nil
      case nm:NestedModule => moduleInner(nm.module.asInstanceOf[Theory])
      case o => declInner(o)
    }}</div>
  </div>

  private def declTop(path:GlobalName)(implicit state: State): NodeSeq =
    <div><h1>Symbol {path.toString}</h1> <hr/>{
      controller.getO(path) match {
      case Some(d: Declaration) => declInner(d)
      case None => <div>Not found</div>
    }}</div>

  private def declTop(d:Declaration)(implicit state: State): NodeSeq =
    <div><h1>Symbol {d.path.toString}</h1><hr/>{declInner(d)}</div>

  private def declInner(path: GlobalName)(implicit state: State): NodeSeq = {
    controller.getO(path) match {
      case Some(d: Declaration) => declInner(d)
      case None =>
        collapsible(true) {doLink(path) {
            <div>Symbol {path.name.toString}</div>
        }} {<div>Not found</div>}
    }
  }

  private def declInner(d:Declaration)(implicit state: State): NodeSeq = {
    d match {
      case c : Constant if c.metadata.getValues(ModelsOf.sym).nonEmpty =>
        <span>TODO: Math Structure</span>
      case c : Constant => doSymbol(c)
      case _ => <span>TODO: {d.getClass.toString} {d.path}</span>
    }
  }

  private def doSymbol(c:Constant)(implicit state: State): NodeSeq = {
    c.rl match {
      case s if s.contains("textsymdecl") =>
        doTextSymbol(c)
      case s if s.contains("variable") =>
        <span>TODO: Variable</span>
      case _ =>
        lazy val header = <span style="display:inline;">Symbol {symbolsyntax(c)}</span>
        (c.tp,c.df,getNotationsC(c)) match {
          case (None,None,Nil) => fakeCollapsible(true){header}
          case _ => collapsible(false, true) {header}{symbolTable(c)}
        }
    }
  }

  private def doTextSymbol(c:Constant)(implicit state: State): NodeSeq = {
    val notations = getNotationsC(c)
    lazy val header = <span style="display:inline;">Text Symbol {symbolsyntax(c)}: {
      notations match {
        case Nil => <span>(Notation missing)</span>
        case ls => <math xmlns={HTMLParser.ns_mml}>{ls.head.present(Nil)}</math>
      }}
    </span>
    (c.tp,c.df) match {
      case (None,None) if notations.length <= 1 =>
        fakeCollapsible(true){header}
      case _ => collapsible(false,true){header}{symbolTable(c,false)}
    }
  }

  def symbolTable(c: Constant, donotations:Boolean=true)(implicit state: State) =
    <table class="symbol-table">{c.df match {
      case Some(df) =>
        <tr>
          <td class="symbol-td">Definiens</td>
          <td class="symbol-td">{xhtmlPresenter.asXML(df, Some(c.path $ DefComponent))}</td>
        </tr>
      case None =>
    }}{c.tp match {
      case Some(tp) =>
        <tr>
          <td class="symbol-td">Type</td>
          <td class="symbol-td">{xhtmlPresenter.asXML(tp, Some(c.path $ TypeComponent))}</td>
        </tr>
      case None =>
    }}{getNotationsC(c) match {
      case Nil =>
      case List(a) if !donotations =>
      case nls =>
        val ls = if (donotations) nls else nls.tail
        val arity = getArity(c).getOrElse("")
        <tr>
          <td class="symbol-td">Notations</td> <td class="symbol-td">
          <table class="notation-table">
            <tr>
              <th class="notation-td">id</th>
              <th class="notation-td">notation</th>{if (arity.nonEmpty) <th class="notation-td">operator</th>}<th class="notation-td">in module</th>
            </tr>{val args = withArguments((getI, getX) => arity.map {
            case 'i' => List(<mi>{getI}</mi>)
            case 'b' => List(<mi>{getX}</mi>)
            case 'a' => val a = getI; List(
                <msub><mi>{a}</mi><mn>1</mn></msub>,
                <mo>...</mo>,
                <msub><mi>{a}</mi><mi>n</mi></msub>)
            case 'B' =>
              val x = getX; List(
                <msub><mi>{x}</mi><mn>1</mn></msub>,
                <mo>...</mo>,
                <msub><mi>{x}</mi><mi>n</mi></msub>)
          }).toList
          ls.map { not =>
            <tr>
              <td class="notation-td">{not.id}</td>
              <td class="notation-td">
                <math xmlns={HTMLParser.ns_mml}>{present(not.present(args))}</math>
              </td>
              {if (arity.nonEmpty) <td class="notation-td">{
                not.op.map(n =>
                  <math xmlns={HTMLParser.ns_mml}>{n.plain.node}</math>
                ).getOrElse(<span>(None)</span>)
              }</td>}
              <td class="notation-td">{
                not.in.map(t => doLink(t.path)(<span>{t.name}</span>)).getOrElse(<span></span>)
                }</td>
            </tr>
          }}
          </table></td></tr>
    }}</table>

  def symbolsyntax(c:Constant)(implicit state: State) = <span>
    <pre style="display:inline;font-size:smaller;">{doLink(c.path)(<span>{c.name}</span>)}</pre>
    {(getMacroName(c), getArity(c)) match {
      case (Some(mn), None | Some("")) =>
        <span> (<pre style="display:inline;font-size:smaller;">\{mn}</pre>)</span>
      case (Some(mn), Some(args)) =>
        <span> (<pre style="display:inline;font-size:smaller;">\{mn}{
            withArguments((getI, getX) => args.map {
                case 'i' => s"{${getI}}"
                case 'a' => s"{${val a = getI; a + "_1,...," + a + "_n"}}"
                case 'b' => s"{${getX}}"
                case 'B' => s"{${val a = getX; a + "_1,...," + a + "_n"}}"
              }.mkString)
            }</pre>)</span>
      case _ => <span></span>
    }}</span>
  private def withArguments[A](f: (=> Char, => Char) => A): A = {
    var iidx = -1
    var vidx = -1
    val is = "abcdefghi"
    val xs = "xyzvwustr"
    def getI = {iidx += 1; is(iidx)}
    def getX = {vidx += 1; xs(vidx)}
    f(getI, getX)
  }

  private def doLink(p: Path)(txt: => NodeSeq)(implicit state: State) =
    <a href={s"/:${this.pathPrefix}/omdoc?${p.toString}&language=${state.language}"} style="color:blue;">{txt}</a>

  private def collapsible(expanded:Boolean = true,small:Boolean=false)(title: => NodeSeq)(content: =>NodeSeq)(implicit state:State) = {
    val id = state.getId
    <div class={if (small) "collapsible collapsible-small" else "collapsible"}>
      {if (expanded) <input type="checkbox" name={id} id={id} checked="checked"/>
        else <input type="checkbox" name={id} id={id}/>
      }
      <div class="handle"><label for={id}>{title}</label></div>
      <div class="content">{content}</div>
    </div>
  }

  private def fakeCollapsible(small: Boolean = false)(title: => NodeSeq)(implicit state: State) =
    <div class={if (small) "fake-collapsible-small" else "fake-collapsible"}>{title}</div>

  private def present(n:NodeSeq):Node = present(n.toString())(None).plain.node

  private def doBarBlock(title:NodeSeq)(content: => NodeSeq): Node = {
    <div style="border-left:2px solid black;padding-left:5px;margin-top:15px;margin-bottom:15px">
      <div style="width:100%;position:relative;border-bottom:1px solid black;border-top:1px solid black;font-weight:bold;margin-bottom:5px;font-size:larger">
        {title}
      </div>
      <div style="margin-left:20px;width:100%;margin-top:5px">{content}</div>
    </div>
  }

  private def doBlock(title: NodeSeq)(content: => NodeSeq): Node = {
    <div style="width:100%">
      <div style="width:100%">{title}</div>
      <div style="width:100%;margin-left:20px">{content}</div>
    </div>
  }
/*
  def doModuleBody(t : Theory,lang:Option[String]) : NodeSeq = {
    t.getPrimitiveDeclarations.map {
      case c: Constant if c.metadata.get(ModelsOf.sym).nonEmpty =>
          <span/>
      case c: Constant =>
        //doInputref(c.path, lang)
        omdocSymbol(c, lang)
      case Include(i) =>
        <div style="width:100%">
          <b>Include</b>
          <a href={doLink(i.from, lang)} style="color:blue">?{i.from.name.toString}</a>
        </div>
      case nm: NestedModule =>
        nm.metadata.getValues(ModelsOf.tp).headOption match {
          case Some(OMS(const)) =>
            controller.getO(const) match {
              case Some(c: Constant) =>
                omdocStructure(nm.module.asInstanceOf[Theory], c, lang)
              case None =>
                doBarBlock(<span>Structure {const.name}</span>){
                  <span>Structure Not Found</span>
                }
            }
          case _ =>
            <div style="width:100%">TODO: Nested Module</div>
        }
      case o =>
        <div style="width:100%">TODO:
          {o.getClass}
        </div>
    }
  }


  // val state = new ParsingState(controller, presentationRules)
  private def omdocSymbol(gn:GlobalName, lang: Option[String]): Node = {
    controller.getO(gn) match {
      case Some(c : Constant) => omdocSymbol(c,lang)
      case Some(o) =>
        <div style="width:100%">TODO:{o.getClass}</div>
      case None => <div style="width:100%;margin-bottom:15px;margin-top:25px">
        <div style="width:100%;font-weight:bold;margin-bottom:15px">{gn.name.toString}</div>
        <span>Symbol not found</span>
      </div>
    }
  }

  private def makeRow(first:NodeSeq)(second:NodeSeq) = <tr>
    <td style="width:1%;padding-right:5px;padding-bottom:2px;vertical-align:top">{first}</td>
    <td style="width:100%;padding-left:5px;padding-bottom:2px;vertical-align:top">{second}</td>
  </tr>

  private def omdocStructure(th:Theory,const:Constant,lang:Option[String]) = {
    doBarBlock(<span>Structure <a href={doLink(const.path,lang)} style="color:blue">{const.name}</a></span>){
      <table style="width:100%;margin-left:30px">
        {this.getMacroName(const) match {
          case Some(mn) => makeRow(<span>macro:</span>)(<code>\{mn}</code>)
          case None =>
        }}
        {
          def getIncls(t: Theory): List[MPath] = t.getIncludesWithoutMeta.flatMap(p => p :: controller.getO(p).map {
            case t : Theory => getIncls(t)
            case _ => Nil
          }.getOrElse(Nil))
          val includes = getIncls(th)
          if (includes.nonEmpty) {
            makeRow(<span>Extends:</span>)(includes.map(p => <span style="display:inline"> <a href={doLink(p, lang)} style="color:blue">?{p.name.toString}</a></span>))
          } else <span/>
        }
        {
          th.getPrimitiveDeclarations.flatMap {
            case c : Constant =>
              val macroname = this.getMacroName(c)
              Some(makeRow(<span>Field {if (macroname.isDefined) macroname.get else c.name}</span>){
                val arity = this.getArity(c)
                <table style="width:100%;margin-left:30px">
                  {if (macroname.nonEmpty && !macroname.contains(c.name.toString)) {
                    makeRow(<span>Name:</span>)(<span>{c.name}</span>)
                  }}
                  {withArguments { (getI, getX) =>
                  arity match {
                    case None | Some("") => <span/>
                    case Some(args) =>
                      makeRow(<span>Arguments:</span>)(<code>{args map {
                          case 'i' => "{" + getI + "}"
                          case 'a' => "{" + {
                            val a = getI;
                            a + "_1,...," + a + "_n"
                          } + "}"
                          case 'b' => "{" + getX + "}"
                          case 'B' => "{" + {
                            val x = getX;
                            x + "_1,...," + x + "_n"
                          } + "}"
                        }}
                      </code>)
                    case _ =>
                  }}}
                {arity.map(a => doNotations(c, a, lang)).getOrElse(doNotations(c, "", lang))}
                {c.tp.map(tp => makeRow(<span>Type:</span>)(xhtmlPresenter.asXML(tp,Some(c.path $ TypeComponent)))).getOrElse()}
                {c.df.map(df => makeRow(<span>Definiens:</span>)(xhtmlPresenter.asXML(df, Some(c.path $ DefComponent)))).getOrElse()}
                </table>
              })
            case Include(_) => None
            case s : Structure =>
              doStructure(s,lang)
            case o =>
              <span>TODO: {o.getClass}</span>
          }
        }
        {makeRow(<span/>)(<span>TODO</span>)}
      </table>
    }
  }

  private def doStructure(s : Structure,lang:Option[String]) : NodeSeq = {
    <span>TODO structure</span>
  }



  private def omdocSymbol(c:Constant,lang:Option[String]): Node = doBlock(
    <b>Symbol <a href={doLink(c.path,lang)} style="color:blue">{c.name.toString}</a></b>){
      val arity = this.getArity(c)
      <table style="width:100%;margin-left:30px">
        {withArguments{(getI,getX) => (this.getMacroName(c), arity) match {
          case (Some(mn), None | Some("")) if mn.forall(_.isLetter) =>
            makeRow(<span>Syntax:</span>)(<code>\{mn}</code>)
          case (Some(mn), Some(args)) if mn.forall(_.isLetter) =>
            makeRow(<span>Syntax:</span>)(<code>\{mn}{args map {
              case 'i' => "{" + getI + "}"
              case 'a' => "{" + {
                val a = getI; a + "_1,...," + a + "_n"
              } + "}"
              case 'b' => "{" + getX + "}"
              case 'B' => "{" + {
                val x = getX; x + "_1,...," + x + "_n"
              } + "}"
            }}</code>)
          case _ =>
        }}}
        {arity.map(a => doNotations(c,a,lang)).getOrElse(doNotations(c,"",lang))}
        {c.tp.map(tp => makeRow(<span>Type:</span>)(xhtmlPresenter.asXML(tp, Some(c.path $ TypeComponent)))).getOrElse()}
        {c.df.map(df => makeRow(<span>Definiens:</span>)(xhtmlPresenter.asXML(df, Some(c.path $ DefComponent)))).getOrElse()}
      </table>
    }

  private def doOMSNotation(not : STeXNotation,doin : Boolean,lang:Option[String]) = <tr>
      <td style="padding-right:5px;padding-left:5px;text-align:center">{not.id}</td>
      <td style="padding-right:5px;padding-left:5px;text-align:center"><math xmlns={HTMLParser.ns_mml}><mrow>{not.present(Nil)}</mrow></math></td>
      <td style="padding-right:5px;padding-left:5px;text-align:center">{if (doin) <a href={doLink(not.in.get.path, lang)} style="color:blue">?{not.in.get.path.name}</a>}</td>
    </tr>
  private def doComplexNotation(not : STeXNotation,args:List[List[Node]],doin : Boolean,lang:Option[String]) = <tr>
    <td style="padding-right:5px;padding-left:5px;text-align:center">{not.id}</td>
    <td style="padding-right:5px;padding-left:5px;text-align:center"><math xmlns={HTMLParser.ns_mml}><mrow>{not.present(args)}</mrow></math></td>
    <td style="padding-right:5px;padding-left:5px;text-align:center">{not.op match {
      case Some(n) => <math xmlns={HTMLParser.ns_mml}><mrow>{n.plain.node}</mrow></math>
      case _ => "(None)"
    }}</td>
    <td style="padding-right:5px;padding-left:5px;text-align:center">{if (doin) <a href={doLink(not.in.get.path, lang)} style="color:blue">?{not.in.get.path.name}</a>}</td>
  </tr>
  private def doNotations(c : Constant,arity:String,lang:Option[String]) : NodeSeq = {
    if (arity.isEmpty) {
      this.getNotations(c) match {
        case Nil => <span/>
        case ls =>
          makeRow(<span>Notations:</span>) {
            <table>
              <tr>
                <th style="padding-right:5px;padding-left:5px;text-align:center">id</th>
                <th style="padding-right:5px;padding-left:5px;text-align:center">notation</th>
                <th style="padding-right:5px;padding-left:5px;text-align:center">in</th>
              </tr>
              {
                val ins = ls.collect { case n if n.in.get.path == c.path.module => n }
                ins.map { doOMSNotation(_, false, lang) }
              }
              {
                val ins = ls.collect { case n if n.in.get.path != c.path.module => n }
                ins.map { doOMSNotation(_,true,lang)}
              }
            </table>
          }
      }
    }
    else {
      val args = withArguments { (getI, getX) =>
        arity.map {
          case 'i' => List(<mi>{getI}</mi>)
          case 'b' => List(<mi>{getX}</mi>)
          case 'a' =>
            val a = getI
            List((<msub><mi>{a}</mi> <mn>1</mn></msub>), (<mo>...</mo>), (<msub><mi>{a}</mi> <mi>n</mi></msub>))
          case 'B' =>
            val x = getX
            List((<msub><mi>{x}</mi> <mn>1</mn></msub>), (<mo>...</mo>), (<msub><mi>{x}</mi> <mi>n</mi></msub>))
        }
      }.toList
      this.getNotations(c) match {
        case Nil => <span/>
        case ls =>
          makeRow(<span>Notations:</span>) {
            <table>
              <tr>
                <th style="padding-right:5px;padding-left:5px;text-align:center">id</th>
                <th style="padding-right:5px;padding-left:5px;text-align:center">notation</th>
                <th style="padding-right:5px;padding-left:5px;text-align:center">operator</th>
                <th>in</th>
              </tr>
              {
                val ins = ls.collect { case n if n.in.get.path == c.path.module => n }
                ins.map { doComplexNotation(_,args,false,lang)}
              }
              {
                val ins = ls.collect { case n if n.in.get.path != c.path.module => n }
                ins.map { doComplexNotation(_,args,true,lang)}
              }
            </table>
          }
      }
    }
  }
 */

  lazy val xhtmlPresenter = controller.extman.get(classOf[STeXPresenterML]) match {
    case p :: _ => p
    case Nil =>
      val p = new STeXPresenterML
      controller.extman.addExtension(p)
      p
  }
  lazy val presenter = new Presenter(xhtmlPresenter) {
    val key: String = "stexpres"

    def applyPath(p: Path, standalone: Boolean = true)(implicit rh: RenderingHandler) = {
      controller.getO(p) match {
        case Some(d) => apply(d, standalone)
        case None => rh.apply("Not found: " + p.toString)
      }
    }
    override def apply(e: StructuralElement, standalone: Boolean)(implicit rh: RenderingHandler): Unit = {
      implicit val state = new State("en")
      e match {
        case d : Document => rh(documentTop(d))
        case m : Theory => rh(moduleTop(m))
        case d : Declaration => rh(declTop(d))
      }
    }

    def doHistories(cp:CPath,h:History*) = {
      implicit val sb = new StringBuilder()
      sb.append(
      "<div style=\"font-weight:bold;\">" + cp.toString + "</div><hr/>"
      )
      h.foreach{h =>
        sb.append("<ul>")
        new HistoryTree().from(h).present
        sb.append("</ul>")
      }
      sb.mkString
    }

    import info.kwarc.mmt.api.checking._
    import info.kwarc.mmt.api.utils.{Union,Left,Right}

    val self = this
    private class HistoryTree(var steps : List[Union[HistoryTree,HistoryEntry]] = Nil,val lvl : Int = 0) {
      def from(h:History) = {
        doSteps(h.getSteps.map(Right(_)))
        steps match {
          case List(Left(ht)) => ht
          case _ =>
            this
        }
      }
      private def doSteps(ls : List[Union[HistoryTree,HistoryEntry]]) : Unit = {
        var ils = ls
        def doNested(top:Union[HistoryTree,HistoryEntry],lvl:Int) = {
          var done = false
          var ret : List[Union[HistoryTree,HistoryEntry]] = List(top)
          var endlvl = lvl
          while(!done && ils.nonEmpty) {
            val h :: t = ils
            ils = t
            h match {
              case Left(ht) if ht.lvl < lvl =>
                endlvl = ht.lvl
                ret ::= Left(ht)
                done = true
              case Right(IndentedHistoryEntry(e,l)) if l < lvl =>
                endlvl = l
                ret ::= Right(e)
                done = true
              case Right(IndentedHistoryEntry(e,_)) =>
                ret ::= Right(e)
              case o =>
                ret ::= o
            }
          }
          ils ::= Left(new HistoryTree(ret,endlvl))
        }
        while (ils.nonEmpty) {
          val h :: t = ils
          ils = t
          if (t.isEmpty) steps ::= h else {
            h match {
              case Left(ht) if ht.lvl > 0 =>
                doNested(Left(ht), ht.lvl)
              case Right(IndentedHistoryEntry(e, lvl)) if lvl > 0 =>
                doNested(Right(e), lvl)
              case Right(IndentedHistoryEntry(e, _)) =>
                steps ::= Right(e)
              case o =>
                steps ::= o
            }
          }
        }
      }

      def present(implicit sb: StringBuilder): Unit = {
        sb.append("<li><div>")
        steps.head match {
          case Left(ht) =>
            ht.present
          case Right(st) =>
            sb.append(st.present(self.objectLevel.asString(_)))
        }
        sb.append("</div>")
        sb.append("<ul>")
        steps.tail.foreach {
          case Left(ht) =>
            ht.present
          case Right(st) =>
            sb.append("<li><div>")
            sb.append(st.present(self.objectLevel.asString(_)))
            sb.append("</div></li>")
        }
        sb.append("</ul></li>")
      }
    }
  }

  lazy val texPresenter = controller.extman.get(classOf[STeXPresenterTex]) match {
    case p :: _ => p
    case Nil =>
      val p = new STeXPresenterTex
      controller.extman.addExtension(p)
      p
  }

}
