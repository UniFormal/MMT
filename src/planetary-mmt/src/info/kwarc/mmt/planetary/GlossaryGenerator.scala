package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._
import presentation._
import frontend._
import libraries._
import objects._
import utils._
import informal._
import archives._
import notations._
import scala.xml.{Node }
import ontology._
import info.kwarc.mmt.stex._

object GlossaryGenerator {

  private var counter = 0
  def getNewId : String = {
    counter += 1
    "gs_" + counter.toString
  }
  private var presenter: PlanetaryPresenter = null
  private var controller: Controller = null
  private var rh: StringBuilder = null
  def generate(controller: Controller): String = {
    this.controller = controller
    this.presenter = controller.extman.get(classOf[Presenter], "planetary") match {
      case Some(p: PlanetaryPresenter) => p
      case _ => throw new Exception("Expected planetary presenter to be loaded")
    }
    this.rh = new StringBuilder
    presenter.setRh(rh)

    val mpaths = controller.depstore.getInds(ontology.IsTheory).toList
    val modules = mpaths flatMap { p =>
      try {
        controller.get(p) match {
          case d: Theory => Some(d)
          case _ =>  None
        }
      } catch {
        case e : Error => None
        case e : Exception => None
      }
    }
    val verbs = modules collect {
      case thy: Theory =>
        thy.getDeclarations collect {
          case c: Constant if c.notC.verbalizationDim.isDefined =>
            c.notC.verbalizationDim.notations.values.flatten.map(c.path -> _)
        }
    }
    present(verbs.flatten.flatten.toList.distinct)
    rh.get
  }

  // easy-to-use HTML markup
  protected val htmlRh = utils.HTML(s => rh(s))
  import htmlRh._

  private def makeString(not : TextNotation) : String = {
    val smks = not.markers map {
      case d : Delimiter => d.text
      case m => m.toString
    }
    smks.mkString(" ")
  }

  private def present(verbs: Iterable[(GlobalName, TextNotation)]): Unit = {
    val items = new collection.mutable.HashMap[String, List[(GlobalName, TextNotation)]]
    verbs foreach {p => p._2.scope.languages.foreach { lang =>
     if (lang != "") {
       if (!items.contains(lang)) {
         items(lang) = Nil
       }
       items(lang) ::= p
     }
    }}
    def getCls(lang: String) = if (lang == "en") "active" else ""
    div(attributes = List("id" -> "glossary")) {
      ul("nav nav-tabs") {
        items.foreach { p =>
          li(getCls(p._1)) {
            rh(<a data-target={ "#gtab_" + p._1 } class="gs_tab"> { p._1 } </a>)
          }
        }
      }
      div("tab-content") {
        items.foreach { p =>
          div(cls = ("tab-pane " + getCls(p._1)), attributes = List("id" -> ("gtab_" + p._1))) {
            ul("glossary") {
              val glossary = p._2.toList.sortWith((x,y) => makeString(x._2).toLowerCase() < makeString(y._2).toLowerCase())
              glossary.foreach(v => present(p._1, v._1, v._2))
            }
          }
        }
      }
    }
  }

  private def getVerbalizations(path: MPath, lang: String) : List[TextNotation] = {

    //getPrimarySym(path : MPath) : Option[GlobalName]
    val primarySyms = controller.depstore.getInds(IsPrimarySymbol)
    val primarySymO = primarySyms.find {
      case p : GlobalName => p.module == path
      case _ => false
    }

    //getVerbs(spath : Option[GlobalName], lang : String) : List[TextNotation]
    primarySymO.map(controller.get) match {
      case Some(c:Constant) => c.notC.verbalizationDim.get(lang=Some(lang))
      case _ => Nil
    }
  }

  private def present(lang : String, spath : GlobalName, not: TextNotation): Unit = {
    val doc = spath.doc
    val mod = spath.module.name
    val name = spath.name
    val constant = controller.library.getConstant(spath)
    val alternatives = constant.notC.verbalizationDim.notations.values.flatten.flatMap(_.scope.languages.filter(_ != lang)).toSet

    val notations = (constant.notC.parsingDim.notations.values.flatten ++ constant.notC.presentationDim.notations.values.flatten).map(n => spath -> n).toList.distinct

    val defs = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy)) flatMap {
          case p: Path =>
            controller.getO(p) match {
              case Some(fd: Constant) =>
                val mpath = fd.home.toMPath
                if (sTeX.getLanguage(mpath) == Some(lang)) {
                  Some(fd)
                } else {
                  None
                }
              case _ => None
            }
        }

    val primarySym = controller.depstore.queryList(spath, HasType(IsPrimarySymbol))
    val hypernyms = if  (!primarySym.isEmpty) {
      val mpath = spath.module
      controller.depstore.queryList(mpath, ToObject(IsHypernymOf)) flatMap {
        case p: Path =>
          controller.getO(p) match {
            case Some(fd: Theory) =>
              val mpath = fd.path
              val verbs = getVerbalizations(mpath, lang)
              if (verbs.isEmpty) {
                None
              } else {
                Some(verbs)
              }
            case _ => None
          }
      }
    } else Nil

    val hyponyms = if  (!primarySym.isEmpty) {
      val mpath = spath.module
      controller.depstore.queryList(mpath, ToSubject(IsHypernymOf)) flatMap {
        case p: Path =>
          controller.getO(p) match {
            case Some(fd: Theory) =>
              val mpath = fd.path
              val verbs = getVerbalizations(mpath, lang)
              if (verbs.isEmpty) {
                None
              } else {
                Some(verbs)
              }
            case _ => None
          }
      }
    } else Nil

    val synonyms = constant.notC.verbalizationDim.get(lang=Some(lang)).filter(_ != not)

    if (defs.isEmpty) {
      return;
    }

    li(cls = "entry") {
      div {
        //name
        span(cls="name keyword", attributes=List("id" -> (presenter.encPath(spath) + "_" + lang))) {
          span {
            presenter.doNotationRendering(spath, not)
          }
        }
        //Show Definition Trigger -- already returning if no def is found so no need to check
        val defId = getNewId
        presenter.doShowHideTrigger("Definition", defId)
        //Show notations Trigger
        val notId = getNewId
        if (!notations.isEmpty) {
          rh(",")
          presenter.doShowHideTrigger("Notations", notId)
        }
        val synId = getNewId
        if (!synonyms.isEmpty) {
          rh(",")
          presenter.doShowHideTrigger("Synonyms", synId)
        }
        val hypId = getNewId
        if (!hypernyms.isEmpty) {
          rh(",")
          presenter.doShowHideTrigger("Hypernyms", hypId)
        }
        val hyponId = getNewId
        if (!hyponyms.isEmpty) {
          rh(",")
          presenter.doShowHideTrigger("Hyponyms", hyponId)
        }
        //SVG
        rh(", <small><a target=\"_blank\" href=\"" + spath.doc.toPath + "!svg\"> Concept Graph </a></small>")

        // Other languages
        span(cls = "pull-right") {
          for (lang <- alternatives) {
            try {
              val altId = presenter.encPath(spath) + "_" + lang;
              rh(<a class="alt_lang" data-lang={lang} data-id={altId}> { lang } </a>)
            } catch {
              case e: Error => //invalid path, nothing to do
              //           rh(<a href="#" style="cursor: pointer;" onclick={"$('#glossary a[\\\'data-toggle=#gtab_" + lang + "\\\']').tab('show');"}> {lang} </a>)
            }
          }
        }
        // Notations Table -- hidden by default, activated by notations trigger
        presenter.doNotationsTable(notations, notId)

        // adding definition (as hidden for now)
        //("style" -> "display:none;")
        div(attributes = ("id" -> (defId)) :: ("style" -> "display:none;") :: Nil) {
          defs foreach { fd =>
            presenter.apply(fd)(rh)
          }
        }

        // adding terminological relations
        presenter.doSynonymsList(synonyms, synId, lang)
        presenter.doHypernymsList(hypernyms, hypId, lang)
        presenter.doHyponymsList(hyponyms, hyponId, lang)
      }
      rh("<hr/>")
    }
  }

}
