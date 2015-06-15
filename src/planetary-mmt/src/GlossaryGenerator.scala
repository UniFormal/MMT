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
          case d : DeclaredTheory => Some(d)
          case _ =>  None
        }
      } catch {
        case e : Error => None
        case e : Exception => None
      }
    }
    val verbs = modules collect {
      case thy: DeclaredTheory =>
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
            rh(<a data-target={ "#gtab_" + p._1 } style="cursor: pointer;" onclick={ "jQuery(this).tab('show');" }> { p._1 } </a>)
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

  private def present(lang : String, spath : GlobalName, not: TextNotation): Unit = {
    val doc = spath.doc
    val mod = spath.module.toMPath.name
    val name = spath.name
    val constant = controller.library.getConstant(spath)
    val alternatives = constant.notC.verbalizationDim.notations.values.flatten.flatMap(_.scope.languages.filter(_ != lang)).toSet
    
    val notations = (constant.notC.parsingDim.notations.values.flatten ++ constant.notC.presentationDim.notations.values.flatten).map(n => spath -> n).toList.distinct
    
    val defs = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy)) map {
          case p: Path =>
            controller.get(p) match {
              case fd: Constant =>
                val mpath = fd.home.toMPath
                if (sTeX.getLanguage(mpath) == Some(lang)) {
                  Some(fd)
                } else {
                  None
                }
              case _ => None
            }
        }
    if (defs.isEmpty) {
      return;
    }
    
    li(cls = "entry") {
      div {
        span(cls="name keyword", attributes=List("id" -> (presenter.encPath(spath) + "_" + lang))) {
          span {
            presenter.doNotationRendering(spath, not)
          }
        }
        //addings defs
        
        presenter.doNotations(notations, spath, lang)
        for (lang <- alternatives) {
          try {
            val altId = presenter.encPath(spath) + "_" + lang;
            rh(<a style="cursor: pointer;" onclick={ "$('#glossary a[data-target=\\\'#gtab_" + lang + "\\\']').tab('show'); window.location.href = \'#" + altId + "\';" + "jQuery(\'#" + altId + "\').parent().effect(\'highlight\', {}, 1500);" }> { lang } </a>)
          } catch {
            case e: Error => //invalid path, nothing to do
            //           rh(<a href="#" style="cursor: pointer;" onclick={"$('#glossary a[\\\'data-toggle=#gtab_" + lang + "\\\']').tab('show');"}> {lang} </a>)
          }
        }
        defs.flatten foreach { fd =>
          div(cls = "hidden", attributes = ("id" -> ("def_" + spath.toPath + "_" + fd.path.toPath)) :: Nil) {
            presenter(fd)(rh)
          }
        }
      }
      rh("<hr/>")
    }
  }

}