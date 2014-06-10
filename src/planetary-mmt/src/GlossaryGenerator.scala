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
import flexiformal._
import archives._
import notations._
import scala.xml.{Node }
import ontology.{ isDefinedBy }
import info.kwarc.mmt.stex._

object GlossaryGenerator {
  private var presenter: PlanetaryPresenter = null
  private var controller: Controller = null
  private var rh: StringBuilder = null
  private var langList = "en" :: "de" :: Nil
  def generate(controller: Controller): String = {
    this.controller = controller
    this.presenter = controller.extman.getPresenter("planetary") match {
      case Some(p: PlanetaryPresenter) => p
      case _ => throw new Exception("Expected planetary presenter to be loaded")
    }
    this.rh = new StringBuilder
    presenter.setRh(rh)
    val modules = controller.library.getModules
    val verbs = modules collect {
      case thy: DeclaredTheory =>
        thy.getDeclarations collect {
          case c: Constant if c.notC.verbalizationDim.isDefined =>
            c.notC.verbalizationDim.notations.values.flatten.map(c.path -> _)
        }
    }
    present(verbs.flatten.flatten)
    rh.get
  }

  // easy-to-use HTML markup
  protected val htmlRh = utils.HTML(s => rh(s))
  import htmlRh._

  private def present(verbs: Iterable[(GlobalName, TextNotation)]): Unit = {
    val langVerbs = verbs.groupBy[String](p => sTeX.getLanguage(p._1).getOrElse(""))
    def getCls(lang: String) = if (lang == "en") "active" else ""
    div(attributes = List("id" -> "glossary")) {
      ul("nav nav-tabs") {
        langVerbs.foreach { p =>
          li(getCls(p._1)) {
            rh(<a data-target={ "#gtab_" + p._1 } style="cursor: pointer;" onclick={ "$(this).tab('show');" }> { p._1 } </a>)
          }
        }
      }
      div("tab-content") {
        langVerbs.foreach { p =>
          div(cls = ("tab-pane " + getCls(p._1)), attributes = List("id" -> ("gtab_" + p._1))) {
            ul("glossary") {
              val glossary = p._2.toList.sortWith((x,y) => x._2.markers.mkString(" ").toLowerCase() < y._2.markers.mkString(" ").toLowerCase())
              glossary.foreach(v => present(v._1, v._2))
            }
          }
        }
      }
    }
  }

  private def present(spath : GlobalName, not: TextNotation): Unit = {

    val doc = spath.doc
    val mod = spath.module.toMPath.name
    val name = spath.name
    val lang = sTeX.getLanguage(spath)
    val masterPath = sTeX.getMasterPath(spath)
    val alternatives = langList.filter(l => Some(l) != lang) map { l =>
      (l -> sTeX.getLangPath(masterPath, l))
    }
    val notations = try {
      controller.library.getConstant(masterPath).notC.getAllNotations.map(n => masterPath -> n)
    } catch {
      case e: Throwable => Nil
    }
    val defs = controller.depstore.getObjects(spath, isDefinedBy) collect {
          case fp: FragPath if fp.isPath =>
            controller.get(fp.path) match {
              case fd: Definition =>
                val thyName = fd.home.toMPath.name.toPath
                val parts = thyName.split('.')
                if (parts.length == 2) {
                  Some(fd)
                } else {
                  None
                }
              case _ => None
            }
        }
    if (defs.isEmpty)
      return;
    
    li(cls = "entry") {
      div {
        span(cls="name keyword", attributes=List("id" -> presenter.encPath(spath))) { 
          val smks = not.markers map {
            case d : Delimiter => d.text
            case x => x.toString //TODO
          }
          text(smks.mkString(" ")) 
        }
        //addings defs
        
        presenter.doNotations(notations, spath)

        for (alt <- alternatives) {
          val lang = alt._1
          val path = alt._2
          try {
            controller.get(path)
            rh(<a style="cursor: pointer;" onclick={ "$('#glossary a[data-target=\\\'#gtab_" + lang + "\\\']').tab('show'); window.location.href = \'#" + presenter.encPath(path) + "\';" }> { lang } </a>)
          } catch {
            case e: Error => //invalid path, nothing to do
            //           rh(<a href="#" style="cursor: pointer;" onclick={"$('#glossary a[\\\'data-toggle=#gtab_" + lang + "\\\']').tab('show');"}> {lang} </a>)
          }
        }
        defs.flatten foreach { fd =>
          div(attributes = ("id" -> ("def_" + spath.toPath + "_" + fd.path.toPath)) :: Nil) {
            presenter(fd)(rh)
          }
        }
      }
      rh("<hr/>")
    }
  }

}