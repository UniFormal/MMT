package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{NamespaceMap, Path, TypeComponent}
import info.kwarc.mmt.api.objects.{FreeOrAny, OMA, OMS}
import info.kwarc.mmt.stex.STeX
import info.kwarc.mmt.stex.features.TheoremFeature
/*
import info.kwarc.mmt.stex.xhtml.{PreConstant, PreElement, PreFeature, PreParent, XHTMLNode, XHTMLOMDoc, XHTMLTerm}

import scala.xml.{Elem, Node}

object FeaturesExtension extends DocumentExtension {
  override def start(args: List[String]): Unit = {
    super.start(args)
    controller.extman.addExtension(new TheoremFeature)
  }

  class XHTMLTheorem(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
    def path = Path.parseS(resource,NamespaceMap.empty)
    def module = path.module
    def name = path.name
    def dpath = module.parent
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p) =>
        val f = new PreFeature(path, "stex:theorem", p)
        val thpath = path.module / (path.name.toString + "_feature")
        semanticChildren.foreach{
          case t : XHTMLTerm =>
            val c = new PreConstant(thpath ? f.newName("conclusion"),f)
            c.addRole("conclusion")
            val term = t.toTerm
            c.addDefiniens(FreeOrAny(term.freeVars,term))
          case _ =>
        }
        Some(f)
      case _ => None
    }
  }

  override lazy val xhtmlRules = List(
    {case e : Elem if e.label == "div" && e.attributes.asAttrMap.get("property").contains("stex:feature:theorem") =>
      new XHTMLTheorem(Some(e))}, // TODO
  )

  override lazy val documentRules = List(
    { case thm: XHTMLTheorem =>
      val c = controller.getConstant(thm.path)
      val sb = <div>
        {scala.xml.Text("Theorem " + thm.name.toString + ":\n")}{FragmentExtension.termLink(c.tp match {
          case Some(OMA(OMS(STeX.ded),List(a))) => a
          case _ =>
            ???
        }, Some(c.path $ TypeComponent))}
      </div>
      DocumentExtension.sidebar(thm,List(sb),atend=true)
    },
  )
}


 */