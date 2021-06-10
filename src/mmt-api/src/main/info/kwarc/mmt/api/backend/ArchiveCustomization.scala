package info.kwarc.mmt.api.backend
import scala.xml.{Elem, Node}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.ContentMathMLPresenter

/** Helper class to permit customizing archives */
abstract class ArchiveCustomization {
  def mwsurl(p: Path) : String
  /**
   * Any post-processing on the query object should be done here
   */
  def prepareQuery(t: Obj): scala.xml.Node
}

class DefaultCustomization extends ArchiveCustomization {
  def mwsurl(p: Path) : String = p.toPath
  def prepareQuery(t: Obj): scala.xml.Node = ContentMathMLPresenter(t)
}

/** Customization for Mizar */
class MML extends ArchiveCustomization {
  def mwsurl(p: Path) : String = p match {
    case par ?? ln =>
      val thyName = par.name.toPath
      val symbolName = ln.head.toPath
      "http://www.mizar.org/version/current/html/" + thyName.toLowerCase() + ".html#" + symbolName
    case _ => throw ImplementationError("Module level references not implemented yet")
      // "http://www.mizar.org/version/current/html/" + "%m.html#o"
  }

  def prepareQuery(t: Obj): scala.xml.Node = {
    makeHVars(removeLFApp(ContentMathMLPresenter(t)), Nil, Nil, true)
  }

  private def removeLFApp(n : scala.xml.Node) : scala.xml.Node = n match {
    case <apply><csymbol>{x}</csymbol>{s @ _*}</apply> =>
      if (x.toString == "http://cds.omdoc.org/foundational?LF?@")
        <apply>{s.map(removeLFApp)}</apply>
      else {
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(removeLFApp) : _*)
      }
    case _ =>
      if (n.child.length == 0)
        n
      else
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(removeLFApp(_)) : _*)
  }

  private def makeHVars(n : scala.xml.Node, uvars : List[String], evars : List[String], negFlag : Boolean) : scala.xml.Node = n match {
    case <apply><csymbol>{s}</csymbol><bvar><apply>{zz}<ci>{v}</ci>{a}{b}</apply>{rest @ _*}</bvar>{body}</apply> =>
      if (s.toString == "http://cds.omdoc.org/foundational?LF?Pi") {//"http://cds.omdoc.org/foundations/lf/lf.omdoc?lf?Pi") {

        var bd = body
        if (rest.length > 0)
          bd = <apply><csymbol>{s}</csymbol><bvar>{rest}</bvar>{body}</apply>

        makeHVars(bd, v.toString :: uvars, evars, negFlag)
      }
      else {
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)
      }

    case <apply><apply><csymbol>{s}</csymbol>{a1}</apply><bvar><apply>{zz}<ci>{v}</ci>{a}{b}</apply></bvar>{body}</apply> =>
      if (s.toString == "http://latin.omdoc.org/foundations/mizar?mizar-curry?for") {
        var uv = uvars
        var ev = evars
        if (negFlag)
          uv = v.toString :: uvars
        else
          ev = v.toString :: evars
        makeHVars(body, uv, ev, negFlag)
      } else {
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)
      }
    case <apply><csymbol>{s}</csymbol>{a}</apply> =>
      if (s.toString == "http://latin.omdoc.org/foundations/mizar?mizar-curry?not") {
        val firstq = s.toString.indexOf('?')
        <apply><csymbol>{s.toString.substring(firstq + 1)}</csymbol>{makeHVars(a, uvars, evars, !negFlag)}</apply>
      } else
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)

    case <ci>{v}</ci> =>
      if (uvars.contains(v.toString))
        <mws:uvar><ci>{v}</ci></mws:uvar>
      else if (evars.contains(v.toString))
        <mws:evar><ci>{v}</ci></mws:evar>
      else
        n
    case <csymbol>{p}</csymbol> =>
      val firstq = p.toString.indexOf('?')
      <csymbol>{p.toString.substring(firstq + 1)}</csymbol>

    case _ =>
      if (n.child.length == 0)
        n
      else
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)
  }
}

/**
 * TPTP customization
 */
class TPTP extends ArchiveCustomization {
  def mwsurl(p: Path) : String = p.toPath

  /**
   * Traverse the node, fixing symbol names and translating $$ terms to qvar
   */
  def prepareQuery(t: Obj): Node = process(ContentMathMLPresenter(t))
  def process(n: Node) : Node = n match {
      case <csymbol>{s}</csymbol> =>
        val ss = getSymbolName(s.toString)
        if (isQvar(ss)) {
          <mws:qvar>{ss}</mws:qvar>
        } else {
          <csymbol>{ss}</csymbol>
        }
      case <ci>{s}</ci> =>
        val ss = s.toString
        if (isQvar(ss)) {
          <mws:qvar>{ss}</mws:qvar>
        } else {
          <ci>{ss}</ci>
        }
      case n =>
        if (n.child.length == 0)
          n
        else
          new Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(process(_)) : _*)
  }

  /**
   * Strips csymbol names to contain only the symbol name
   */
  def getSymbolName(ss: String): String = {
    val ind = ss.lastIndexOf("?")
    if (ind != -1) {
      if (ind == ss.length - 1)
        return "?"
      else
        return ss.substring(ind + 1, ss.length)
    }
    return ss
  }

  /**
   * qvars start with $$ followed by lower case character
   */
  def isQvar(ss: String): Boolean = ss.startsWith("$$")
}
