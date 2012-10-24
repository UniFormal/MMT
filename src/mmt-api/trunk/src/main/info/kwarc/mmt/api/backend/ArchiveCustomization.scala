package info.kwarc.mmt.api.backend
import scala.xml.{Node,Elem}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._

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
  def prepareQuery(t: Obj): scala.xml.Node = t.toCML
}

/** Customization for Mizar */
class MML extends ArchiveCustomization {
  def mwsurl(p: Path) : String = p match {
    case par % ln => 
      val thyName = par.toMPath.name.flat
      val symbolName = ln.head.toPath
      "http://www.mizar.org/version/current/html/" + thyName.toLowerCase() + ".html#" + symbolName
    case _ => throw ImplementationError("Module level references not implemented yet")
      // "http://www.mizar.org/version/current/html/" + "%m.html#o"
  }
  
  def prepareQuery(t: Obj): scala.xml.Node = {
    makeHVars(removeLFApp(t.toCML), Nil, Nil, true)
  }

  private def removeLFApp(n : scala.xml.Node) : scala.xml.Node = n match {
    case <m:apply><csymbol>{x}</csymbol>{s @ _*}</m:apply> => 
      if (x.toString == "http://cds.omdoc.org/foundational?LF?@")
        <m:apply>{s.map(removeLFApp)}</m:apply>
      else {        
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(removeLFApp) : _*)
      }
    case _ =>  
      if (n.child.length == 0)
        n
      else
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(removeLFApp(_)) : _*)
  }
  
  private def makeHVars(n : scala.xml.Node, uvars : List[String], evars : List[String], negFlag : Boolean) : scala.xml.Node = n match {
    case <m:apply><csymbol>{s}</csymbol><m:bvar><m:apply>{zz}<m:ci>{v}</m:ci>{a}{b}</m:apply>{rest @ _*}</m:bvar>{body}</m:apply> =>
      if (s.toString == "http://cds.omdoc.org/foundational?LF?Pi") {//"http://cds.omdoc.org/foundations/lf/lf.omdoc?lf?Pi") {

        var bd = body
        if (rest.length > 0)
          bd = <m:apply><csymbol>{s}</csymbol><m:bvar>{rest}</m:bvar>{body}</m:apply>
            
        makeHVars(bd, v.toString :: uvars, evars, negFlag)
      }
      else {  
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)
      }
        
    case <m:apply><m:apply><csymbol>{s}</csymbol>{a1}</m:apply><m:bvar><m:apply>{zz}<m:ci>{v}</m:ci>{a}{b}</m:apply></m:bvar>{body}</m:apply> => 
    	if (s.toString == "http://latin.omdoc.org/foundations/mizar?mizar-curry?for") {
        var uv = uvars
        var ev = evars
        if (negFlag)
          uv = v.toString :: uvars
        else
          ev = v.toString :: evars
        makeHVars(body, uv, ev, negFlag)
      } else {
    	  new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)
      }
    case <m:apply><csymbol>{s}</csymbol>{a}</m:apply> =>
      if (s.toString == "http://latin.omdoc.org/foundations/mizar?mizar-curry?not") {
        val firstq = s.toString.indexOf('?')
        <m:apply><csymbol>{s.toString.substring(firstq + 1)}</csymbol>{makeHVars(a, uvars, evars, !negFlag)}</m:apply>
      } else
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)

    case <m:ci>{v}</m:ci> => 
      if (uvars.contains(v.toString))
        <mws:uvar><m:ci>{v}</m:ci></mws:uvar>
      else if (evars.contains(v.toString))
        <mws:evar><m:ci>{v}</m:ci></mws:evar>
      else
        n
    case <csymbol>{p}</csymbol> => 
      val firstq = p.toString.indexOf('?')
      <csymbol>{p.toString.substring(firstq + 1)}</csymbol>
      
    case _ =>
      if (n.child.length == 0)
        n
      else
        new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(makeHVars(_,uvars, evars, negFlag)) : _*)
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
  def prepareQuery(t: Obj): Node = process(t.toCML)
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
          new Elem(n.prefix, n.label, n.attributes, n.scope, n.child.map(process(_)) : _*)
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