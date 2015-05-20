package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import parser._
import scala.xml._
import modules._
import symbols._
import objects._
import informal._
import utils._

object sTeX {
  def inSmglom(p : Path) : Boolean = {
    //group is smglom
    p.doc.uri.path.head == "smglom"
  }
  
  def getLanguage(p : Path) : Option[String] = {
    val name = p match {
      case s : GlobalName => s.module.toMPath.name.toPath
      case m : MPath => m.name.toPath
      case d : DPath => d.last.split('.').toList.init.mkString(".") // removing extension
      case c : CPath => c.parent.module.toMPath.name.toPath
    }
    name.split("\\.").toList match {
      case hd :: lang :: tl => Some(lang)
      case _ => None
    }
  }
  
  def getMasterPath(p : GlobalName) : GlobalName = _recursePath(p)(_getMasterName)
  def getMasterPath(p : MPath) : MPath = _recursePath(p)(_getMasterName)
  def getMasterPath(p : DPath) : DPath = _recursePath(p)(_getMasterName)
  
  def getLangPath(p : GlobalName, lang : String) : GlobalName = _recursePath(p)(_getLangName(lang))
  def getLangPath(p : MPath, lang : String) : MPath = _recursePath(p)(_getLangName(lang))
  def getLangPath(p : DPath, lang : String) : DPath = _recursePath(p)(_getLangName(lang))
  
  private def _recursePath(p : GlobalName)(f : String => String) : GlobalName = _recursePath(p.module.toMPath)(f) ? p.name.toPath
  private def _recursePath(p : MPath)(f : String => String) : MPath = _recursePath(p.doc)(f) ? f(p.name.toPath)  
  private def _recursePath(p : DPath)(f : String => String) : DPath = p.^! / f(p.last)
  
  private def _getLangName(lang : String)(s : String) : String = {
    s.split("\\.").toList match {
      case name  :: tl => (name :: lang :: tl).mkString(".")
      case _ => s
    }
  }
  
  private def _getMasterName(s : String) : String = {
    s.split("\\.").toList match {
      case name :: lang :: tl => (name :: tl).mkString(".")
      case _ => s
    }
  }
}

//COmmon OMDoc functionality that is not specific to the sTeX importer
object OMDoc {
  def parseSourceRef(n : scala.xml.Node,dpath : DPath)(implicit errorCont : ErrorHandler) : Option[SourceRef] = {
    val attrs = n.attributes.asAttrMap
    if (attrs.contains("stex:srcref")) { //try to parse the source ref
      try {
        val srcrefS = n.attributes.asAttrMap("stex:srcref")
        val trangeIdx = srcrefS.indexOf("#textrange") + "#textrange".length
        val trangeS = srcrefS.substring(trangeIdx)
        val fromto = trangeS.split(",").toList
        fromto match { //(from=4;1,to=12;16)
          case fromS :: toS :: Nil => 
           val frangeIdx = fromS.indexOf("from=") + "from=".length
           val frangeS = fromS.substring(frangeIdx)
           val fvalsS = frangeS.split(";").toList
           val (fl, fr) = fvalsS match {
             case lS :: rS :: Nil => 
               val l = lS.toInt 
               val r = rS.toInt
               (l,r)
             case _ => throw new STeXParseError("Invalid 'from' value in STeX source reference" + srcrefS, None, None)
           }
           val trangeS = toS.substring("to=".length, toS.length - 1) //removing "to=" and ending bracket
           val tvalsS = trangeS.split(";").toList
           val (tl, tr) = tvalsS match {
             case lS :: rS :: Nil => 
               val l = lS.toInt 
               val r = rS.toInt
               (l,r)
             case _ => throw new STeXParseError("Invalid 'to' value in STeX source reference " + srcrefS, None, None)
           }
           
           val from = SourcePosition(-1, fl, fr)
          val to = SourcePosition(-1, tl, tr)
          val sreg = SourceRegion(from,to)
          Some(SourceRef(dpath.uri, sreg))
          case _ => throw new STeXParseError("Invalid STeX source reference " + srcrefS, None, None)
        }
      } catch {
        case e : STeXParseError => //reporting and returning none
          errorCont(e)
          None
        case e : Exception => //producing parse error and returning none
          val err = STeXParseError.from(e, Some("Failed to parse SourceRef for <" + n.label + " " + n.attributes.toString + ">"), None, Some(Level.Warning))
          errorCont(err)
          None
      }
    } else { //no srcref attr so returning none and producing an Info type error if actual node elem
      if (n.isInstanceOf[Elem]) {
        val err = new STeXParseError("No stex:srcref attribute for <" + n.label + " " + n.attributes.toString + ">", None, Some(Level.Info))
        errorCont(err)
      }
      None
    }
  }
  
   def parseNarrativeObject(n : scala.xml.Node)(implicit dpath : DPath, 
                                                         thy : DeclaredTheory, 
                                                         errorCont : ErrorHandler,
                                                         resolveSPath : (Option[String], String, MPath) => GlobalName) : Option[Term]= {
    val sref = parseSourceRef(n, dpath) 
    implicit val mpath = thy.path
    n.child.find(_.label == "CMP").map(_.child) match {
      case Some(nodes) => 
        val narrNode = <div class="CMP"> {nodes} </div> //effectively treating CMP as a narrative div
        val cmp =  translateCMP(rewriteCMP(narrNode))(dpath, thy, errorCont : ErrorHandler)
        Some(cmp)
      case None => 
        val err = new STeXParseError("No CMP in narrative object " + n.label, sref, Some(Level.Warning))
        None
    }
  }
  
  def rewriteCMP(node : scala.xml.Node)(implicit mpath : MPath, 
                                                 errorCont : ErrorHandler, 
                                                 resolveSPath : (Option[String], String, MPath) => GlobalName) : scala.xml.Node = node.label match {
    case "OMS" if (xml.attr(node, "cd") == "OMPres") =>
      <om:OMS base={Narration.path.doc.toPath} module={Narration.path.module.toMPath.name.toPath} name={Narration.path.name.toPath}/>
    case "OMS" => 
      val cd =  xml.attr(node, "cd")
      val name = xml.attr(node, "name")
      val sym = resolveSPath(Some(cd), name, mpath)
      <om:OMS base={sym.module.toMPath.parent.toPath} module={sym.module.toMPath.name.last.toPath} name={sym.name.last.toPath}/>
    case "OME" => //OME(args) -> OMA(Informal.error -> args)
      val pre = OMS(Informal.constant("error")).toNode
      val newChild = node.child.map(rewriteCMP)
      new Elem(node.prefix, "OMA", node.attributes, node.scope, (pre +: newChild) : _*)
    case "#PCDATA" => new scala.xml.Text(node.toString)
    case _ => new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteCMP) :_*)
  }
  
  private def getChildren(node : scala.xml.Node)(implicit hasProp : scala.xml.Node => Boolean) : Seq[Node] = {
    if (hasProp(node)) {
     List(node)
    } else {
      node.child.flatMap(getChildren)
    }
  }
  
  def translateCMP(n : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, errorCont : ErrorHandler) : Term = {
    val sref = parseSourceRef(n, dpath)
    n.label match {
      case "#PCDATA" =>
        FlexiformalXML(scala.xml.Text(n.toString))
      case "OMOBJ" => 
        FlexiformalTerm(Obj.parseTerm(n, NamespaceMap(dpath)))
      case _ => //informal (narrative) term
        val err = new STeXParseError("Unexpected element label in CMP: " + n.label, sref, Some(Level.Info))
        errorCont(err)
        val terms = getChildren(n)(n => n.label == "term" || n.label == "OMOBJ").map(translateCMP)
        FlexiformalNode(n, terms.toList)
    }
  }
 }