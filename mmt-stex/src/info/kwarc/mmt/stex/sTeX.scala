package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import parser._
import scala.xml._
import modules._
import symbols._
import objects._
import informal._
import utils._


/** Filters errors before passing them to the another error handler */
class FilteringErrorHandler(handler : ErrorHandler, filter : Error => Boolean) extends ErrorHandler {
  override def mark = handler.mark
  override def hasNewErrors = handler.hasNewErrors
  override def catchIn(a: => Unit) = handler.catchIn(a)
  override def apply(e: Error) =if (filter(e)) handler.apply(e) //otherwise ignore
  def addError(e : Error) = {} //nothing to do here, not caalled
}

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
  
  def getDefaultSRef(s : String, dpath : DPath) : SourceRef = {
    val from = SourcePosition(-1, 1, 1)
    val lines = s.split(System.lineSeparator)
    val to = SourcePosition(-1, lines.length, lines.last.length)
    val sreg = SourceRegion(from,to)
    SourceRef(dpath.uri, sreg)
  }
  
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
             case _ => throw new STeXParseError("Invalid 'from' value in STeX source reference" , Some(s"srcref value is `$srcrefS`"), None, None)
           }
           val trangeS = toS.substring("to=".length, toS.length - 1) //removing "to=" and ending bracket
           val tvalsS = trangeS.split(";").toList
           val (tl, tr) = tvalsS match {
             case lS :: rS :: Nil => 
               val l = lS.toInt 
               val r = rS.toInt
               (l,r)
             case _ => throw new STeXParseError("Invalid 'to' value in STeX source reference " , Some(s"srcref value is `$srcrefS`"), None, None)
           }
           
           val from = SourcePosition(-1, fl, fr)
          val to = SourcePosition(-1, tl, tr)
          val sreg = SourceRegion(from,to)
          Some(SourceRef(dpath.uri, sreg))
          case _ => throw new STeXParseError("Invalid STeX source reference " , Some(s"srcref value is `$srcrefS`"), None, None)
        }
      } catch {
        case e : STeXParseError => //reporting and returning none
          errorCont(e)
          None
        case e : Exception => //producing parse error and returning none
          val err = STeXParseError.from(e, "Failed to parse SourceRef for <" + n.label + " " + n.attributes.toString + ">", None, None, Some(Level.Warning))
          errorCont(err)
          None
      }
    } else { //no srcref attr so returning none and producing an Info type error if actual node elem
      if (n.isInstanceOf[Elem]) {
        val err = new STeXParseError("No stex:srcref attribute for <" + n.label + " " + n.attributes.toString + ">", None, None, Some(Level.Info))
        errorCont(err)
      }
      None
    }
  }
  
   def parseNarrativeObject(n : scala.xml.Node, tsref : SourceRef)(implicit dpath : DPath, 
                                                         mpath : MPath, 
                                                         errorCont : ErrorHandler,
                                                         resolveSPath : (Option[String], Option[String], String, MPath, SourceRef) => GlobalName) : Option[Term]= {
    val sref = parseSourceRef(n, dpath).getOrElse(tsref)
    n.child.find(_.label == "CMP").map(_.child) match {
      case Some(nodes) => 
        val narrNode = <div class="CMP"> {nodes} </div> //effectively treating CMP as a narrative div
        val cmp =  translateCMP(rewriteCMP(narrNode, sref), sref)(dpath, mpath, errorCont)
        Some(cmp)
      case None => 
        val err = new STeXParseError("No CMP in narrative object " + n.label, None, Some(sref), Some(Level.Warning))
        errorCont(err)
        None
    }
  }
  
  def rewriteCMP(node : scala.xml.Node, tsref : SourceRef)(implicit mpath : MPath, 
                                                 errorCont : ErrorHandler, 
                                                 resolveSPath : (Option[String], Option[String], String, MPath, SourceRef) => GlobalName) : scala.xml.Node = node.label match {
    case "OMS" if (xml.attr(node, "cd") == "OMPres") =>
      <om:OMS base={Narration.path.doc.toPath} module={Narration.path.module.toMPath.name.toPath} name={Narration.path.name.toPath}/>
    case "OMS" => 
      
      val baseO =  xml.attr(node, "base") match {
        case "" => None
        case s => Some(s)
      }
      
      val cdO =  xml.attr(node, "cd") match {
        case "" => None
        case s => Some(s)
      }
      val name = xml.attr(node, "name")
      val sym = resolveSPath(baseO, cdO, name, mpath, tsref)
      <om:OMS base={sym.module.toMPath.parent.toPath} module={sym.module.toMPath.name.last.toPath} name={sym.name.last.toPath}/>
    case "OME" => <om:OMV name="error"/> //TODO temporary hack for OEIS
    case "OME" => //OME(args) -> OMA(Informal.error -> args)
      val pre = OMS(Informal.constant("error")).toNode
      val newChild = node.child.map(n => rewriteCMP(n, tsref))
      new Elem(node.prefix, "OMA", node.attributes, node.scope, (pre +: newChild) : _*)
    case "OMR" =>
      val baseO =  xml.attr(node, "base") match {
        case "" => None
        case s => Some(s)
      }
      val xref = xml.attr(node, "xref")
      val sym = resolveSPath(baseO, Some(xref), xref, mpath, tsref)
      <om:OMS base={sym.module.toMPath.parent.toPath} module={sym.module.toMPath.name.last.toPath} name={sym.name.last.toPath}/>
    case "#PCDATA" => new scala.xml.Text(node.toString)
    case _ => new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(n => rewriteCMP(n, tsref)) :_*)
  }
  
  private def getChildren(node : scala.xml.Node, pos : List[Int] = Nil)(implicit hasProp : scala.xml.Node => Boolean) : Seq[(Node, List[Int])] = {
    if (hasProp(node)) {
     List((node, pos.reverse))
    } else {
      node.child.zipWithIndex.flatMap(p => getChildren(p._1, p._2 :: pos))
    }
  }
  
  def translateCMP(n : scala.xml.Node, tsref : SourceRef)(implicit dpath : DPath, mpath : MPath, errorCont : ErrorHandler) : Term = {
    val sref = parseSourceRef(n, dpath)
    n.label match {
      case "#PCDATA" =>
        FlexiformalXML(scala.xml.Text(n.toString))
      case "OMOBJ" => 
        FlexiformalTerm(Obj.parseTerm(n, NamespaceMap(dpath)))
      case _ => //informal (narrative) term
        val terms = getChildren(n)(n => n.label == "term" || n.label == "OMOBJ").map(p => (translateCMP(p._1, tsref), p._2))
        FlexiformalNode(n, terms.toList)
    }
  }
}