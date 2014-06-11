package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.flexiformal._
import info.kwarc.mmt.api.presentation.Precedence

import scala.xml.{Node,Elem,NamespaceBinding}

abstract class STeXError(msg : String) extends Error(msg)

case class STeXParseError(msg : String, sref : Option[SourceRef]) extends STeXError(msg)
case class STeXLookupError(msg : String) extends STeXError(msg)

class STeXImporter extends Importer {
  val key : String = "stex-omdoc"
  override val logPrefix = "steximporter"
  def includeFile(name : String) : Boolean = name.endsWith(".omdoc") //stex/latexml generated omdoc
  
  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
   }
  
  override def apply(modifier: BuildTargetModifier, arch: Archive, in: List[String], args: List[String]) {
      val reqArgs = requiredArguments(modifier)
      if (reqArgs != args.length)
         throw ParseError("wrong number of arguments, required: " + reqArgs)
      modifier match {
         case Update => update(arch, args, in)
         case Clean  => clean(arch, args, in)
         case Build  => 
           //running twice, first to load all theories, then to successfully parse objects
           build(arch, args, in)
           build(arch, args, in)
      }
   }
  
  def importDocument(bt : BuildTask, cont : Document => Unit) {
    try {
      val src = scala.io.Source.fromFile(bt.inFile.toString)
      val cp = scala.xml.parsing.ConstructingParser.fromSource(src, true)
      val node : Node = cp.document()(0)
      src.close 
      val errors = translateDocument(node)(bt.narrationDPath)
      val doc = controller.getDocument(bt.narrationDPath)
      if (errors != Nil) {
        log("Errors: " + errors.mkString("\n"))
      }
      cont(doc)
    } catch {
      case e : Throwable => 
        log("WARNING: Skipping article due to error: " + e.toString() + " \n" + e.getStackTraceString) //skipping declaration
    }
  }
  
  def compileOne(inText : String, dpath : DPath) : (String, List[Error]) = {
   
    val node = scala.xml.XML.loadString(inText) //clearXmlNS(    
    val cleanNode = node //scala.xml.Utility.trim(node)
    val errors = translateDocument(cleanNode)(dpath)
    
    errors match {
      case Nil => //returning result
        val docXML = controller.getDocument(dpath).toNodeExpanded(controller.memory.content, controller.memory.narration)
        (docXML.toString, Nil)
      case _ => //reporting errors
        ("",errors)
    }
  } 
  
  val xmlNS = "http://www.w3.org/XML/1998/namespace"
  val omdocNS = "http://omdoc.org/ns"
  val mhBase = DPath(URI("http://mathhub.info/"))
   
  def add(s : StructuralElement) = {
    log("adding " + s.path.toPath)
    controller.add(s)
  }
  
  /**
   * Translate a toplevel <omdoc> node
   */
  private def translateDocument(n : Node)(implicit dpath : DPath) : List[Error] = {
    var errors : List[Error] = Nil
    try {
      n.label match {
        case "omdoc" => 
          //creating document and implicit theory
          implicit val doc = new Document(dpath)
          controller.add(doc)
          implicit val anonthy = new DeclaredTheory(dpath, OMV.anonymous, None) //no meta for now
          val ref = MRef(dpath, anonthy.path, true)
          controller.add(anonthy)
          controller.add(ref)
          //recursing into children
          errors ++= n.child.map(translateModule).flatten
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }

  /**
   * translate second-level, in-document elements (typically modules)
   */
  private def translateModule(n : Node)(implicit doc : Document, anonthy : DeclaredTheory) : List[Error] = {
    var errors : List[Error] = Nil
    try {
      n.label match {
        case "theory" => //create theory
          val name = getName(n, doc)
          val thy = new DeclaredTheory(doc.path, name, None)
          val ref = MRef(doc.path, thy.path, true)
          add(ref)
          add(thy)
          errors ++= n.child.map(translateDeclaration(_)(doc, thy)).flatten
        case "omgroup" => //recurse to find internal modules
          val errs = n.child.map(translateModule)
          errors ++= errs.flatten
        case "metadata" => //TODO
        case "bibliography" => //TODO
        case "index" => //TODO
        case "oref" => //TODO
        case _ => translateDeclaration(n : Node)(doc, anonthy)
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }
  
  /** 
   *  translate third level, in-module elements (typically declarations)
   */
  private def translateDeclaration(n : Node)(implicit doc : Document, thy : DeclaredTheory) : List[Error] = {
    var errors : List[Error] = Nil 
    implicit val dpath = doc.path
    implicit val mpath = thy.path
    try {
      n.label match {
        case "imports" => //omdoc import -> mmt (plain) include
          val fromS = (n \ "@from").text
          val from = parseMPath(fromS)
          if (from != mpath) {
            val include = PlainInclude(from, thy.path)
            add(include)
          }
        case "symbol" => //omdoc symbol -> mmt constant
          val name = (n \ "@name").text match {
            case "" => getName(n, thy)
            case s => LocalName(s)
          }
          val tpWrapperO = n.child.find(_.label == "type")
          val tpO = tpWrapperO.map(tpN => translateTerm(tpN.child.head))
          val dfO = None //TODO, get also def
          val const = new FinalConstant(OMMOD(mpath), name, None, TermContainer(tpO), TermContainer(dfO), None, NotationContainer())
          add(const)
        case "definition" => //omdoc definition -> immt flexiformal declaration
          val sref = parseSourceRef(n, doc.path)
          val name = getName(n, thy)
          val targetsS = (n \ "@for").text.split(" ")
          val targets = targetsS map {s =>
            mpath ? LocalName(s) //TODO handle non-local references 
          }
          parseNarrativeObject(n)(dpath, thy, targets.head) match {
            case None => //nothing to do  
            case Some(no) =>
              val dfn = new Definition(OMMOD(mpath), name, targets.toList, no)
              SourceRef.update(dfn, sref)
              add(dfn)
            }
        case "omtext" => 
          val name = getName(n, thy)
          val sref = parseSourceRef(n, doc.path)
          parseNarrativeObject(n)(dpath, thy, mpath ? name) match {
            case Some(no) =>               
              val dfn = new PlainNarration(OMMOD(mpath), name, no)
              SourceRef.update(dfn, sref)
              add(dfn)
            case None => //nothing to do
          }
        case "notation" =>
          //getting symbol info
          val cd = (n \ "@cd").text
          val name = (n \ "@name").text
          val refPath = Path.parseM("?" + cd, doc.path)
          val refName = refPath ? LocalName(name)
          val c = controller.memory.content.getConstant(refName, p => "Notation for nonexistent constant " + p)
          //getting macro info
          val macro_name = (n \ "@macro_name").text
          val nrArgs = (n \ "@nargs").text.toInt
          val macroMk = Delim("\\" + macro_name)
          val notArgs = macroMk :: (0 until nrArgs).toList.flatMap(i => Delim("{") :: Arg(i + 1) :: Delim("}") :: Nil)
          val stexScope = NotationScope(None, "stex" :: "tex" :: Nil, 0)
          val texNotation = new TextNotation(Mixfix(notArgs), Precedence.integer(0), None, stexScope)
          c.notC.parsingDim.set(texNotation)
          //getting mathml rendering info
          val prototype = n.child.find(_.label == "prototype").get
          val renderings = n.child.filter(_.label == "rendering")
          renderings foreach { rendering => 
            val notation = makeNotation(prototype, rendering)(doc.path, thy, refName)
            c.notC.presentationDim.set(notation)
          }
        case "metadata" => //TODO
        case "#PCDATA" => //ignore
        case _  =>
          val name = getName(n, thy)
          val nr = new PlainNarration(OMMOD(mpath), name, FlexiformalDeclaration.parseNarrativeObject(rewriteNode(n))(doc.path))
          add(nr)
      }
    } catch {
      case e : Error => 
        try {
          val sref = parseSourceRef(n, doc.path)
          errors ::= STeXParseError(e.shortMsg, Some(sref))
        } catch {
          case e2 : Throwable => errors ::= e
        }
      case e : Throwable => log("WARNING: declaration ignored because of error " + e.getMessage() + "\n" + e.getStackTrace() + "\n" + n.toString)
    }
    errors
  }
  
  def parseNarrativeObject(n : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : Option[NarrativeObject]= {
     n.child.find(_.label == "CMP").map(_.child) match {
      case Some(nodes) => 
        val cmpElems = nodes.map(translateCMP(_)(dpath, thy, spath))
        val cmp = new NarrativeNode(<span/>, cmpElems.toList)    
        Some(cmp)
        case None => 
          log("no CMP: " + n) //TODO probably turn this into a warning  
          None
    }
  }
  
  def makeNotation(prototype : scala.xml.Node, rendering : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : TextNotation = {
    val argMap : collection.mutable.Map[String, Int] = new collection.mutable.HashMap() 
    val protoBody = scala.xml.Utility.trim(prototype).child.head
    val renderingChildren = scala.xml.Utility.trim(rendering).child.toList
    val symName = protoBody.label match {
      case "OMA" => 
        val n = protoBody.child.head
        n.label match {
          case "OMS" => 
            val cd = (n \ "@cd").text
            val name = (n \ "@name").text
            val refPath = Path.parseM("?" + cd, dpath)
            //computing map of arg names to positions
            protoBody.child.tail.zipWithIndex foreach {p => 
              val name = (p._1 \ "@name").text
              argMap(name) = p._2 + 1 //args numbers start from 1
            }
            refPath ? LocalName(name)
          case _ => throw ParseError("invalid  prototype" + protoBody)
        }
      case "OMS" => 
        val cd = (protoBody \ "@cd").text
        val name = (protoBody \ "@name").text
        val refPath = Path.parseM("?" + cd, dpath)
        refPath ? LocalName(name)
      case _ => throw ParseError("invalid prototype" + prototype + rendering)
    }
   
   val markers =  renderingChildren.flatMap(mk => parseRenderingMarkers(mk, argMap.toMap))
   val precedence = getPrecedence(rendering)
   val variant = (rendering \ "@ic").text match {
     case "" => None
     case s => Some(s)
   }
   val languages = "mathml" :: Nil
   val scope = NotationScope(variant, languages, 0)
   new TextNotation(Mixfix(markers), precedence, None, scope)
  }
  
  def getPrecedence(n : scala.xml.Node) : Precedence = {
    val precS = (n \ "@precedence").text
    try {
      Precedence.integer(precS.toInt)
    } catch {
      case _ : Throwable => precS match {
        case "posinfty" => Precedence.infinite
        case "neginfty" => Precedence.neginfinite
        case _ => Precedence.integer(0) //default 
      }
    }
  }
  
  def translateCMP(n : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : NarrativeObject = n.label match {
    case "term" => 
      val cd = (n \ "@cd").text
      val name = (n \ "@name").text
      val objects = n.child.map(translateCMP).toList
      val refName = resolveSPath(cd, name, thy.path)
      val role = (n \ "@role").text
      if (role == "definiendum") {
        val ref = new NarrativeRef(refName, objects, true)
        val const = controller.memory.content.getConstant(refName)
        //creating notation
        val markers = n.text.split(" ").map(Delim(_))
        val prec = presentation.Precedence.integer(0)
        val verbScope = NotationScope(None, sTeX.getLanguage(spath).toList, 0)
        val not = TextNotation(prec, None, verbScope)(markers :_*)
        const.notC.verbalizationDim.set(not)
        ref
      } else {
        new NarrativeRef(refName, objects)
      }
    case "#PCDATA" =>
      makeNarrativeText(n.toString)
    case "OMOBJ" => new NarrativeTerm(translateTerm(n)(dpath, thy.path))
    case _ => 
      log("Unexpected element label in CMP" + n)
      val children = n.child.map(translateCMP)
      new NarrativeNode(n, children.toList)
  }
  
  def makeNarrativeText(s : String) : NarrativeObject = {
    
    val elems = s.toCharArray().toList
    def makeChildren(elems : List[Char], buffer : String) : List[Node] = elems match {
      case Nil if buffer == "" => Nil
      case Nil => scala.xml.Text(buffer) :: Nil
      case hd :: tl if hd == ' '  => 
        scala.xml.Text(buffer) :: scala.xml.EntityRef("#160") :: makeChildren(tl, "")
      case hd :: tl => makeChildren(tl, buffer + hd)
    }
    
    val children = makeChildren(elems, "")
    new NarrativeXML(<span type="XML"> { children} </span>)
    
    new NarrativeXML(scala.xml.Text(s))
  }
  
  def rewriteNode(node : scala.xml.Node)(implicit mpath : MPath) : scala.xml.Node = node.label match {
      case "OMS" => 
        val cd =  xml.attr(node, "cd")
        val name = xml.attr(node, "name")
        val sym = resolveSPath(cd, name, mpath)
        <om:OMS base={sym.module.toMPath.parent.toPath} module={sym.module.toMPath.name.last.toPath} name={sym.name.last.toPath}/>
      case "#PCDATA" => new scala.xml.Text(node.toString)
      case _ => new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteNode) :_*)
    }
  
  def translateTerm(n : scala.xml.Node)(implicit dpath : DPath, mpath : MPath) : Term = {
    Obj.parseTerm(rewriteNode(n), dpath)
  }
  
   
  def parseRenderingMarkers(n : scala.xml.Node, argMap : Map[String, Int])(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : List[Marker] = n.label match {
    case "mrow" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
    case "mmultiscripts" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList //treated as mrow because not sure what it should do
    case "msub" => 
      val main = GroupMarker(parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath))
      val sub = GroupMarker(parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath))
      List(ScriptMarker(main,None,Some(sub),None,None))
    case "msup" =>
      val main = GroupMarker(parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath))
      val sup = GroupMarker(parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath))
      List(ScriptMarker(main,Some(sup),None,None,None))
    case "msubsup" =>
      val main = GroupMarker(parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath))
      val sub = GroupMarker(parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath))
      val sup = GroupMarker(parseRenderingMarkers(n.child(2),argMap)(dpath,thy,spath))
      List(ScriptMarker(main,Some(sup),Some(sub),None,None))
    case "munder" =>
      val main = GroupMarker(parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath))
      val under = GroupMarker(parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath))
      List(ScriptMarker(main,None,None,None,Some(under)))
    case "mover" =>
      val main = GroupMarker(parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath))
      val over = GroupMarker(parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath))
      List(ScriptMarker(main,None,None,Some(over),None))
    case "munderover" =>
      val main = GroupMarker(parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath))
      val under = GroupMarker(parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath))
      val over = GroupMarker(parseRenderingMarkers(n.child(2),argMap)(dpath,thy,spath))
      List(ScriptMarker(main,None,None,Some(over),Some(under)))
      
    case "mpadded" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
    case "mo" => makeDelim(n.child.mkString) :: Nil
    case "mi" => makeDelim(n.child.mkString) :: Nil //for now treated exactly like mo        
    case "mn" => makeDelim(n.child.mkString) :: Nil //for now treated exactly like mo
    case "mtext" => makeDelim(n.child.mkString) :: Nil
    case "text" => makeDelim(n.child.mkString) :: Nil
    case "mfrac" => 
      val above = parseRenderingMarkers(n.child(0), argMap)
      val below = parseRenderingMarkers(n.child(1), argMap)
      val attribOpt =  (n \\ "@bevelled") find {_.text == "bevelled"}
      val render_line = attribOpt match {
        case Some(attrib) if attrib.text == "true" => true
        case _ => false
      }
      val fraction = FractionMarker(above, below, render_line) //true => render line
      List(fraction)
    case "mtd" => TdMarker( n.child.toList.flatMap(parseRenderingMarkers(_, argMap)))::Nil
    case "mtr" => TrMarker( n.child.toList.flatMap(parseRenderingMarkers(_, argMap)))::Nil 
    case "mtable" => TableMarker( n.child.toList.flatMap(parseRenderingMarkers(_, argMap)))::Nil 
    case "render" => 
      val argName = (n \ "@name").text
      val argNr = argMap(argName)
      
      Arg(argNr, Some(getPrecedence(n))) :: Nil
    case "iterate" =>
      val argName = (n \ "@name").text
      val argNr = argMap(argName)
      val precO = Some(getPrecedence(n))
      n.child.find(_.label == "separator") match {
        case None => SeqArg(argNr, makeDelim(","), precO) :: Nil
        case Some(sep) => 
          sep.child.toList match {
            case Nil => 
              SeqArg(argNr, makeDelim(","), precO) :: Nil
            case hd :: tl =>
              val delim = parseRenderingMarkers(hd, argMap).mkString("")
              SeqArg(argNr, makeDelim(delim), precO) :: Nil
          } 
        }
    case "mstyle" => n.child.toList.flatMap(parseRenderingMarkers(_, argMap))
    case "merror" => Nil
    case "mroot" => Delim("√") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap))
    case "msqrt" => Delim("√") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap))
    case "none" => Nil
  }
  
  def makeDelim(s : String) : Delim = {
    val str = s.replaceAll("…", "...")
    Delim(s)
  }
  
  def parseSourceRef(n : scala.xml.Node,dpath : DPath) : SourceRef = {
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
         case _ => throw STeXParseError("Invalid 'from' value in STeX source reference" + srcrefS, None)
       }
       val trangeS = toS.substring("to=".length, toS.length - 1) //removing "to=" and ending bracket
       val tvalsS = trangeS.split(";").toList
       val (tl, tr) = tvalsS match {
         case lS :: rS :: Nil => 
           val l = lS.toInt 
           val r = rS.toInt
           (l,r)
         case _ => throw STeXParseError("Invalid 'to' value in STeX source reference " + srcrefS, None)
       }
       
      val from = SourcePosition(-1, fl, fr)
      val to = SourcePosition(-1, tl, tr)
      val sreg = SourceRegion(from,to)
      SourceRef(dpath.uri, sreg)
      
      case _ => throw STeXParseError("Invalid STeX source reference " + srcrefS, None)
    }
    
  }
  
  def getName(n : Node, container : Content) : LocalName = {
    try {
      val nameS =  (n \ s"@{$xmlNS}id").text
      LocalName(nameS)
    } catch {
      case e : Exception => LocalName("D" + container.children.length.toString)
    }
  }
  
  def parseDPath(s : String) : DPath = {
    val parts = s.split("/").toList
    parts match {
      case group :: project :: "source" :: name :: Nil => //MathHub URI
        mhBase / group / project / (name + ".omdoc")
      case _ => throw STeXParseError("Expected group/project/'source'/name for document path. Got: " + s, None)
    }
  }
  
  def parseMPath(s : String) : MPath = {
    val parts = s.split("#")
    parts.toList match {
      case fpathS :: tnameS :: Nil => 
        val dpath = parseDPath(fpathS)
        dpath ? LocalName(tnameS)
      case _ => throw STeXParseError("Expected 2 # separated parts for module path in: " + s, None)
    }
  }
  
  def resolveSPath(tnameS : String, snameS : String, container : MPath) : GlobalName = {
    val tpaths = controller.globalLookup.visible(OMMOD(container)) //includes container
    val options = tpaths.map(_.toMPath).filter(_.name.last.toPath == tnameS)
    options.toList match {
      case Nil => throw ParseError("Cannot resolve module for " + tnameS + "?" + snameS + " from theory " + container.toPath) //perhaps not included
      case l => 
        val matches = l.map(controller.get(_)) flatMap {
          case d : DeclaredTheory => 
            d.getConstants.filter(_.name.last.toPath == snameS)
          case _ => None
        }
        matches match {
          case Nil => throw ParseError("Cannot resolve symbol for module=" + tnameS + " and symbol=" + snameS + ". No matching modules " + l +  " , contain symbol " + snameS ) 
          case hd :: Nil => hd.path
          case _ => throw ParseError("Cannot resolve symbol for module=" + tnameS + " and symbol=" + snameS + ". Several matching symbols: " + matches) 
        }
    }
  }
}
