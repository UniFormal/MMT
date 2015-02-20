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

import scala.xml.{Node,Elem,NamespaceBinding}

abstract class STeXError(msg : String) extends Error(msg)

case class STeXParseError(msg : String, sref : Option[SourceRef]) extends STeXError(msg) {
  private def srefS = sref.map(_.region.toString).getOrElse("")
  override def toNode = <error type={this.getClass().toString} shortMsg={this.shortMsg} level={Level.Error.toString} sref={srefS}> {this.getLongMessage.toString} </error>
}
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
      errors foreach {e =>
        bt.errorCont(e)
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
        val doc = try {
          controller.getDocument(dpath).toNodeExpanded(controller.memory.content, controller.memory.narration).toString
        } catch {
          case e : Throwable => ""
        }
        (doc, errors)
    }
  } 
  
  val xmlNS = "http://www.w3.org/XML/1998/namespace"
  val omdocNS = "http://omdoc.org/ns"
  val mhBase = DPath(URI("http://mathhub.info/"))
   
  def add(s : StructuralElement) = {
    log("adding " + s.path.toPath)
    controller.add(s)
  }
  
  private def getAnonThy(dpath : DPath) : DeclaredTheory = {
    val anonpath = dpath ? OMV.anonymous
    try {
      controller.get(anonpath) match {
        case d : DeclaredTheory => d
      }
    } catch {
      case e : GetError => 
        val anonthy = new DeclaredTheory(anonpath.doc, anonpath.name, None) //no meta for now
        val ref = MRef(dpath, anonthy.path, true)
        controller.add(anonthy)
        controller.add(ref)
        anonthy
    }
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
  private def translateModule(n : Node)(implicit doc : Document, refadd : XRef => Unit = add) : List[Error] = {
    var errors : List[Error] = Nil
    try {
      n.label match {
        case "theory" => //create theory
          val name = getName(n, doc)
          val thy = new DeclaredTheory(doc.path, name, None)
          val ref = MRef(doc.path, thy.path, true)
          refadd(ref)
          add(thy)
          errors ++= n.child.map(translateDeclaration(_)(doc, thy)).flatten
        case "omgroup" => //recurse to find internal modules
          var group = XRefGroup(doc.path, Nil)
          def refadd(ref : XRef) = {
            group = group.append(ref)
          } 
          val errs = n.child.map(n => translateModule(n)(doc, refadd))
          add(group)
          errors ++= errs.flatten
        case "metadata" => //TODO
        case "bibliography" => //TODO
        case "index" => //TODO
        case "oref" => 
          val href = (n \ "@href").text
          val target = doc.path.^! / (href + ".omdoc")
          val dref = new DRef(doc.path, target)
          refadd(dref)
        case _ => translateDeclaration(n : Node)(doc, getAnonThy(doc.path))
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
            val comps = s.split("\\?").toList
            resolveSPath(Some(comps.head), comps.tail.head, thy.path)
          }
          parseNarrativeObject(n)(dpath, thy, targets.head) match {
            case None => //nothing to do  
            case Some(no) =>
              val dfn = new Definition(OMMOD(mpath), name, targets.toList, None, no)
              SourceRef.update(dfn, sref)
              add(dfn)
            }
        case "omtext" => 
          val name = getName(n, thy)
          val sref = parseSourceRef(n, doc.path)
          parseNarrativeObject(n)(dpath, thy, mpath ? name) match {
            case Some(no) =>               
              val dfn = new PlainNarration(OMMOD(mpath), name, None, no)
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
            if (notation.markers.length > 0) 
              c.notC.presentationDim.set(notation)
          }
        case "metadata" => //TODO
        case "#PCDATA" => //ignore
        case _  =>
          val name = getName(n, thy)
          val nr = new PlainNarration(OMMOD(mpath), name, None, FlexiformalDeclaration.parseObject(rewriteNode(n))(doc.path))
          add(nr)
      }
    } catch {
      case e : Error => 
        try {
          val sref = parseSourceRef(n, doc.path)
          val err = STeXParseError(e.shortMsg, Some(sref))
          //println("###############")
          //println(e.shortMsg)
          //println(e.getLongMessage)
          //println(e.getStackTraceString)
          err.setCausedBy(e)
          errors ::= err
        } catch {
          case e2 : Throwable => errors ::= e
        }
      case e : Throwable => log("WARNING: declaration ignored because of error " + e.getMessage() + "\n" + e.getStackTrace() + "\n" + n.toString)
    }
    errors
  }
  
  def parseNarrativeObject(n : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : Option[FlexiformalObject]= {
     n.child.find(_.label == "CMP").map(_.child) match {
      case Some(nodes) => 
        val cmpElems = nodes.map(translateCMP(_)(dpath, thy, spath))
        val cmp = new FlexiformalNode(<span/>, cmpElems.toList)    
        Some(cmp)
        case None => 
          log("no CMP: " + n) //TODO probably turn this into a warning  
          None
    }
  }
  
  abstract class ProtoPlaceholder(val nr : Int)
  case class ProtoArg(override val nr : Int) extends ProtoPlaceholder(nr)
  case class ProtoVar(override val nr : Int) extends ProtoPlaceholder(nr)
  case class ProtoSub(override val nr: Int) extends ProtoPlaceholder(nr)
  
  
  def parsePrototype(protoBody : scala.xml.Node, inBinder : Boolean = false)(implicit dpath : DPath) :(GlobalName, Map[String, ProtoPlaceholder]) = {
    val argMap : collection.mutable.Map[String, ProtoPlaceholder] = new collection.mutable.HashMap()
    var nextArgNumber = 1 //start
    val symName = protoBody.label match {
      case "OMA" | "OMBIND"=> 
        val n = protoBody.child.head
        n.label match {
          case "OMS" | "OMA" | "OMBIND"  => 
            val (spath, args) = parsePrototype(n, protoBody.label == "OMBIND")
            argMap ++= args
            nextArgNumber = argMap.size + 1
            //computing map of arg names to positions
            protoBody.child.tail foreach {p => p.label match {
              case "expr" | "exprlist" => 
                val name = (p \ "@name").text
                argMap(name) = if (inBinder) ProtoSub(nextArgNumber) else ProtoArg(nextArgNumber)
                nextArgNumber += 1
              case "OMBVAR" => 
                val name = (p.child.head \ "@name").text
                argMap(name) = ProtoVar(nextArgNumber) 
                nextArgNumber += 1
              case _ => throw ParseError("invalid prototype" + protoBody)
            }}
            spath
          case _ => throw ParseError("invalid prototype" + protoBody)
        }
      case "OMS" =>
        val cd = (protoBody \ "@cd").text
        val name = (protoBody \ "@name").text
        val refPath = Path.parseM("?" + cd, dpath)
        refPath ? LocalName(name)
      case _ => throw ParseError("invalid prototype" + protoBody)
    }
    (symName -> argMap.toMap)
  }
  
  def makeNotation(prototype : scala.xml.Node, rendering : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : TextNotation = {
   val protoBody = scala.xml.Utility.trim(prototype).child.head
   val renderingChildren = scala.xml.Utility.trim(rendering).child.toList
   val (symName, argMap) = parsePrototype(protoBody)
   
   val markers =  renderingChildren.flatMap(mk => parseRenderingMarkers(mk, argMap))
   val precedence = getPrecedence(rendering)
   val variant = (rendering \ "@ic").text match {
     case "" => None
     case s => Some(s)
   }
   val languages = "mathml" :: Nil
   val scope = NotationScope(variant, languages, 0)
   val notation = new TextNotation(Mixfix(markers), precedence, None, scope)
   notation
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
  
  def translateCMP(n : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : FlexiformalObject = n.label match {
    case "term" => 
      val cd = (n \ "@cd").text 
      val name = (n \ "@name").text
      val objects = n.child.map(translateCMP).toList
      val refName = resolveSPath(Some(cd), name, thy.path)
      val role = (n \ "@role").text
      if (role == "definiendum") {
        val ref = new FlexiformalRef(refName, None, objects, true)
        val const = controller.memory.content.getConstant(refName)
        //creating notation
        var i = 0
        var markers : Seq[Marker] = Nil 
        var tmpS : String = ""
        n.child.toList map {
          case <OMOBJ>{s}</OMOBJ> => 
            if (tmpS != "") {
              markers ++= tmpS.split(" ").map(Delim(_)).toList
              tmpS = ""
            }
            i += 1
            markers = markers :+ Arg(i)
          case n => tmpS += n.text
        }
        if (tmpS != "") {
          markers ++= tmpS.split(" ").map(Delim(_)).toList
          tmpS = ""
        }
        val prec = Precedence.integer(0)
        val verbScope = NotationScope(None, sTeX.getLanguage(thy.path).toList, 0)
        val not = TextNotation(prec, None, verbScope)(markers :_*)
        const.notC.verbalizationDim.set(not)
        ref
      } else {
        new FlexiformalRef(refName, None, objects)
      }
    case "#PCDATA" =>
      makeNarrativeText(n.toString)
    case "OMOBJ" => new FlexiformalTerm(translateTerm(n)(dpath, thy.path))
    case _ => 
      log("Unexpected element label in CMP" + n)
      val children = n.child.map(translateCMP)
      new FlexiformalNode(n, children.toList)
  }
  
  def makeNarrativeText(s : String) : FlexiformalObject = {
    
    val elems = s.toCharArray().toList
    def makeChildren(elems : List[Char], buffer : String) : List[Node] = elems match {
      case Nil if buffer == "" => Nil
      case Nil => scala.xml.Text(buffer) :: Nil
      case hd :: tl if hd == ' '  => 
        scala.xml.Text(buffer) :: scala.xml.EntityRef("#160") :: makeChildren(tl, "")
      case hd :: tl => makeChildren(tl, buffer + hd)
    }
    
    val children = makeChildren(elems, "")
    new FlexiformalXML(<span type="XML"> { children} </span>)
    
    new FlexiformalXML(scala.xml.Text(s))
  }
  
  def rewriteNode(node : scala.xml.Node)(implicit mpath : MPath) : scala.xml.Node = node.label match {
      case "OMS" => 
        val cd =  xml.attr(node, "cd")
        val name = xml.attr(node, "name")
        val sym = resolveSPath(Some(cd), name, mpath)
        <om:OMS base={sym.module.toMPath.parent.toPath} module={sym.module.toMPath.name.last.toPath} name={sym.name.last.toPath}/>
      case "#PCDATA" => new scala.xml.Text(node.toString)
      case "OMATTR" if node.child(1).label == "OMV" => //Complex Variable
        val n = node.child(0).child(1) //TODO
        val nn = <om:OMFOREIGN>{scala.xml.Utility.trim(n).child.map(cleanNamespaces)}</om:OMFOREIGN>
        nn //TODO refactor cleanNamespaces to some xml helper object --duplicate in FlexiformalDeclarationObject
      case _ => new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteNode) :_*)
    }
  
  def translateTerm(n : scala.xml.Node)(implicit dpath : DPath, mpath : MPath) : Term = {
    Obj.parseTerm(rewriteNode(n), dpath)
  }
  
  def cleanNamespaces(node : scala.xml.Node) : Node = node match {
    case el : Elem => 
      val scope = _cleanNamespaces(el.scope, Nil)
      new scala.xml.Elem(null, el.label, el.attributes, scope, el.minimizeEmpty, el.child.map(cleanNamespaces) : _*)
    case _ => node
  }
  
  private def _cleanNamespaces(scope : NamespaceBinding, prefixes : List[String] = Nil) : NamespaceBinding = {
    if (scope == scala.xml.TopScope) {
      scope
    } else {
      if (prefixes.contains(scope.prefix)) {
        _cleanNamespaces(scope.parent, prefixes)
      } else {
        NamespaceBinding(scope.prefix, scope.uri, _cleanNamespaces(scope.parent, scope.prefix :: prefixes))
      }
    }
  }
  
   
  def parseRenderingMarkers(n : scala.xml.Node, argMap : Map[String, ProtoPlaceholder])(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName) : List[Marker] = n.label match {
    case "mrow" => Delim("(") :: n.child.flatMap(parseRenderingMarkers(_, argMap)).toList ::: List(Delim(")"))
    case "mmultiscripts" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList //treated as mrow because not sure what it should do
    case "msub" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath)
      val sub = parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath)
      main ::: List(Delim("_")) :::sub
    case "msup" => //assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath)
      val sup = parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath)
      main ::: List(Delim("^")) :::sup
    case "msubsup" =>//assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath)
      val sub = parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath)
      val sup = parseRenderingMarkers(n.child(2),argMap)(dpath,thy,spath)
        main::: List(Delim("_")) :::sub ::: List(Delim("^")) :::sup
    case "munder" =>//assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath)
      val under = parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath)
      main ::: List(Delim("__")) ::: under
    case "mover" =>//assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath)
      val over = parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath)
      main ::: List(Delim("^^")) ::: over
    case "munderover" =>//assuming well formed elem and children
      val main = parseRenderingMarkers(n.child(0),argMap)(dpath,thy,spath)
      val under = parseRenderingMarkers(n.child(1),argMap)(dpath,thy,spath)
      val over = parseRenderingMarkers(n.child(2),argMap)(dpath,thy,spath)
      main ::: List(Delim("__")) ::: under ::: List(Delim("^^")) ::: over
    case "mpadded" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
    case "mo" => makeDelim(n.child.mkString) :: Nil
    case "mi" => makeDelim("#id_" + n.child.mkString) :: Nil
    case "mn" => makeDelim("#num_" + n.child.mkString) :: Nil
    case "mtext" => makeDelim(n.child.mkString) :: Nil
    case "text" => makeDelim(n.child.mkString) :: Nil
    case "mfrac" => //assuming well formed elem and children
      val enum = parseRenderingMarkers(n.child(0), argMap)
      val denum = parseRenderingMarkers(n.child(1), argMap)
//      val attribOpt =  (n \\ "@bevelled") find {_.text == "bevelled"}   NOT SURE HOW TO HANDLE THIS ATTRIBUTE
//      val render_line = attribOpt match {
//        case Some(attrib) if attrib.text == "true" => true
//        case _ => false
//      }
      enum ::: List(Delim("/")) ::: denum
    case "mtd" => val content = n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
      makeDelim("[&")::content:::makeDelim("&]")::Nil
    case "mtr" => makeDelim("[\\")::n.child.toList.flatMap(parseRenderingMarkers(_, argMap)).toList:::makeDelim("\\]")::Nil
    case "mtable" => makeDelim("[[")::n.child.toList.flatMap(parseRenderingMarkers(_, argMap)).toList:::makeDelim("]]")::Nil 
    case "render" => 
      val argName = (n \ "@name").text
      argMap(argName) match{
        case ProtoArg(nr) => Arg(nr, None) :: Nil //TODO add precedence back and fix printing and parsing of Args with precedence Some(getPrecedence(n))
        case ProtoVar(nr) => Var(nr, false, None, None) :: Nil //TODO Some(getPrecedence(n)
        case ProtoSub(nr) => Arg(nr, None) :: Nil //Some(getPrecedence(n))
      }
    case "iterate" =>
      val argName = (n \ "@name").text
      val argNr = argMap(argName)
      val precO = None //Some(getPrecedence(n))
      val delim = n.child.find(_.label == "separator") match {
        case None => makeDelim(",")
        case Some(sep) => 
          sep.child.toList match {
            case Nil => makeDelim(",")
            case hd :: tl =>
              val dm = parseRenderingMarkers(hd, argMap).mkString("")
              makeDelim(dm)
          } 
      }
      
      argMap(argName) match {
        case ProtoArg(nr) => SeqArg(nr, delim, precO) :: Nil
        case ProtoVar(nr) => Var(nr, false, Some(delim), precO) :: Nil
        case ProtoSub(nr) => throw STeXParseError("cannot have sequence sub as arg", None)
      }
    case "mstyle" => n.child.toList.flatMap(parseRenderingMarkers(_, argMap))
    case "merror" => Nil
    case "mroot" => Delim("√") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap))
    case "msqrt" => //SqrtMarker(n.child.toList.flatMap(parseRenderingMarkers(_, argMap))) :: Nil
      Delim("√") :: Delim("(") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap)) ::: List(Delim(")"))
    case "none" => Nil
  }
  
  def makeDelim(s : String) : Delim = {
    val str = s.replaceAll("…", "...")
    str match {
      case "(" => Delim("&#40;")
      case ")" => Delim("&#41;")
      case _ => Delim(str)
    }
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
      case group :: project :: "source" :: tl => //MathHub URI
        tl match {
          case Nil => throw STeXParseError("Expected group/project/'source'/name for document path. Got: " + s + " (empty document name)", None)
          case l =>  
            val base = mhBase / group / project 
            l.init.foldLeft(base)((dc, x) => dc / x) / (l.last + ".omdoc")
        }
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
  
  def resolveSPath(tnameSO : Option[String], snameS : String, container : MPath) : GlobalName = {
    val tpaths = controller.globalLookup.visible(OMMOD(container)) //includes container
    //filter those that match tname
    val options = tpaths.map(_.toMPath).filter(t => tnameSO.map(t.name.last.toPath == _).getOrElse(true))
    options.toList match {
      case Nil => throw ParseError("Cannot resolve module for " + tnameSO.getOrElse("*") + "?" + snameS + " from theory " + container.toPath) //perhaps not included
      case l => 
        val matches = l.map(controller.get(_)) flatMap {
          case d : DeclaredTheory => 
            d.getConstants.filter(_.name.last.toPath == snameS)
          case _ => None
        }
        matches match {
          case Nil => throw ParseError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + " and symbol=" + snameS + ". No matching modules " + l +  " , contain symbol " + snameS ) 
          case hd :: Nil => hd.path
          case _ => throw ParseError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + " and symbol=" + snameS + ". Several matching symbols: " + matches.map(_.path)) 
        }
    }
  }
}
