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

abstract class STeXError(msg : String, severity : Option[Level.Level]) extends Error(msg) {
  override def level = severity.getOrElse(super.level)
}

class STeXParseError(val msg : String, val sref : Option[SourceRef], severity : Option[Level.Level]) extends STeXError(msg, severity) {
  private def srefS = sref.map(_.region.toString).getOrElse("")
  override def toNode = <error type={this.getClass().toString} shortMsg={this.shortMsg} level={level.toString} sref={srefS}> {Stacktrace.asNode(this)} </error>
}

object STeXParseError {
  def from(e : Exception, preMsg : Option[String] = None, sref : Option[SourceRef] = None, severity : Option[Level.Level] = None) : STeXParseError = {
    val pre = preMsg.map(_ + ": ").getOrElse("")
    val errMsg = pre + {
      e match {
        case er : Error => er.shortMsg
        case ex : Exception => ex.getMessage
      }
    }
    val err = new STeXParseError(errMsg, sref, severity)
    err.setStackTrace(e.getStackTrace)
    err
  }
}

class STeXLookupError(val msg : String, severity : Option[Level.Level]) extends STeXError(msg, severity)

object STeXLookupError {
  def from(e : Error, severity : Option[Level.Level]) : STeXLookupError = {
    val err = new STeXLookupError(e.shortMsg, severity)
    err.setStackTrace(e.getStackTrace)
    err
  }
}

class STeXImporter extends Importer {
  val key : String = "stex-omdoc"
  override val logPrefix = "steximporter"
  def includeFile(name : String) : Boolean = name.endsWith(".omdoc") //stex/latexml generated omdoc
  
  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
   }
  
  var docCont : Document => Unit = null
  
  override def apply(modifier: BuildTargetModifier, arch: Archive, in: List[String], args: List[String]) {
      val reqArgs = requiredArguments(modifier)
      if (reqArgs != args.length)
         throw ParseError("wrong number of arguments, required: " + reqArgs)
      modifier match {
         case up : Update => update(arch, args, up, in)
         case Clean  => clean(arch, args, in)
         case Build  => 
           //running twice, first to load all theories, then to successfully parse objects
           build(arch, args, in)
           build(arch, args, in)
      }
   }
  
  def importDocument(bt : BuildTask, cont : Document => Unit) {
    try {
      docCont = cont // to reindex document
      val src = scala.io.Source.fromFile(bt.inFile.toString)
      val cp = scala.xml.parsing.ConstructingParser.fromSource(src, true)
      val node : Node = cp.document()(0)
      src.close 
      translateDocument(node)(bt.narrationDPath, bt.errorCont)
      val doc = controller.getDocument(bt.narrationDPath)
      //if (sTeX.getLanguage(doc.path).isDefined) { //don't index signatures, will call manually after bindings are added
      cont(doc)
      //}
    } catch {
      case e : Exception => 
        val err = STeXParseError.from(e, Some("Skipping article due to exception"), None, Some(Level.Fatal))
        bt.errorCont(err)
    }
  }
  
  def compileOne(inText : String, dpath : DPath) : (String, List[Error]) = {
    val node = scala.xml.XML.loadString(inText) //clearXmlNS(    
    val cleanNode = node //scala.xml.Utility.trim(node)
    val errHandler = new ErrorContainer(None)
    translateDocument(cleanNode)(dpath, errHandler)
    val errors = errHandler.getErrors
    errors match {
      case Nil => //returning result
        val docXML = controller.getDocument(dpath).toNodeExpanded(controller.memory.content, controller.memory.narration)
        (docXML.toString, Nil)
      case _ => //reporting errors
        val doc = try {
          controller.getDocument(dpath).toNodeExpanded(controller.memory.content, controller.memory.narration).toString
        } catch {
          case e : Exception => ""
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
  private def translateDocument(n : Node)(implicit dpath : DPath, errorCont : ErrorHandler) : Unit = {
    n.label match {
      case "omdoc" => 
        //creating document and implicit theory
        implicit val doc = new Document(dpath)
        controller.add(doc)
        //recursing into children
        n.child.map(translateModule)
    }
  }

  /**
   * translate second-level, in-document elements (typically modules)
   */
  private def translateModule(n : Node)(implicit doc : Document, refadd : XRef => Unit = add, errorCont : ErrorHandler) : Unit = {
    val sref = parseSourceRef(n, doc.path)
    try {
      n.label match {
        case "theory" => //create theory
          val name = getName(n, doc)
          val thy = new DeclaredTheory(doc.path, name, None)
          val ref = MRef(doc.path, thy.path, true)
          refadd(ref)
          add(thy)
          n.child.map(translateDeclaration(_)(doc, thy, errorCont))
        case "omgroup" => //recurse to find internal modules
          var group = XRefGroup(doc.path, Nil)
          def refadd(ref : XRef) = {
            group = group.append(ref)
          } 
          n.child.map(n => translateModule(n)(doc, refadd, errorCont))
          add(group)
        case "metadata" => //TODO
        case "bibliography" => //TODO
        case "index" => //TODO
        case "oref" => 
          val href = (n \ "@href").text
          //val target = doc.path.^! / (href + ".omdoc")
          //val dref = new DRef(doc.path, target)
          val target = parseRelDPath(href, doc.path)
          val dref = new DRef(doc.path,target)
          refadd(dref)
        case "#PCDATA" | "#REM" => //Atom or Comment => do nothing
        case "tableofcontents" => //ignore for now
        case _ => 
          log("CREATED ANON THY because of label: " + n.label)
          translateDeclaration(n : Node)(doc, getAnonThy(doc.path), errorCont) 
      }
    } catch {
      case e : STeXError => errorCont(e)
      case e : Exception => 
        val err = STeXParseError.from(e, Some("Skipping module-level element " + n.label + " due to error") ,sref, None)
    }
  }
  
  /** 
   *  translate third level, in-module elements (typically declarations)
   */
  private def translateDeclaration(n : Node)(implicit doc : Document, thy : DeclaredTheory, errorCont : ErrorHandler) = {
    implicit val dpath = doc.path
    implicit val mpath = thy.path
    val sref = parseSourceRef(n, doc.path)
    try {
      n.label match {
        case "imports" => //omdoc import -> mmt (plain) include
          val fromS = (n \ "@from").text
          val from = parseMPath(fromS, doc.path)
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
          val name = getName(n, thy)
          val targetsS = (n \ "@for").text.split(" ")
          val targets = targetsS map {s => 
            val comps = s.split("\\?").toList
            resolveSPath(Some(comps.head), comps.tail.head, thy.path)
          }
          parseNarrativeObject(n)(dpath, thy, targets.head, errorCont) match {
            case None => //nothing to do  
            case Some(no) =>
              val dfn = new Definition(OMMOD(mpath), name, targets.toList, None, no)
              sref.map(ref => SourceRef.update(dfn, ref))
              add(dfn)
          }
        case "exercise" => 
          val name = getName(n, thy)
          parseNarrativeObject(n)(dpath, thy, thy.path ? name, errorCont) match {
            case None => //nothing to do  
            case Some(no) =>
              val prob = no
              val sol = n.child.find(_.label == "solution").flatMap(c => parseNarrativeObject(c)(dpath, thy, thy.path ? name, errorCont))
              
              val ex = new Exercise(OMMOD(mpath), name, prob, sol)
              sref.map(ref => SourceRef.update(ex, ref))
              add(ex)
          }
          
        case "omtext" => 
          val name = getName(n, thy)
          parseNarrativeObject(n)(dpath, thy, mpath ? name, errorCont) match {
            case Some(no) =>               
              val dfn = new PlainNarration(OMMOD(mpath), name, None, no)
              sref.map(ref => SourceRef.update(dfn, ref))
              add(dfn)
            case None => //nothing to do
          }
        case "notation" =>
          //getting symbol info
          val cd = (n \ "@cd").text
          val name = (n \ "@name").text
          val refPath = Path.parseM("?" + cd, NamespaceMap(doc.path))
          val refName = refPath ? LocalName(name)
          val c = controller.memory.content.getConstant(refName, p => "Notation for nonexistent constant " + p)
          //getting macro info
          try {
            val macro_name = (n \ "@macro_name").text
            val nrArgs = (n \ "@nargs").text.toInt
            val macroMk = Delim("\\" + macro_name)
            val notArgs = macroMk :: (0 until nrArgs).toList.flatMap(i => Delim("{") :: Arg(i + 1) :: Delim("}") :: Nil)
            val stexScope = NotationScope(None, "stex" :: "tex" :: Nil, 0)
            val texNotation = new TextNotation(Mixfix(notArgs), Precedence.integer(0), None, stexScope)
            c.notC.parsingDim.set(texNotation)
          } catch {
            case e : Exception => 
              val err = STeXParseError.from(e, Some("Notation is missing latex macro information"), sref, Some(Level.Warning))
              errorCont(err)
          }
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
          log("Parsing " + n.label + " as plain narration")
          val name = getName(n, thy)
          val nr = new PlainNarration(OMMOD(mpath), name, None, FlexiformalDeclaration.parseObject(rewriteNode(n))(doc.path))
          add(nr)
      }
    } catch {
      case e : Exception => 
        val sref = parseSourceRef(n, doc.path)
        val err = STeXParseError.from(e, Some("Skipping declaration element " + n.label + " due to error"), sref, None)
        errorCont(err)
      case e : Throwable => 
        val sref = parseSourceRef(n, doc.path)
        val err = new STeXParseError("Skipping declaration element " + n.label + " due to throwable: " + e.getMessage, sref, Some(Level.Error))
        errorCont(err)
    }
  }
  
  def parseNarrativeObject(n : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName, errorCont : ErrorHandler) : Option[FlexiformalObject]= {
    val sref = parseSourceRef(n, dpath) 
    n.child.find(_.label == "CMP").map(_.child) match {
      case Some(nodes) => 
        val cmpElems = nodes.map(translateCMP(_)(dpath, thy, spath, errorCont : ErrorHandler))
        val cmp = new FlexiformalNode(<span/>, cmpElems.toList)    
        Some(cmp)
      case None => 
        val err = new STeXParseError("No CMP in narrative object " + n.label, sref, Some(Level.Warning))
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
        val refPath = Path.parseM("?" + cd, NamespaceMap(dpath))
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
  
  def translateCMP(n : scala.xml.Node)(implicit dpath : DPath, thy : DeclaredTheory, spath : GlobalName, errorCont : ErrorHandler) : FlexiformalObject = {
    val sref = parseSourceRef(n, dpath)
    n.label match {
      case "term" => 
        val cd = (n \ "@cd").text 
        val name = (n \ "@name").text
        val objects = n.child.map(translateCMP).toList
        val refName = resolveSPath(Some(cd), name, thy.path)
        val role = (n \ "@role").text
        if (role == "definiendum") { //verbalization notation
          val ref = new FlexiformalRef(refName, None, objects, true)
          try {
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
            val doc = controller.getDocument(const.parent.doc, d => "cannot find parent doc for reindexing" + d)
            docCont(doc) //reindexing that document
          } catch {
            case e : NotFound => 
              val err = new STeXParseError("Cannot add verbalization notation, symbol not found: " + refName, sref, Some(Level.Warning))
          }
          ref
        } else {
          new FlexiformalRef(refName, None, objects)
        }
      case "#PCDATA" =>
        makeNarrativeText(n.toString)
      case "OMOBJ" => new FlexiformalTerm(translateTerm(n)(dpath, thy.path, errorCont))
      case _ => 
        val err = new STeXParseError("Unexpected element label in CMP: " + n.label, sref, Some(Level.Info))
        errorCont(err)
        val children = n.child.map(translateCMP)
        new FlexiformalNode(n, children.toList)
    }
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
  
  def rewriteNode(node : scala.xml.Node)(implicit mpath : MPath, errorCont : ErrorHandler) : scala.xml.Node = node.label match {
      case "OMS" => 
        val cd =  xml.attr(node, "cd")
        val name = xml.attr(node, "name")
        val sym = resolveSPath(Some(cd), name, mpath)
        <om:OMS base={sym.module.toMPath.parent.toPath} module={sym.module.toMPath.name.last.toPath} name={sym.name.last.toPath}/>
      case "#PCDATA" => new scala.xml.Text(node.toString)
      case "OME" => 
        val nchild = node.child.map(rewriteNode)
        new scala.xml.Elem(node.prefix, "OMA", node.attributes, node.scope, false, nchild :_ *)
      case "OMATTR" if node.child(1).label == "OMV" => //Complex Variable1
        val n = node.child(0).child(1) //TODO
        val nn = <om:OMFOREIGN>{scala.xml.Utility.trim(n).child.map(cleanNamespaces)}</om:OMFOREIGN>
        nn //TODO refactor cleanNamespaces to some xml helper object --duplicate in FlexiformalDeclarationObject
      case _ => new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteNode) :_*)
    }
  
  def translateTerm(n : scala.xml.Node)(implicit dpath : DPath, mpath : MPath, errorCont : ErrorHandler) : Term = {
    Obj.parseTerm(rewriteNode(n), NamespaceMap(dpath))
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
        case ProtoSub(nr) => throw new STeXParseError("cannot have sequence sub as arg", None, None)
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
      
  def getName(n : Node, container : Content) : LocalName = {
    try {
      val nameS =  (n \ s"@{$xmlNS}id").text
      LocalName(nameS)
    } catch {
      case e : Exception => LocalName("D" + container.children.length.toString)
    }
  }
  
  
   def parseRelDPath(s : String, base : DPath) : DPath = {
    val baseGroup = base.uri.path(0)
    val baseArchive = base.uri.path(1)
    //makes path absolute
    def removeRel(frags : List[String], acc : List[String]) : List[String] = frags match {
      case Nil => acc.reverse
      case ".." :: tl if acc.isEmpty =>  removeRel(tl, acc)
      case ".." :: tl => removeRel(tl, acc.tail)
      case hd :: tl => removeRel(tl, hd :: acc)
    }
    val plainFrags = removeRel(s.split("/").toList, Nil)
    val srcidx = plainFrags.indexOf("source")
    val fragPath = plainFrags.drop(srcidx + 1)

    val (group, archive) = srcidx match {
      case -1 => throw new STeXParseError("Invalid DPath, cannot produce group/project/'source'/path canonical form -- no 'source' " + s, None, None)
      case 0 => (baseGroup, baseArchive)
      case 1 => (baseGroup, plainFrags(0))
      case _ => (plainFrags(srcidx - 2), plainFrags(srcidx - 1))
    }
    
    fragPath match {
      case Nil => throw new STeXParseError("Invalid DPath. Got: " + s + " (empty document name)", None, None)
      case _ => 
        val base = mhBase / group / archive 
        fragPath.init.foldLeft(base)((dc, x) => dc / x) / (fragPath.last + ".omdoc")
    }
  }

/*  
  def parseDDPath(s : String) : DPath = {
    val parts = s.split("/").toList
    parts match {
      case group :: project :: "source" :: tl => //MathHub URI
        tl match {
          case Nil => throw new STeXParseError("Expected group/project/'source'/name for document path. Got: " + s + " (empty document name)", None, None)
          case l =>  
            val base = mhBase / group / project 
            l.init.foldLeft(base)((dc, x) => dc / x) / (l.last + ".omdoc")
        }
      case _ => throw new STeXParseError("Expected group/project/'source'/name for document path. Got: " + s, None, None)
    }
  }
*/
  
  def parseMPath(s : String, base : DPath) : MPath = {
    val parts = s.split("#")
    parts.toList match {
      case fpathS :: tnameS :: Nil => 
        val dpath = parseRelDPath(fpathS, base)
        dpath ? LocalName(tnameS)
      case _ => throw new STeXParseError("Expected 2 # separated parts for module path in: " + s, None, None)
    }
  }
  
  def resolveSPath(tnameSO : Option[String], snameS : String, container : MPath)(implicit errorCont : ErrorHandler) : GlobalName = {
    val defaultDoc = container.doc
    val defaultThy = tnameSO.map(LocalName(_)).getOrElse(container.name)
    val defaultSym = LocalName(snameS)
    val tpaths = controller.globalLookup.visible(OMMOD(container)) //includes container
    //filter those that match tname
    val tyhOptions = tpaths.map(_.toMPath).filter(t => tnameSO.map(t.name.last.toPath == _).getOrElse(true))
    val spath = tyhOptions.toList match {
      case Nil => // taking dpath from container
        val err = new STeXLookupError("Cannot resolve module for " + tnameSO.getOrElse("*") + "?" + snameS + 
            " from theory " + container.toPath, Some(Level.Warning)) //perhaps not included
        errorCont(err)
        defaultDoc ? defaultThy ? defaultSym
        case hd :: Nil => // found one matching thy, will use as default 
        hd ? defaultSym
      case l => //several options, must look inside theories to resolve
        val thys = l flatMap {p => 
          try {
            Some(controller.get(p)) 
          } catch {
            case e : Error => Nil
          }
        }
        
        val symOptions = thys flatMap {
          case d : DeclaredTheory => 
            d.getConstants.filter(_.name.last.toPath == snameS)
          case _ => Nil
        }
        symOptions match {
          case Nil => //adding error and defaulting
            val err = new STeXLookupError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + 
                " and symbol=" + snameS + ". No matching modules " + l +  " , contain symbol " + snameS, Some(Level.Warning))
            errorCont(err)
            defaultDoc ? defaultThy ? defaultSym
          case hd :: Nil => hd.path
          case _ => //adding error and defaulting
            val err = new STeXLookupError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + 
                " and symbol=" + snameS + ". Several matching symbols: " + symOptions.map(_.path), Some(Level.Warning))
            errorCont(err)
            defaultDoc ? defaultThy ? defaultSym
        }
    }
    spath
  }
}
