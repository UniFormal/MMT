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
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.parser._


import scala.xml.{Node,Elem,NamespaceBinding}

class STeXImporter extends Compiler with Logger {
  val key : String = "stex-omdoc"
  override val logPrefix = "steximporter"    
 
    
  override def init(controller : Controller) {
    this.controller = controller
    report = controller.report
  }
    
  def includeFile(name : String) : Boolean = name.endsWith(".omdoc") //stex/latexml generated omdoc
  
  def buildOne(inFile : File, dpath : DPath, outFile : File) : List[Error] = {
    val src = scala.io.Source.fromFile(inFile.toString)
	val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
	val node : Node = cp.document()(0)
	src.close	
	
    val errors = translateArticle(node)(dpath)
 
    val docXML = controller.getDocument(dpath).toNodeResolved(controller.memory.content)
    outFile.toJava.getParentFile().mkdirs() //TODO shouldn't MMT API handle this ?
    val out = new java.io.PrintWriter(outFile.toJava)
    val pp = new scala.xml.PrettyPrinter(100,2)
    out.write(pp.format(docXML))

    out.close()
    errors foreach (throw _)
    if (errors != Nil) {
      log("Errors: " + errors.mkString("\n"))
    }
    errors
  }
  
  def compileOne(inText : String, dpath : DPath) : (String, List[Error]) = {
   
    val node = scala.xml.XML.loadString(clearXmlNS(inText))    
    val cleanNode = scala.xml.Utility.trim(node)
    val errors = translateArticle(cleanNode)(dpath)
    
    errors match {
      case Nil => //returning result
        val docXML = controller.getDocument(dpath).toNodeExpanded(controller.memory.content, controller.memory.narration)
        (docXML.toString, Nil)
      case _ => //reporting errors
        ("",errors)
    }

  } 
  
  //just fixing here an latexml bug, to be removed when it's fixed there
  //removing bad xmlns reference that conflicts with MMT's xmlns
  def clearXmlNS(s : String) : String = {
   val s1 = s.replaceAll("xmlns=\"http://www.w3.org/1999/xhtml\"","")   
   s1.replaceFirst("xmlns=\"http://omdoc.org/ns\"","xmlns=\"http://www.w3.org/1999/xhtml\"")   
  }
  

  val xmlNS = "http://www.w3.org/XML/1998/namespace"
  val omdocNS = "http://omdoc.org/ns"
  
  private def translateArticle(n : Node)(implicit dpath : DPath) : List[Error] = {
    var errors : List[Error] = Nil
    try {
      n.label match {
        case "omdoc" => 
          implicit val doc = new Document(dpath)
          controller.add(doc)
          errors ++= n.child.map(translateTheory).flatten
      }
    } catch {
      case e : Error => errors ::= e
    }
    
    errors
  }
  
  
  private def translateTheory(n : Node)(implicit doc : Document) : List[Error] = {
    var errors : List[Error]  = Nil
    try {
      n.label match {
        case "theory" => 
          val id =  (n \ s"@{$xmlNS}id").text
          val name = LocalPath(List(id))
          val thy = new DeclaredTheory(doc.path, name, None)
          implicit val mpath = thy.path 
          val ref = MRef(doc.path, mpath, true)
          controller.add(ref)
          controller.add(thy)
          errors ++= n.child.map(translateDeclaration).flatten
        case "metadata" => //ignore for now
        case "omgroup" => 
          val errs = n.child.map(translateTheory)
          errors ++= errs.flatten
        case _ =>           
          val no = translateCMP(n)(doc.path, mmt.mmtcd) //defaulting to mmtcd for context in parsing objects
          val nr = new PlainNarration(doc.path, no)
          doc.add(nr)
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }
  
  def _tmp_translate_hardcoded_uris(fromS : String) : String = fromS match {
    case "../../../slides/extcds/omstd/arith1.omdoc#arith1" => "http://docs.omdoc.org/smglo/sTeX/arith.omdoc#arith"
    case "../../../slides/extcds/omstd/relation1.omdoc#relation1" => "http://docs.omdoc.org/smglo/sTeX/relation.omdoc#relation"
    case _ => fromS
  }  
  
  private def translateDeclaration(n : Node)(implicit doc : Document, mpath : MPath) : List[Error] = {
    var errors : List[Error] = Nil 
    try {
      n.label match {
        case "imports" => //omdoc import -> mmt (plain) include
          var fromS = (n \ "@from").text
          fromS = _tmp_translate_hardcoded_uris(fromS)
          val from = fromS.split("#").toList match {
            case dpathS :: localPathS :: Nil =>
              val dpath = if (dpathS.startsWith("..//") && dpathS(4).isLetter) {
                Path.parseD(dpathS.substring(4), doc.path)
              } else {
                DPath(URI(dpathS))
              }
              dpath ? LocalPath(List(localPathS))
            case _ => throw ParseError("invalid stex mpath: " + fromS)
          }
          if (from != mpath) {
            val include = PlainInclude(from, mpath)
            controller.add(include)
          }
        case "symbol" => //omdoc symbol -> mmt constant
          val nameS = (n \ "@name").text
          val name = LocalName(nameS)
          val const = new Constant(OMMOD(mpath), name, None, TermContainer(None), TermContainer(None), None, presentation.NotationContainer())
          controller.add(const)
        case "definition" => 
          val nameS =  (n \ s"@{$xmlNS}id").text
          val name = LocalName(nameS)
          val targetsS = (n \ "@for").text.split(" ")
          val targets = targetsS map {s =>
            mpath ? LocalName(s) //TODO handle non-local references 
          }
          //only interested in CMP for now
          n.child.find(_.label == "CMP") match {
            case None => log("no CMP: " + n.child.mkString("\n"))//nothing to do  
            case Some(cmpXML) =>
              val cmp = translateCMP(cmpXML)(doc.path, mpath)
              val dfn = new Definition(doc.path, targets.toList, cmp)
              doc.add(dfn)
          }
        case "notation" =>
          val prototype = n.child.find(_.label == "prototype").get
          val rendering = n.child.find(_.label == "rendering").get
          val notation = makeNotation(prototype.child.head, rendering.child.head)(doc.path, mpath)
          val cd = (n \ "@cd").text
          val name = (n \ "@name").text
          val refPath = Path.parseM("?" + cd, doc.path)
          val refName = refPath ? LocalName(name)
          val c = controller.memory.content.getConstant(refName, p => "Notation for nonexistent constant " + p)
          val const = new Constant(c.home, c.name, c.alias, c.tpC, c.dfC, c.rl, presentation.NotationContainer(notation))
          controller.memory.content.update(const) 
          val res = controller.memory.content.getConstant(refName, p => "Notation for nonexistent constant " + p)
        
        case "metadata" => //TODO  
        case _ =>
          val nr = new PlainNarration(doc.path, Narration.parseNarrativeObject(n)(doc.path))
          doc.add(nr)
      }
    } catch {
      case e : Throwable => throw e //uncomment in debug mode
      case e : Error => errors ::= e
      case e : Throwable => log("WARNING: declaration ignored because of error " + e.getMessage() + "\n" + n.toString)
    }
    errors
  }
  
  def makeNotation(proto : scala.xml.Node, rendering : scala.xml.Node)(implicit dpath : DPath, mpath : MPath) : parser.TextNotation = {
    val argMap : collection.mutable.Map[String, Int] = new collection.mutable.HashMap() 
    val symName = proto.label match {
      case "OMA" => 
        val n = proto.child.head
        n.label match {
          case "OMS" => 
            val cd = (n \ "@cd").text
            val name = (n \ "@name").text
            val refPath = Path.parseM("?" + cd, dpath)
            //computing map of arg names to positions
            proto.child.tail.zipWithIndex foreach {p => 
              val name = (p._1 \ "@name").text
              argMap(name) = p._2 + 1 //args numbers start from 1
            }
            refPath ? LocalName(name)
          case _ => throw ParseError("invalid  prototype" + proto)
        }
      case "OMS" => 
        val cd = (proto \ "@cd").text
        val name = (proto \ "@name").text
        val refPath = Path.parseM("?" + cd, dpath)
        refPath ? LocalName(name)
      case _ => throw ParseError("invalid prototype" + proto + rendering)
    }
   
   val markers =  parseRenderingMarkers(rendering, argMap.toMap)
   val precS = try {
     (rendering \ "@precedence").text.toInt
   } catch {
     case _ : Exception => 1
   }
   new TextNotation(symName, Mixfix(markers), presentation.Precedence.integer(precS))
  }
  
  def parseRenderingMarkers(n : scala.xml.Node,argMap : Map[String, Int])(implicit dpath : DPath, mpath : MPath) : List[Marker] = n.label match {
    case "mrow" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
    case "mmultiscripts" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList //treated as mrow because not sure what it should do
    case "msub" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList //treated mrow because there is no subscript support in MMT TextNotation
    case "msup" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList //treated mrow because there is no superscript support in MMT TextNotation
    case "mpadded" => n.child.flatMap(parseRenderingMarkers(_, argMap)).toList
    case "mo" => makeDelim(n.child.mkString) :: Nil
    case "mi" => makeDelim(n.child.mkString) :: Nil //for now treated exactly like mo        
    case "mn" => makeDelim(n.child.mkString) :: Nil //for now treated exactly like mo
    case "mtext" => makeDelim(n.child.mkString) :: Nil
    case "text" => makeDelim(n.child.mkString) :: Nil
    case "mfrac" => 
      val above = parseRenderingMarkers(n.child(0), argMap)
      val below = parseRenderingMarkers(n.child(0), argMap)
      val fraction = FractionMarker(above, below, true) //true => render line
      List(fraction)
    case "mtd" => Delim("[&") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap)) ::: List(Delim("&]"))
    case "mtr" => Delim("[/") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap)) ::: List(Delim("/]")) 
    case "mtable" => Delim("[[") :: n.child.toList.flatMap(parseRenderingMarkers(_, argMap)) ::: List(Delim("]]")) 
    case "render" => 
      val argName = (n \ "@name").text
      val argNr = argMap(argName)
      Arg(argNr) :: Nil
    case "iterate" =>
      val argName = (n \ "@name").text
      val argNr = argMap(argName)
      n.child.find(_.label == "separator") match {
        case None => SeqArg(argNr, makeDelim(",")) :: Nil
        case Some(sep) => 
          sep.child.toList match {
            case Nil => 
              SeqArg(argNr, makeDelim(" ")) :: Nil
            case hd :: tl =>
              val delim = parseRenderingMarkers(hd, argMap).mkString(" ")
              SeqArg(argNr, makeDelim(delim)) :: Nil
          } 
        }
    case "none" => Nil
  }
  
  def makeDelim(s : String) : Delim = {
    s match {
      case "…" => Delim("...")
      case _ => Delim(s)
    }
  }
  
  def parseSourceRef(n : scala.xml.Node) : SourceRegion = {
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
         case _ => throw new Exception("Invalid STeX source reference " + srcrefS)
       }
       val trangeIdx = toS.indexOf("from=") + "from=".length
       val trangeS = toS.substring(trangeIdx)
       val tvalsS = trangeS.split(";").toList
       val (tl, tr) = tvalsS match {
         case lS :: rS :: Nil => 
           val l = lS.toInt 
           val r = rS.toInt
           (l,r)
         case _ => throw new Exception("Invalid STeX source reference " + srcrefS)
       }
       
       
       
    

       
       
       
          
      case _ => throw new Exception("Invalid STeX source reference " + srcrefS)
    }
    
    throw new Exception("TODO")
  }
  
  def translateCMP(n : scala.xml.Node)(implicit dpath : DPath, mpath : MPath) : NarrativeObject = n.label match {
    case "term" => 
      val cd = (n \ "@cd").text
      val name = (n \ "@name").text
      val text = n.child.mkString(" ")
      val refPath = Path.parseM(cd + ".omdoc?" + cd, dpath)
      val refName = refPath ? LocalName(name)
      new NarrativeRef(refName, text) 
    case "idx" | "idt" => translateCMP(n.child.head)  //ignore and take first child -- should get to a "term" elem in 2/1 steps
    case "#PCDATA" => new NarrativeText(n.toString)
    case "OMOBJ" => new NarrativeTerm(translateTerm(n))
    case _ => 
      val children = n.child.map(translateCMP)
      new NarrativeNode(n, children.toList)
  }
  
  def translateTerm(n : scala.xml.Node)(implicit dpath : DPath, mpath : MPath) : Term = {
    def rewriteNode(node : scala.xml.Node) : scala.xml.Node = node.label match {
      case "OMS" => 
        var cd =  xml.attr(node, "cd")
        cd = _tmp_hardcoded_cd_rewrite(cd)
        val name = xml.attr(node, "name")
        val docName = cd + ".omdoc"
        val doc = mpath.doc.^! / docName 
        <om:OMS base={doc.toPath} module={cd} name={name}/>
      case "#PCDATA" => new scala.xml.Text(node.toString)
      case _ => new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteNode) :_*)
    }
    println(rewriteNode(n))
    Obj.parseTerm(rewriteNode(n), dpath)
  }
  
  
  def _tmp_hardcoded_cd_rewrite(cd : String) : String  = cd match {
    case "arith1" => "arith"
    case "relation1" => "relation"    
    case _ => cd
  }
  
  
  
  
  
}