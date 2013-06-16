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



import scala.xml.Node

class STeXImporter extends Compiler with Logger {
  val key : String = "stex-omdoc"
  override val logPrefix = "steximporter"    
 
    
  def includeFile(name : String) : Boolean = name.endsWith(".omdoc") //stex/latexml generated omdoc
  
  def buildOne(inFile : File, dpathO : Option[DPath], outFile : File) : List[Error] = {
    val src = scala.io.Source.fromFile(inFile.toString)
	val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
	val node : Node = cp.document()(0)
	src.close
	val dpath = dpathO match {
      case Some(p) => p
      case None => DPath(URI(inFile.toString))
    }
	
    val errors = translateArticle(node)(dpath)
 
    val docXML = controller.getDocument(dpath).toNodeResolved(controller.memory.content)
    outFile.toJava.getParentFile().mkdirs() //TODO shouldn't MMT API handle this ?
    val out = new java.io.PrintWriter(outFile.toJava)
    val pp = new scala.xml.PrettyPrinter(100,2)
    out.write(pp.format(docXML))

    out.close()
    errors
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
          
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }
  
  
  private def translateDeclaration(n : Node)(implicit doc : Document, mpath : MPath) : List[Error] = {
    var errors : List[Error] = Nil 
    try {
      n.label match {
        case "imports" => //omdoc import -> mmt (plain) include
          val fromS = (n \ "@from").text
          val from = fromS.split("#").toList match {
            case dpathS :: localPathS :: Nil => DPath(URI(dpathS)) ? LocalPath(List(localPathS))
            case _ => throw ParseError("invalid stex mpath: " + fromS)
          }
          val include = PlainInclude(from, mpath)
          controller.add(include)
        case "symbol" => //omdoc symbol -> mmt constant
          val nameS = (n \ "@name").text
          val name = LocalName(nameS)
          val const = new Constant(OMMOD(mpath), name, None, TermContainer(None), TermContainer(None), None, None)
          controller.add(const)
          
        case "definition" => 
          val nameS =  (n \ s"@{$xmlNS}id").text
          val name = LocalName(nameS)
          val targetsS = (n \ "@for").text.split(" ")
          log(" " + targetsS.toList)
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
        case "notation" => //TODO
        case _ => println("Ignoring elem" + n)//TODO
      }
    } catch {
      case e : Error => errors ::= e
    }
    errors
  }
  
  def translateCMP(n : scala.xml.Node)(implicit dpath : DPath, mpath : MPath) : NarrativeObject = n.label match {
    case "term" => 
      val cd = (n \ "@cd").text
      val name = (n \ "@name").text
      val text = n.child.mkString(" ")
      val refPath = Path.parseM("?" + cd, dpath)
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
        val cd =  xml.attr(node, "cd")
        val name = xml.attr(node, "name")
        val docName = cd + ".omdoc"
        val doc = dpath.^! / docName //smglo library uses same name for doc and theories
        <om:OMS base={doc.toPath} module={cd} name={name}/>
      case _ => new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, node.child.map(rewriteNode) :_*)
    }
    
    Obj.parseTerm(rewriteNode(n), dpath)
  }
  
  
  
  
  
}