package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.informal._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

import scala.compat.Platform.EOL
import scala.xml.Node

class GAPDocError(msg: String, extraMsg: Option[String], severity: Option[Level.Level]) extends Error(msg) {
  override val extraMessage = extraMsg.getOrElse("")
  override def level = severity.getOrElse(super.level)
}

object GAPDocError {
  def from(e: Exception, msg: String, severity: Option[Level.Level] = None): GAPDocError = {
    val err = new GAPDocError(msg, Some(e.getMessage), severity)
    err.setStackTrace(e.getStackTrace)
    err
  }
}

class GAPDocImporter extends Importer {
  val key = "gapdoc-omdoc"
  def inExts = List("xml")
  override val logPrefix = "gapdocimporter"
  var count = 0

  var errCount = 0
  var succCount = 0
  var groups = new scala.collection.mutable.HashMap[Path, List[String]]()

  def add(s : StructuralElement, desc : String = "")(implicit errorCont : ErrorHandler) = {
    try {
        controller.add(s)
        groups(s.path) = List(desc)
        succCount += 1
    } catch {
      case e : AddError =>
        errorCont(new GAPDocError(s.name.toString + " already exists", None, Some(Level.Error)))
        groups(s.path) ::= desc
        errCount += 1
    }
  }

  def importDocument(bt : BuildTask, index : Document => Unit) : BuildResult = {
    try {
      errCount = 0
      succCount = 0
      groups = new scala.collection.mutable.HashMap[Path, List[String]]()

      val src = scala.io.Source.fromFile(bt.inFile.toString)
      val cp = scala.xml.parsing.ConstructingParser.fromSource(src, preserveWS = true)
      val node: Node = cp.document()(0)
      src.close()
      val errCont = new HandlerWithTreshold(bt.errorCont, Level.Error)
      parseBook(node)(bt.narrationDPath, errCont)
      val doc = controller.getDocument(bt.narrationDPath)

      index(doc)

      /* Debugging, should be removed when this is no longer experimental */
      // statistics
      println(errCount)
      println(succCount)
      // groups of duplicate symbols
      groups foreach {p =>
        if (p._2.length > 1) {
          println(p._1.last + ":")
          p._2.foreach{s =>
            println("    " + s)
          }
          println("")
        }
      }
      // all gapdoc-declared symbol names
      println("##############")
      groups foreach {p =>
        if (!p._1.last.startsWith("narr.") && !(p._1.last.startsWith("[") && p._1.last.endsWith("]"))) {
          println(p._1.last)
        }
      }
      println("##############")
      /* End Debugging */

      BuildResult.empty
    } catch {
      case e : Exception =>
        println(e.getMessage)
        println(e.getStackTrace().mkString("", System.lineSeparator(), System.lineSeparator()))
        bt.errorCont(GAPDocError.from(e, "Unknown error in importDocument"))
        BuildFailure(Nil, Nil)
    }
  }

  def parseBook(n : Node)(implicit dpath : DPath, errorCont : ErrorHandler): Unit = {
    n.label match {
      case "Book" =>
        val doc = new Document(dpath, FileLevel)
        add(doc)
        val tname = LocalName("thy")
        count += 1
        val thy = Theory.empty(dpath, tname, Theory.noMeta)
        val ref = MRef(dpath, thy.path)
        add(ref)
        add(thy)
        implicit val mpath = thy.path

        n.child.foreach(parseInDoc)
    }
  }

  def parseInDoc(n: Node)(implicit mpath: MPath, errorCont: ErrorHandler): Unit = {
    n.label match {
      case "Body" =>
        n.child.foreach(parseInBody)
      case l => log("Ignoring inDoc element " + l)
    }
  }

  def parseInBody(n : Node)(implicit mpath: MPath, errorCont : ErrorHandler): Unit = {
    n.label match {
      case "Chapter" | "Section" | "Subsection" => //recursing & ignoring, treating as if flattened
        n.child.foreach(parseInBody)
      case "ManSection" => parseManSection(n)
      case l => log("Ignoring inDoc element " + l)
    }
  }

  val declTypes = List("Filt",  "Func",  "Oper","Attr","Prop") //  "Meth")  ignoring Methods for now

  def parseManSection(n : Node)(implicit mpath: MPath, errorCont : ErrorHandler): Unit = {
    var lastDecl : Option[(Declaration, Node)] = None
    n.child foreach {c => c.label match {
      case dtype if (declTypes.contains(dtype)) =>
        val gname = (c \ "@Name").text
        var name = LocalName(gname)
        val label = (c \ "@Label").text
        if (!label.isEmpty) { //refinement of a pre-existing constant

        }
        val arg = (c \ "@Arg").text // seems to be unnecessary
        val const = Constant(OMMOD(mpath), name, Nil, TermContainer(None), TermContainer(None), None, NotationContainer.empty())
        add(const, c.toString.substring(0, c.toString.indexOf(">") + 1))
        lastDecl = Some(const, c)
      case "Returns" =>
        lastDecl match {
          case Some(d) =>
            val const = Constant(OMMOD(mpath), d._1.name, Nil, TermContainer(None), TermContainer(None), None, NotationContainer.empty())
            //add(const) // should update
          case _ => errorCont(new GAPDocError("Found Returns tag without preceeding concept entry", None, Some(Level.Warning)))
        }
      case "Var" | "Fam" | "InfoClass" => //TODO
      case "Description" =>
        val name = "narr." + count
        count += 1
        val obj = parseObject(<div> {c.child} </div>)(mpath ? name, errorCont)
        val narr = PlainNarration(OMMOD(mpath), LocalName(name), obj, LocalName.empty)
        add(narr)
      case l => log("Ignoring inDoc element " + l)
      }
    }
  }

  def resolveRef(ref : Node)(implicit spath : GlobalName, errorCont : ErrorHandler) : Term =  {
    declTypes.find(d => !(ref \ ("@" + d)).text.isEmpty) match {
      case Some(dtype) =>
        val rname = (ref \ ("@" + dtype)).text
        var refName = LocalName(dtype) / rname
        val label = (ref \ "@Label").text
        if (!label.isEmpty) refName /= label.split(" ").mkString("_")
        val constants = controller.library.get(spath.module).getDeclarations.collect {case c : Constant => c}
        constants.filter(c => c.name.steps.length >= 2 && c.name.steps.tail.head.toPath == rname) match {
          case Nil =>
            if (spath.name.toPath == refName.toPath) {
              parseObject(<b> {spath.name} </b>)
            } else {
              errorCont(new GAPDocError("Dangling Ref, nonexistent symbol: " + rname, None, Some(Level.Warning)))
              makeTextObj(rname.toString)
            }
          case l =>
            l.find(_.name.toPath == refName.toPath) match {
              case Some(d) => OMS(d.path)
              case None =>
                val options = l.map(_.name.steps.head.toPath)
                errorCont(new GAPDocError(s"Dangling Ref, wrong decl type, given $refName, valid options are ${options.mkString(",")}",
                    None, Some(Level.Warning)))
                makeTextObj(rname.toString)
            }
        }
      case _ =>
        errorCont(new GAPDocError("Found Ref tag without valid attribute", None, Some(Level.Warning)))
        makeTextObj("#invalidRef")

    }
  }

  def parseObject(n : Node)(implicit spath : GlobalName, errorCont : ErrorHandler) : Term = {
    n.label match {
      case "Ref" => resolveRef(n)
      case "#PCDATA" => makeTextObj(n.toString)
      case _ =>
        val cterms = n.child.zipWithIndex.map(p => parseObject(p._1) -> List(p._2))
        FlexiformalNode(n, cterms.toList)
    }
  }

  def makeTextObj(s : String) : Term = {
    val elems = s.toCharArray.toList
    def makeChildren(elems: List[Char], buffer: String): List[Node] = elems match {
      case Nil if buffer == "" => Nil
      case Nil => scala.xml.Text(buffer) :: Nil
      case hd :: tl if hd == ' ' =>
        scala.xml.Text(buffer) :: scala.xml.EntityRef("#160") :: makeChildren(tl, "")
      case hd :: tl => makeChildren(tl, buffer + hd)
    }

    val children = makeChildren(elems, "")
    FlexiformalXML(<span type="XML">
      {children}
    </span>)

    FlexiformalXML(scala.xml.Unparsed(s))
  }
}
