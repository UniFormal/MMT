package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import frontend.MMTInterpolator
import frontend.actions.SetBase
import modules._
import objects._
import parser._
import utils._
import documents._
import console._
import info.kwarc.mmt.api.uom.SimplificationUnit
import org.gjt.sp.jedit.{View => JEditView, _}
import org.gjt.sp.jedit.bufferset._

import scala.collection.mutable

//Class for the MMT interpreter
class MMTInterpreter extends console.Shell("mmt-interpreter") {
  val mmt = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).
    asInstanceOf[MMTPlugin]
  val controller = mmt.controller
  val buffmanag : BufferSetManager = jEdit.getBufferSetManager
  val interpolator = new MMTInterpolator(controller)

  var scratchnspace = DPath(URI("http://cds.omdoc.org/scratch"))
  var scratchfname = "scratchpad.mmt"
  var scratchtheory = "scratch"
  var scratchcheck = true
  var scratchoutput = true

  val tmap = mutable.HashMap.empty[String, Int]

  //Default message
  override def printInfoMessage (output: Output) {
     output.print(null, """This is the MMT Interpreter.

For help, type '!help'.
""")
  }

  private var success : Option[Boolean] = None
  override def waitFor(console: Console) : Boolean = {
    synchronized {success.get}
  }

  //Nothing to do here...
  override def stop (console: Console) {}

  //Overrides prompt
  override def printPrompt(console : Console, output : Output){
    output.writeAttrs(
      ConsolePane.colorAttributes(new java.awt.Color(10, 170, 10)),
      "MMTinterp>")
    output.writeAttrs(
      ConsolePane.colorAttributes(java.awt.Color.BLACK)
        , " ")
  }

  //Main execution
  def execute(console: Console, input: String,
    output: Output, error: Output, command: String) = synchronized {

    val buffers = jEdit.getBuffers
    val helpcmd = """!help(.*)""".r

    val namecmd = """!setname (.*)""".r
    val chckcmd = """!check (.*)""".r
    val outpcmd = """!output (.*)""".r
    val thrycmd = """!theory (.*)""".r

    val cformat = """!(.*)""".r

    //Matches commands
    command match {

      case helpcmd(_) =>
        output.print(null, """
Usage:

Commands always begin with '!'

Returns this help screen:
!help

Sets scratch buffer name (default 'Untitled-1'):
!setname (name)

Sets type-checking (default 'true'):
!check (true|false)

Sets output to scratchpad (default 'true'):
!output (true|false)

Sets theory name in the scratchpad (default 'scratch'):
!theory (name)

Non-commands are MMT code.
""")

      case namecmd(name) =>
        scratchfname = name

      case chckcmd(cond) =>
        cond match {
          case "true" =>
            scratchcheck = true
          case "false" =>
            scratchcheck = false
          case _ =>
            output.print(null, "Illegal argument!")
        }

      case outpcmd(cond) =>
        cond match {
          case "true" =>
            scratchoutput = true
          case "false" =>
            scratchoutput = false
          case _ =>
            output.print(null, "Illegal argument!")
        }

      case thrycmd(name) =>
        scratchtheory = name

      case cformat(_) =>
        output.print(null, "Unknown command!")

        /*

         MMT command

         */
      case _ =>
        //Buffer stuff

        //Checks all the buffers for the MMTScratchpad.
        //If it exists, it loads it. If not, it creates and loads it.
        val scratch = buffers.find(b => b.getName == scratchfname).getOrElse {
          val scratch = jEdit.openTemporary(jEdit.getActiveView,
            jEdit.getSettingsDirectory,
            scratchfname,
            true)
          scratch.setReadOnly(true)
          buffmanag.addBuffer(null.asInstanceOf[JEditView], scratch)
          scratch.setMode("mmt")
          scratch.insert(0, "namespace " + scratchnspace +
            Reader.GS.toChar.toString + "\n\n")
          jEdit.commitTemporary(scratch)
          scratch.save(jEdit.getActiveView,
            jEdit.getSettingsDirectory + "/" + scratchfname, false, false)
          scratch
        }

        val currView = jEdit.getActiveView
        val caretPos = currView.getEditPane.
          getTextArea.getCaretPosition

        //Gets current theory and current metatheory
        val ct = MMTSideKick.getAssetAtOffset(currView, caretPos).flatMap(_.getScope).getOrElse {
          output.print(null, "Caret not positioned in theory!")
          throw new Exception
        }

        val mt = try{
          controller.handle(SetBase(ct))
          controller.globalLookup.get(ct) match {
            case d: Theory =>
              d.meta.map(" : " + _.toPath).getOrElse("")
            case _ => ""
          }
        } catch {
          case _ : Exception => ""
        }

        //Type checking and simplification

        //Parsing occurs here
        try {
          //TODO why does parsing happen twice in different ways?
          val u = interpolator.parse(List(command), Nil, None, check = false)
          val uS = controller.presenter.asString(u)

          output.print(null, "Internal form: " + u.toString)
          output.print(null, uS)

          //-------------
          val str = command
          val theory = ct

          val pu = ParsingUnit(SourceRef.anonymous(str), Context(theory), str, InterpretationInstructionContext(NamespaceMap(theory.doc)), None)
          val parser = controller.extman.get(classOf[Parser], "mmt").get
          val t = parser(pu)(ErrorThrower).toTerm
          //Type checking
           val stack = Stack(Context(theory))
          val solveout = checking.Solver.check(controller, stack, t)

          val (tR, tpR) = solveout match{
            case Left((a,b)) => (a,b)
            case Right(solver) =>
              solver.logState("jedit-interp")
              throw new Exception("Checking error!")
          }

          def simp(k: Term) = {
            val uE = controller.globalLookup.ExpandDefinitions(k, p => p.doc == scratchnspace)
            controller.simplifier(uE, SimplificationUnit(Context(theory), false,false, true))
          }
          val tRS = simp(tR)
          val tpRS = simp(tpR)

          output.print(null, "===DEBUG===")
          output.print(null, tRS.toString)
          output.print(null, tpRS.toString)
          output.print(null, "===========")
          //-------------

          if (scratchoutput){
            addDeclaration(scratch, scratchtheory, controller.presenter.asString(tRS), mt, ct.toString)
            scratch.setReadOnly(true)
          }

          success = Some(true)
        } catch {
          //Basic error handling
          case e : Error =>
            output.print(null, "Error:")
            output.print(null, e.toString)
            success = Some(false)
          case e : Exception =>
            val eM = GeneralError("unknown exception during parsing").setCausedBy(e)
            output.print(null, e.toString)
            e.printStackTrace()
            success = Some(false)
        } finally {
          controller.report("jedit", "MMTInterp - done.")
        }
        scratch.autosave

    }
    output.commandDone
  }
  /*
   Adds declaration to scratchpad
   buffer      : The scratchpad buffer
   theoryName  : The desired theory name to create
   declaration : The declaration which is to be put
   mtname      : Metatheory name
   tname       : Theory to include
   */
  def addDeclaration(buffer : Buffer, theoryName : String, declaration : String, mtname : String, tname : String) {

    val tpattern = ("""(?s).*theory """ +
      theoryName + """(.*?)=\n(.*?)""" + Reader.GS.toChar.toString).r
    val currText = buffer.getText

    val tcontent = (tpattern findFirstMatchIn currText).orNull

    //Checks if theory is existent or not :
    //If it is, it writes declarations in the new theory;
    //if not, creates a new theory
    if(tcontent == null) {
      buffer.insert(buffer.getText.length,
        "theory " + theoryName + mtname + " =\n  include " +
          tname + Reader.RS.toChar.toString + "\n  it = " +
          declaration + Reader.RS.toChar.toString + "\n" +
          Reader.GS.toChar.toString + "\n")
      tmap += (theoryName -> 0)

    } else {
      val scontent = tcontent.start(2)
      val econtent = tcontent.end(2)
      val content = tcontent.group(2)

      //Matches 'it'; if not found, appends a new it
      val itpattern = ("""it = (.*)""" +
        Reader.RS.toChar.toString + """\n""").r

      val itmatch = (itpattern findFirstMatchIn content).orNull
      val itcontent = itmatch.group(1)
      val ndeclaration = declaration.replaceAll("it", itcontent)

      try{
        buffer.remove(itmatch.start +
          scontent, 2)
          buffer.insert(itmatch.start +
            scontent, "c" + tmap(theoryName))
        tmap(theoryName) = tmap(theoryName) + 1
      } catch {
        case _ : Throwable =>
      }

      buffer.insert(econtent, "  it = " +
        ndeclaration + Reader.RS.toChar.toString + "\n")

      //Matches inclusion of theories
      if (!tname.startsWith(scratchnspace.toString)) {
        val inpattern = ("""include (.*)""" + Reader.RS.toChar.toString).r
        val inmap = inpattern findAllMatchIn content

        if (inmap exists {
          case inpattern(incl) => incl == tname
          case _ => false
        }) { } else {
          buffer.insert(scontent, "  include " +
            tname + Reader.RS.toChar.toString + "\n")
        }
      }
    }
  }
}
