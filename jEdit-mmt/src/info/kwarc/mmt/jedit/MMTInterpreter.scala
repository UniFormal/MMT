package info.kwarc.mmt.jedit

import scala.util._
import scala.collection.mutable.HashMap

import org.gjt.sp.jedit.bufferset._
import org.gjt.sp.jedit.buffer._
import org.gjt.sp.jedit._

import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api._
import frontend.{SetBase, MMTInterpolator}
import objects._

import console._

//Class for the MMT interpreter
class MMTInterpreter extends console.Shell("mmt-interpreter") {
  val mmt = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).
    asInstanceOf[MMTPlugin]
  val controller = mmt.controller
  val buffmanag : BufferSetManager = jEdit.getBufferSetManager()
  val interpolator = new MMTInterpolator(controller)

  var scratchfname = "Untitled-1"
  var scratchtheory = "scratch"
  var scratchcheck = true
  var scratchoutput = true

  val tmap = HashMap.empty[String, Int]

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

    val buffers = jEdit.getBuffers()
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
          val scratch = BufferSetManager.createUntitledBuffer()
          scratch.setReadOnly(true)
          buffmanag.addBuffer(null.asInstanceOf[View], scratch)
          scratch.setMode("mmt")
          scratch
        }

        val currView = jEdit.getActiveView()
        val caretPos = currView.getEditPane().
          getTextArea().getCaretPosition()

        //Gets current theory and current metatheory
        val ct = (try{
          (MMTSideKick.getAssetAtOffset(currView, caretPos) getOrElse
            null).getScope getOrElse null
        } catch {
          case _ : Throwable =>
            output.print(null, "Caret not positioned in theory!")
        }).asInstanceOf[Term]
        
        val mt = try{
          controller.handle(SetBase(ct.toMPath));
          //" : " + controller.globalLookup.get(ct.toMPath).parent.toString
          " : " + controller.globalLookup.getDeclaredTheory(ct.toMPath).toString
        } catch {
          case _ : Throwable=> ""
        }

        //Parsing occurs here
        try {
          val t = interpolator.parse(List(command), Nil, None, scratchcheck)
          val tP = controller.presenter.asString(t)

          output.print(null, tP)
          output.print(null, t.toString)

          if (scratchoutput){
            addDeclaration(scratch, scratchtheory,
              tP, mt, "?" + ct.toMPath.name.toString)
            scratch.setReadOnly(true)
          }

          success = Some(true)
        } catch {
          //Basic error handling
          case e : Error =>
            output.print(null, "Error:")
            output.print(null, e.toString)
            success = Some(false)
          case e : Throwable=>
            val eM = GeneralError("unknown exception during parsing").setCausedBy(e)
            output.print(null, e.toString)
            success = Some(false)
        } finally {
          controller.report("jedit", "MMTInterp - done.")
        }
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
  def addDeclaration(buffer : Buffer, theoryName : String,
    declaration : String, mtname : String, tname : String) {

    val tpattern = ("""(?s).*theory """ +
      theoryName + """(.*?)=\n(.*?)"""+Reader.GS.toChar.toString).r
    val currText = buffer.getText

    val tcontent = tpattern findFirstMatchIn currText getOrElse null

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
      val itpattern = """it =(.*)\n""".r
      val itmatch = itpattern findFirstMatchIn content getOrElse null

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
        declaration + Reader.RS.toChar.toString + "\n")

      //Matches inclusion of theories
      val inpattern = ("""include (.*)""" + Reader.RS.toChar.toString).r
      val inmap = inpattern findAllMatchIn content

      if (inmap exists {
        m => m match {
          case inpattern(incl) => (incl == tname)
          case _ => false
        }
      }) { } else {
        buffer.insert(scontent, "  include " +
          tname + Reader.RS.toChar.toString + "\n")
      }

    }

  }
}
