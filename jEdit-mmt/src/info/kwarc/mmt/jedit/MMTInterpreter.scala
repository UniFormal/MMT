package info.kwarc.mmt.jedit

import scala.util.Random

import org.gjt.sp.jedit.bufferset.BufferSetManager
import org.gjt.sp.jedit.buffer.JEditBuffer 
import org.gjt.sp.jedit._

import console._

//Class for the MMT interpreter
class MMTInterpreter extends console.Shell("mmt-interpreter") {
  val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
  //!!!
  //THIS WILL BE NORMALLY USED TO DISPLAY THE ERRORS IN THE
  //SCRACHPAD
  //!!!
  //val controller : Controller = mmt.controller
  val compileActions = mmt.compileActions
  val buffmanag : BufferSetManager = jEdit.getBufferSetManager()

  //Hashmap for loaded files list

  var filestack : scala.collection.mutable.Map[String, Buffer] =
    scala.collection.mutable.Map[String, Buffer]()

  //Default message
  override def printInfoMessage (output: Output) {
     output.print(null, """This is the MMT Interpreter.

For importing files, use '!import buffername' where
'buffername' is the buffer which you want to import.
""")
  }

  //Nothing to do here...
  override def stop (console: Console) {}

  //Main execution
  def execute(console: Console, input: String,
    output: Output, error: Output, command: String) {

    val buffers : Array[Buffer] = jEdit.getBuffers()
    val iformat = """!import (.*)""".r
    val uformat = """!unimport (.*)""".r
    val lformat = """(!list)""".r
    val cformat = """!(.*)""".r

    var scratch : Buffer = null
    var tbuffer : Buffer = null

    //Checks all the buffers for the MMTScratchpad.
    //If it exists, it loads it. If not, it creates and loads it.
    for (somebuffer <- buffers) {
      if(somebuffer.getName() == "Untitled-1") {
        scratch = somebuffer
      }
    }

    if (scratch == null) {
      scratch = BufferSetManager.createUntitledBuffer()
      scratch.setStringProperty("name", "MMTScratchpad")
      scratch.setReadOnly(true)
      buffmanag.addBuffer(null.asInstanceOf[View], scratch)
      output.print(null, scratch.toString())
    }

    //Matches commands
    command match {

      //Import file command
      case iformat(fname) =>
        if (filestack.contains(fname)) {
          output.print(null, "File already imported.")
        } else {
          for (somebuffer <- buffers) {
            if (somebuffer.getName() == fname) {
              tbuffer = somebuffer
            }
          }
          if (tbuffer == null) {
            error.print(null, "Buffer " + fname + " not found!")
          } else {
            filestack += (fname -> tbuffer)
            output.print(null, "Buffer imported!")
          }
        }

      //Unimport file command
      case uformat(fname) =>
        filestack.remove(fname) match {
          case Some(_) =>
            output.print(null, "Unimported buffer " + fname + "!")
          case None =>
            output.print(null, "Buffer is not imported!")
          case _ =>
        }

      //List all imported files
      case lformat(_) =>
        filestack.foreach((b : (String, _)) =>
          output.print(null, b._1))

      //Any other case
      case cformat(cname) =>
        error.print(null, "Invalid command " + cname + " !")

      //MMT command?
      case _ =>
        //!!!
        //It must always generate a new random-named file
        //The only problem is that when I save,
        //it still asks me for the directory and filename
        //Maybe deleting it after I use it will be enough
        //!!!
        var tmpstr : String = ""
        val tmpfile = Random.alphanumeric.take(7).mkString + ".txt"
        val tmpbuff = jEdit.openTemporary(null.asInstanceOf[View],
          "./", tmpfile, true)

        //writing into file all the imported files +
        //the command
        filestack.foreach((b : (_, Buffer)) =>
          tmpstr += "\n" + b._2.getText())
        tmpstr += "\n" + command

        tmpbuff.insert(0, tmpstr)
        tmpbuff.save(null.asInstanceOf[View], null)


        //!!!
        //THIS IS THE CODE WHICH WILL BE USED
        //TO TREAT ERRORS
        //!!!
        /*
        try {
          controller.build(tmpstr)
        } catch {
          case e: Error =>

        } finally {
          jEdit.closeBuffer(null.asInstanceOf[View], tmpbuff)
         
         }
         */

        //!!!
        //FOR NOW, I WILL USE THE ERROR DIALOG
        //!!!
        compileActions.compile(tmpfile)
    }
  }
}
