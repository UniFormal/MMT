
import info.kwarc.mmt.api.{GeneralError, Level}
import info.kwarc.mmt.api.archives.Update
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.utils.{EmptyList, File, FilePath}
import info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api.ontology.RelationalReader
import info.kwarc.mmt.api.archives.Archive

/**
object Tester extends App {
  val filetest = File("C:")/"mmt2"
  if (filetest.exists)
    {
      print("exists")
    }
  else {throw GeneralError("Does not exist")}
}

object Translator extends MagicTest{
  def run : Unit = {
    val a = controller.backend.getArchive( "Happening")
    //println(controller.backend.getArchives.map(_.id).mkString("; "))
    println(a)
    val pres = new MMTSyntaxPresenter()
    controller.extman.addExtension(pres)
    pres.build(a.get, Update(Level.Warning), FilePath("/"))
  }
}

object Translator2 extends MagicTest{
  import info.kwarc.mmt.api.presentation._
  import info.kwarc.mmt.api.archives._
  def run : Unit = {
    val a = controller.backend.getArchive("MMT/LATIN").get
    val pres = new MMTSyntaxPresenter()
    controller.extman.addExtension(pres)
    // pres.build(a.get,Update(Level.Warning),FilePath(""))
    controller.buildArchive(List(a.id), "present-text-notations", Build, FilePath(""))
  }
}

object MyReader extends MagicTest{
  def run : Unit = {
    val a = controller.backend.getArchive( "MitM/algebra")
    val read =new RelationalReader()
    a map read.oncePerArchive
  }
}

object MyReader2 extends MagicTest{
  def run : Unit = {
    val a = controller.backend.getArchive( "MitM/Foundation")
    val mygraph = a.get.allContent
    println{mygraph}
  }
}



// object testserver extends Server (8080, ) {}

class SemanticComputer {
  /** Takes an MMT archive and computes argumentation semantics on it.
    *
    */
  def main: Unit = {

  }

  def grounded: Unit = {

  }

}

/**
  *
  */
  *
  **/

object Graphtester extends MagicTest {
  def run : Unit = {
    val test = WebQuery("type=archivegraph&graphdata=MMT/urtheories&semantic=grounded")
    // println(test)
    println( test("graphdata"))
    //val serve = new JSONBasedGraphServer()



  }


}

object Jsonprinter extends MagicTest {
  def run : Unit = {
    new JSONBasedGraphServer("GET", Map(("uri", "MMT/urtheories" ), ))
  }
}