import info.kwarc.mmt.argsemcomp
import info.kwarc.mmt.imps

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

object Graphtester extends MagicTest("jgraph") {
  def run : Unit = {
    hl("extension info.kwarc.mmt.argsemcomp.ArgumentationSemanticComputer")
    //val test = WebQuery("type=archivegraph&graphdata=MMT/urtheories&semantic=grounded")
    // println(test)
    //println( test("graphdata"))
    //val serve = new JSONBasedGraphServer()
  }


}
/**
object Jsonprinter extends MagicTest {
  def run : Unit = {
    val server = new JSONBasedGraphServer
    server(ServerRequest("GET", Map(("uri", "MMT/urtheories"),("key", "archivegraph"),("id", "full")), Session, ("http://localhost:8080/graphs/tgview.html?type=archivegraph&graphdata=MMT/urtheories&highlight=null"),"?type=archivegraph&graphdata=MMT/urtheories&highlight=null", Body ))
  }
}**/

/* class SemanticComputer (f: JSON, sem: String, computer: String = "best") {
  def JsonToTgf (f: JSON) : String ="test"
  def TgfToJson (tgf: String) : JSON = null
  def CallComputer (tgf: String, computer: String) : String ="test"
} */
