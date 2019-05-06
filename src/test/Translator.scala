//import info.kwarc.mmt.argsemcomp
import MagicTest.home
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.imps
import info.kwarc.mmt.contextgraphs.BinaryDefinedRelation


object Graphtester extends MagicTest("jgraph") {
  def run : Unit = {
    println(MagicTest.archiveRoot)
    //List(File(System.getProperty("user.home") / "MMT" / "myformalizations")find.(_exists).getOrElse(println("Does not exist"))
    hl("extension info.kwarc.mmt.argsemcomp.ArgumentationSemanticComputer")
    //val test = WebQuery("type=archivegraph&graphdata=MMT/urtheories&semantic=grounded")
    // println(test)
    //println( test("graphdata"))
    //val serve = new JSONBasedGraphServer()
  }
}

object DefinedBinaryRelationTester extends MagicTest("jgraph") {
  def run : Unit = {
    println(MagicTest.archiveRoot)
    //List(File(System.getProperty("user.home") / "MMT" / "myformalizations")find.(_exists).getOrElse(println("Does not exist"))
    hl("extension info.kwarc.mmt.contextgraphs.BinaryDefinedRelation")
    //hl("build BinaryDefinedRelationTest mmt-omdoc ")
    hl("build ALMANAC/BinaryDefinedRelation mmt-omdoc sth")
    //val test = WebQuery("type=archivegraph&graphdata=MMT/urtheories&semantic=grounded")
    // println(test)
    //println( test("graphdata"))
    //val serve = new JSONBasedGraphServer()
  }
}