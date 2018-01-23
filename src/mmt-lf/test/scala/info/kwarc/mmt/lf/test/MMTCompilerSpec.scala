package scala.info.kwarc.mmt.api.test
/*
import info.kwarc.mmt.api._
import archives.BuildManager
import frontend._
import utils._
import org.scalatest._

class MMTCompilerSpec extends FlatSpec with Matchers {
  val controller = new Controller()
  val urLocation = "test/resources/MMTCompilerTest/urtheories"
  val testLocation = "test/resources/MMTCompilerTest/examples"

  "Adding an archive" should "not throw an exception" in {
    controller.handleLine("archive add " + urLocation)
    controller.handleLine("archive add " + testLocation)
  }

  val docbase = new DPath(URI("http://docs.omdoc.org/test/"))
  val dpaths = List(docbase / "int.omdoc",
    docbase / "fol.omdoc",
    docbase / "literals.omdoc",
    docbase / "arithmetic_rules.omdoc")

  val modbase = new DPath(URI("http://cds.omdoc.org/"))
  val mpaths = List((modbase / "examples") ? "Int",
    (modbase / "examples") ? "FOL",
    (modbase / "examples") ? "FOLEQNatDed",
    (modbase / "examples") ? "Numbers",
    (modbase / "examples") ? "NatLiterals",
    (modbase / "examples") ? "Sums")

  val folEqNatDedMPath = (modbase / "examples") ? "FOLEQNatDed"

  val intMPath = (modbase / "examples") ? "Int"
  val spaths = List((modbase / "examples") ? "Int" ? "minus",
    (modbase / "examples") ? "FOL" ? "exists",
    (modbase / "examples") ? "FOLEQNatDed" ? "congF",
    (modbase / "examples") ? "Numbers" ? "forall",
    (modbase / "examples") ? "Sums" ? "sum_example")

  //controller.handleLine("log console")
  //controller.handleLine("log+ archive")
  //controller.handleLine("log+ build")
  //controller.handleLine("log+ controller")

  // cleaning archive manually to begin testing
  val archiveFolder = new java.io.File(testLocation)
  archiveFolder.listFiles().toList foreach { f =>
    val name = f.getName()
    if (f.isDirectory() && name != "source" && name != "META-INF") {
      File(f).deleteDir
    }
  }

  "Indexing an archive" should "not throw an error" in {

    controller.handleLine("extension info.kwarc.mmt.lf.Plugin")
    controller.handleLine("build urtheories mmt-omdoc")
    controller.handleLine("build test mmt-omdoc")
    controller.handleLine("build test mmt-omdoc")
  }
  it should "allow access to documents from narration" in {
    controller.memory.clear
    val docs = dpaths.map(controller.get)
  }
  it should "allow access to modules from content" in {
    controller.memory.clear
    val thys = mpaths.map(controller.get)
  }
  it should "allow access to symbols from a module" in {
    controller.memory.clear
    val syms = spaths.map(controller.get)
  }
  "Checking an archive" should "not throw an error" in {
    controller.handleLine("archive test check")
  }

  "Cleaning an archive" should "remove MMT-generated files" in {
    controller.handleLine("build test -mmt-omdoc")
    controller.memory.clear
    dpaths foreach { dpath =>
      a[BackendError] should be thrownBy {
        controller.get(dpath)
      }
    }
    mpaths foreach { mpath =>
      a[GetError] should be thrownBy {
        controller.get(mpath)
      }
    }
    spaths foreach { spath =>
      a[GetError] should be thrownBy {
        controller.get(spath)
      }
    }
  }
}
*/
