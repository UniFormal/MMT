package info.kwarc.mmt.api.test

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{Controller, Run}
import info.kwarc.mmt.api.test.matchers._
import info.kwarc.mmt.api.utils.URI
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

abstract class MMTTest(neededArchives : String*)(neededExtensions : String*) extends FlatSpec with Matchers with BeforeAndAfterAll
  with ContentCheckMatcher with ExtensionMatcher {
  lazy val controller: Controller = {
    Run.controller
  }

  // we need to run a bit of setup before tests are run
  override def beforeAll(): Unit = {
    configureController
    configureArchives
  }

  override def afterAll(): Unit = report.flush

  // and start the tests
  behavior of "MMT"

  // load all the extensions
  private final val defaultExtensions = List(
    "info.kwarc.mmt.api.ontology.AlignmentsServer",
    "info.kwarc.mmt.api.web.JSONBasedGraphServer"
  )
  val extensions = neededExtensions.toList ::: defaultExtensions
  shouldLoadExtensions

  // install all the archives that we need for tests\
  val archives = neededArchives.toList
  shouldInstallArchives
}

class APITest extends MMTTest()() {
  behavior of "APITest"
  lazy val brackets = (DPath(URI.http colon "cds.omdoc.org") / "mmt") ? "mmt" ? "brackets"
  it should "get a Constant: " in controller.getConstant(brackets)
}
