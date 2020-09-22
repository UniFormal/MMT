import Utils.utils
import sbt.Keys._
import sbt._
import travis.Config._
import travis.Matrix._

import scala.io.Source

val travisConfig = taskKey[TravisConfig]("Generate travis.yml configuration")
travisConfig := {
  val ourScalaVersion: String = scalaVersion.value

  // convenience wrapper to run an sbt task and an optional check
  def sbt(task: String, check: Option[String] = None): List[String] = List(
    s"cd src && (cat /dev/null | sbt ++$ourScalaVersion $task) && cd .."
  ) ::: check.toList

  // convenience functions for checks
  def file(name: String): Option[String] = Some("[[ -f \"" + name + "\" ]]")

  def identical(name: String): Option[String] = Some("(git diff --quiet --exit-code \"" + name + "\")")

  def dir(name: String): Option[String] = Some("[[ -d \"" + name + "\" ]]")

  val LinuxTesting = MatrixSet(
    Bionic, Language("scala"), Env(Map(("SBT_VERSION_CMD", "\"^validate\""))),
    OpenJDK14
  )

  // in principle we would test OS X as follows
  // but this does not properly work, because Travis
  // does not fully support OS X (yet?)
  val OSXTesting = MatrixSet(
    OSX, Language("java"), Env(Map(("SBT_VERSION_CMD", "\"^validate ^validateUniversal\""))),
    XCode83
  )

  TravisConfig(
    Map(
      // before installation, we need to make sure that we have sbt
      // on Mac OS X, this means we need to install it via 'brew install'
      // hopefully this will be provided by Travis CI in the future
      "before_install" -> List("if [[ \"$TRAVIS_OS_NAME\" = \"osx\" ]]; then brew update; brew install sbt; fi"),

      // on the install step, we run 'sbt update' to install all the dependencies
      // if this fails, we will get an errored test, instead of a failed one
      "install" -> sbt("update"),

      // setup the test environment, so that the lmh versioning is ignored on devel
      "before_script" -> List(
        "export TEST_USE_BRANCH=$TRAVIS_BRANCH; echo TEST_USE_BRANCH=;"
      )
    ),

    MatrixSet(
      Scala(ourScalaVersion)
    ) && LinuxTesting, /* (LinuxTesting && OSXTesting) if OS X testing is ever fixed )*/


    TravisStage("SelfCheck", "check that 'sbt genTravisYML' has been run")(
      TravisJob("Check that `sbt genTravisYML` has been run", sbt("genTravisYML", identical(".travis.yml")), MatrixSet(OpenJDK14), expansion = FirstExpansion)
    ),

    TravisStage("CompileAndCheck", "Check that our tests run and the code compiles")(
      TravisJob("Check that unit tests run", sbt("test")),
      TravisJob("Check mmt.jar generation and integration tests",
        sbt("deploy", file("deploy/mmt.jar")) ::: List("java -cp deploy/mmt.jar info.kwarc.mmt.test.TestRunner")),
    ),

    TravisStage("DeployCheck", "check that the 'apidoc' and 'deployLFCatalog' targets work")(
      TravisJob("Check lfcatalog.jar generation using `sbt deployLFCatalog`", sbt("deployLFCatalog", file("deploy/lfcatalog/lfcatalog.jar")))
    //   TravisJob("Check that apidoc generation works", sbt("apidoc", dir("apidoc")))
    )

    // TravisStage("deploy", "deploy the api documentation", Some("branch = master"))(
    //  TravisJob("Auto-deploy API documentation", List("bash scripts/travis/deploy_doc.sh"), MatrixSet(OpenJDK14), expansion = FirstExpansion)
    // )
  )
}


val genTravisYML = taskKey[Unit]("Print out travis.yml configuration")
genTravisYML := {
  // read the prefix and the config
  val prefix = Source.fromFile(utils.value.src / "project" / "prefix.travis.yml").getLines.filter(!_.startsWith("##")).mkString("\n")
  val config = travisConfig.value.serialize

  // and write it into .travis.yml
  val outFile = utils.value.root / ".travis.yml"
  IO.write(outFile, prefix + "\n" + config)
  streams.value.log.info(s"Wrote $outFile")
}
