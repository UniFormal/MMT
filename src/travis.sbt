import sbt._
import sbt.Keys._
import src.main.scala.travis.{Job, _}

import scala.io.Source

val travisConfig = taskKey[Config]("Generate travis.yml configuration")
travisConfig := {

  // convenience wrapper to run an sbt task and an optional check
  // we need to hard-code scala 2.10.7 to work on JDK 7/8/9
  def sbt(task: String, check: Option[String] = None) : List[String] = List(
    s"cd src && (cat /dev/null | sbt -Dsbt.scala.version=2.10.7 $task) && cd .."
  ) ::: check.toList

  // convenience functions for checks
  def file(name: String) : Option[String] = Some("[[ -f \"" + name + "\" ]]")
  def identical(name: String) : Option[String] = Some("(git diff --quiet --exit-code \"" + name + "\")")
  def dir(name: String) : Option[String] = Some("[[ -d \"" + name + "\" ]]")

  Config(
    Map(
      // on the install step, we run 'sbt update' to install all the dependencies.
      // if this fails, we will get an errored test, instead of a failed one.
      "install" -> sbt("update"),

      // setup the test environment, so that the lmh versioning is ignored on devel.
      "before_script" -> List("if [ \"$TRAVIS_BRANCH\" == \"devel\" ]; then export TEST_USE_ARCHIVE_HEAD=1; fi")
    ),
    Scala(scalaVersion.value),
    JDK(JDKVersion.OpenJDK7),
    JDK(JDKVersion.OpenJDK8), JDK(JDKVersion.OracleJDK8),
    JDK(JDKVersion.OracleJDK9)
  )(

    Stage("SelfCheck", "check that 'sbt genTravisYML' has been run")(
      Job("Check that `sbt genTravisYML` has been run", sbt("genTravisYML", identical(".travis.yml")))(JDK(JDKVersion.OpenJDK8))
    ),

    Stage("CompileCheck", "check that the code complies and conforms to standarssa")(
      Job("Check that the code compiles", sbt("scalastyle"), sbt("compile"))()
    ),

    Stage("DeployCheck", "check that the 'apidoc', 'deploy' and 'deployFull' targets work")(
      Job("Check mmt.jar generation using `sbt deploy`", sbt("deploy", file("deploy/mmt.jar")))(),
      Job("Check mmt.jar generation using `sbt deployfull`", sbt("deployFull", file("deploy/mmt.jar")))(),
      Job("Check that apidoc generation works", sbt("apidoc", dir("apidoc")))()
    ),

    Stage("test", "check that our own tests run")(
      Job("Run MMT Tests", sbt("test"))()
    ),

    Stage("deploy", "deploy the api documentation", Some("branch = master"))(
      Job("Auto-deploy API documentation", List("bash scripts/travis/deploy_doc.sh"))(JDK(JDKVersion.OpenJDK8))
    )
  )
}


val genTravisYML = taskKey[Unit]("Print out travis.yml configuration")
genTravisYML := {
  // read the prefix and the config
  val prefix = Source.fromFile(Utils.src / "project" / "prefix.travis.yml").getLines.filter(!_.startsWith("##")).mkString("\n")
  val config = travisConfig.value.serialize

  // and write it into .travis.yml
  val outFile = Utils.root / ".travis.yml"
  IO.write(outFile, prefix + "\n" + config)
  streams.value.log.info(s"Wrote $outFile")
}
