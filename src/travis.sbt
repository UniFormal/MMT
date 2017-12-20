import sbt._
import sbt.Keys._

import src.main.scala.travis._

import scala.io.Source

val travisConfig = taskKey[Config]("Generate travis.yml configuration")
travisConfig := {

  // convenience wrapper to run an sbt task and an optional check
  // we need to hard-code scala 2.10.7 to work on JDK 7/8/9
  def sbt(task: String, check: Option[String] = None) = s"cd src && (cat /dev/null | sbt -Dsbt.scala.version=2.10.7 $task)${check.map(" && cd .. && " +).getOrElse("")}"

  // convenience functions for checks
  def file(name: String) : Option[String] = Some("[[ -f \"" + name + "\" ]]")
  def identical(name: String) : Option[String] = Some("(git diff --quiet --exit-code \"" + name + "\")")
  def dir(name: String) : Option[String] = Some("[[ -d \"" + name + "\" ]]")

  Config(
    Scala(scalaVersion.value),
    JDK(JDKVersion.OpenJDK7),
    JDK(JDKVersion.OpenJDK8), JDK(JDKVersion.OracleJDK8),
    JDK(JDKVersion.OracleJDK9)
  )(
    Stage("build.sbt", "check that build.sbt loads")(
      Job("Check that build.sbt loads", sbt("exit"))()
    ),

    Stage("CodeCheck", "check that the code conforms to standards")(
      Job("Check that the code compiles", sbt("compile"))(),
      Job("Print scalastyle violations", sbt("scalastyle"))()
    ),

    Stage("DeployCheck", "check that the 'apidoc', 'deploy' 'genTravisYML' and 'deployFull' targets work")(
      Job("Check mmt.jar generation using `sbt deploy`", sbt("deploy", file("deploy/mmt.jar")))(),
      Job("Check mmt.jar generation using `sbt deployfull`", sbt("deployFull", file("deploy/mmt.jar")))(),
      Job("Check that apidoc generation works", sbt("apidoc", dir("apidoc")))(),
      Job("Check that `sbt genTravisYML` has been run", sbt("genTravisYML", identical(".travis.yml")))()
    ),

    Stage("test", "check that our own tests work")(
      Job("Run MMT Tests", sbt("test"))()
    ),

    Stage("deploy", "deploy the api documentation", Some("branch = master"))(
      Job("Auto-deploy API documentation", "bash scripts/travis/deploy_doc.sh")(JDK(JDKVersion.OpenJDK8))
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
