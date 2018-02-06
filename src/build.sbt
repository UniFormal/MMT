import scala.io.Source

import sbt.Keys._

import scala.sys.process.Process
import scala.util.Try

// =================================
// META-DATA and Versioning
// =================================
version in ThisBuild := {Source.fromFile("mmt-api/resources/versioning/system.txt").getLines.mkString.trim}
isSnapshot in ThisBuild := Try(Process("git rev-parse HEAD").!!).isFailure

lazy val gitVersion = {
  val gitRef = Try(Process("git rev-parse HEAD").!!).toOption
  val isClean = Try(Process("git status --porcelain").!!).map(_.trim.isEmpty).toOption == Some(true)
  gitRef.getOrElse("") + (if(!isClean) "-localchanges" else "")
}


import java.util.Calendar
import java.text.SimpleDateFormat

packageOptions in (Compile, packageBin) ++= Seq(
  Package.ManifestAttributes("Implementation-Version" -> (version.value + gitVersion)),
  Package.ManifestAttributes("Build-Time" -> new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(Calendar.getInstance)),
  Package.ManifestAttributes("Git-Version" -> gitVersion)
)


organization in ThisBuild := "info.kwarc.mmt"
lazy val mmtMainClass = "info.kwarc.mmt.api.frontend.Run"

// =================================
// GLOBAL SETTINGS
// =================================
scalaVersion in Global := "2.11.12"
scalacOptions in Global := Seq("-feature", "-language:postfixOps", "-language:implicitConversions", "-deprecation", "-Xmax-classfile-name", "128")

parallelExecution in Global := false
javaOptions in Global ++= Seq("-Xmx1g")

publish := {}
fork in Test := true
testOptions in Test += Tests.Argument("-oI")

// =================================
// DEPLOY TASKS
// =================================

val deploy = TaskKey[Unit]("deploy", "copies packaged jars for MMT projects to deploy location.")
val deployFull = TaskKey[Unit]("deployFull", "copies all (including tiscaf and lfcatalog) packaged jars to deploy location.")
val install = TaskKey[Unit]("install", "copies jedit jars to local jedit installation folder.")

// =================================
// DOCUMENTATION TASKS
// =================================
scalacOptions in(ScalaUnidoc, unidoc) ++=
  "-diagrams" +:
    Opts.doc.title("MMT") ++:
    Opts.doc.sourceUrl({
      val repo = System.getenv("TRAVIS_REPO_SLUG")
      s"https://github.com/${if(repo != null) repo else "UniFormal/MMT"}/blob/master€{FILE_PATH}.scala"
    })
target in(ScalaUnidoc, unidoc) := file("../apidoc")

lazy val cleandoc = taskKey[Unit]("remove api documentation.")
cleandoc := Utils.delRecursive(streams.value.log, file("../apidoc"))

lazy val apidoc = taskKey[Unit]("generate post processed api documentation.")
apidoc := Unit
apidoc := apidoc.dependsOn(cleandoc, unidoc in Compile in src).value

// =================================
// SHARED SETTINGS
// =================================

/** settings shared by all projects */
def commonSettings(nameStr: String) = Seq(
  name := nameStr,
  sourcesInBase := false,
  autoAPIMappings := true,
  exportJars := true,
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.4" % "test",
  fork := true,
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
    case
      PathList("rootdoc.txt") | // 2 versions from from scala jars
      PathList("META-INF", _*) => // should never be merged anyway
      MergeStrategy.discard
    // work around weird behavior of default strategy, which renames files for no apparent reason
    case _ => MergeStrategy.singleOrError
  }
)

/** settings reused by MMT projects */
def mmtProjectsSettings(nameStr: String) = commonSettings(nameStr) ++ Seq(
  scalaSource in Compile := baseDirectory.value / "src",

  scalaSource in Test := baseDirectory.value / "test" / "scala",
  resourceDirectory in Compile := baseDirectory.value / "resources",

  unmanagedBase := baseDirectory.value  / "lib",

  publishTo := Some(Resolver.file("file", Utils.deploy.toJava / " main")),

  install := {},
  deploy := Utils.deployPackage("main/" + nameStr + ".jar").value,
  deployFull := Utils.deployPackage("main/" + nameStr + ".jar").value
)

// =================================
// EXCLUDED PROJECTS
// =================================
import VersionSpecificProject._
lazy val excludedProjects = {
  Exclusions()
    .java7(repl, odk)
    .java9(concepts)
}

// =================================
// Main MMT Projects
// =================================
lazy val src = (project in file(".")).
  enablePlugins(ScalaUnidocPlugin).
  exclusions(excludedProjects).
  aggregate(
      mmt, api,
      leo, lf, concepts, tptp, owl, mizar, frameit, mathscheme, pvs, metamath, tps, imps, odk, specware, stex, webEdit, planetary, interviews, latex, openmath, oeis, repl,
      tiscaf, lfcatalog,
      jedit
  ).settings(
    unidocProjectFilter in (ScalaUnidoc, unidoc) := excludedProjects.toFilter
  )


lazy val mmt = (project in file("mmt")).
  exclusions(excludedProjects).
  dependsOn(tptp, stex, pvs, specware, webEdit, oeis, odk, jedit, latex, openmath, imps, repl, concepts, interviews).
  settings(mmtProjectsSettings("mmt"): _*).
  settings(
    exportJars := false,
    publish := {},
    deploy := {
      assembly in Compile map Utils.deployTo(Utils.deploy / "mmt.jar")
    }.value,
    deployFull := deployFull.dependsOn(deploy).value,
    assemblyExcludedJars in assembly := {
      val cp = (fullClasspath in assembly).value
      cp filter { j => jeditJars.contains(j.data.getName) }
    },
    mainClass in Compile := Some(mmtMainClass),
    connectInput in run := true,
    mainClass in assembly := Some(mmtMainClass)
  )


// =================================
// MMT Projects
// =================================

lazy val api = (project in file("mmt-api")).
  settings(mmtProjectsSettings("mmt-api"): _*).
  settings(
    scalacOptions in Compile ++= Seq("-language:existentials"),
    scalaSource in Compile := baseDirectory.value / "src" / "main",
    unmanagedJars in Compile += Utils.lib.toJava / "tiscaf.jar",
    unmanagedJars in Compile += Utils.lib.toJava / "scala-compiler.jar",
    unmanagedJars in Compile += Utils.lib.toJava / "scala-reflect.jar",
    unmanagedJars in Compile += Utils.lib.toJava / "scala-parser-combinators.jar",
    unmanagedJars in Compile += Utils.lib.toJava / "scala-xml.jar",
    unmanagedJars in Test += Utils.lib.toJava / "tiscaf.jar",
    unmanagedJars in Test += Utils.lib.toJava / "scala-compiler.jar",
    unmanagedJars in Test += Utils.lib.toJava / "scala-reflect.jar",
    unmanagedJars in Test += Utils.lib.toJava / "scala-parser-combinators.jar",
    unmanagedJars in Test += Utils.lib.toJava / "scala-xml.jar",
    libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4" % "test",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "test",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"
  )


lazy val lf = (project in file("mmt-lf")).
  dependsOn(api % "compile -> compile; test -> test").
  settings(mmtProjectsSettings("mmt-lf"): _*).
  settings(
    unmanagedJars in Compile += Utils.deploy.toJava / "lfcatalog" / "lfcatalog.jar",
    libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4" % "test",
    unmanagedJars in Test += Utils.lib.toJava / "tiscaf.jar"
  )

lazy val leo = (project in file("mmt-leo")).
  dependsOn(lf, api).
  settings(mmtProjectsSettings("mmt-leo"): _*).
  settings(
    libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.4"
  )

lazy val concepts = (project in file("concept-browser")).
  dependsOn(api).
  settings(mmtProjectsSettings("concept-browser"): _*).
  settings(
    libraryDependencies ++= Seq(
      "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"
    ),
    unmanagedJars in Compile += Utils.lib.toJava / "tiscaf.jar",
    unmanagedJars in Compile += Utils.lib.toJava / "scala-xml.jar"
  )

lazy val tptp = (project in file("mmt-tptp")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-tptp"): _*).
  settings(
    unmanagedJars in Compile += baseDirectory.value / "lib" / "leo.jar"
  )

lazy val owl = (project in file("mmt-owl")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-owl"): _*).
  settings(
    unmanagedJars in Compile += baseDirectory.value / "lib" / "owlapi-bin.jar"
  )

lazy val mizar = (project in file("mmt-mizar")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-mizar"): _*)

lazy val frameit = (project in file("frameit-mmt")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("frameit-mmt"): _*)

lazy val mathscheme = (project in file("mmt-mathscheme")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-mathscheme"): _*)

lazy val pvs = (project in file("mmt-pvs")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-pvs"): _*)

lazy val mmscala = RootProject(uri("https://github.com/UniFormal/mm-scala#master"))
lazy val metamath = (project in file("mmt-metamath")).
  dependsOn(api, lf, mmscala).
  settings(mmtProjectsSettings("mmt-metamath"): _*)

lazy val tps = (project in file("mmt-tps")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-tps"): _*)

lazy val imps = (project in file("mmt-imps")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-imps"): _*)

lazy val odk = (project in file("mmt-odk")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-odk"): _*)

lazy val specware = (project in file("mmt-specware")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-specware"): _*)

lazy val stex = (project in file("mmt-stex")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-stex"): _*)

lazy val webEdit = (project in file("mmt-webEdit")).
  dependsOn(stex).
  settings(mmtProjectsSettings("mmt-webEdit"): _*)

lazy val planetary = (project in file("planetary-mmt")).
  dependsOn(stex).
  settings(mmtProjectsSettings("planetary-mmt"): _*)

lazy val interviews = (project in file("mmt-interviews")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-interviews"): _*)

lazy val latex = (project in file("latex-mmt")).
  dependsOn(stex).
  settings(mmtProjectsSettings("latex-mmt"): _*)

lazy val openmath = (project in file("mmt-openmath")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-openmath"): _*)

lazy val oeis = (project in file("mmt-oeis")).
  dependsOn(planetary).
  settings(mmtProjectsSettings("mmt-oeis"): _*).
  settings(
    unmanagedJars in Compile += Utils.lib.toJava / "scala-parser-combinators.jar"
  )

lazy val repl = (project in file("mmt-repl")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-repl")).
  settings(
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.1.2"
    )
  )

// =================================
// DEPENDENT PROJECTS
// =================================

lazy val tiscaf = (project in file("tiscaf")).
  settings(commonSettings("tiscaf"): _*).
  settings(
    scalacOptions in Compile ++= Seq("-language:reflectiveCalls"),
    scalaSource in Compile := baseDirectory.value / "src/main/scala",
    libraryDependencies ++= Seq(
      "net.databinder.dispatch" %% "dispatch-core" % "0.11.3" % "test",
      "org.slf4j" % "slf4j-simple" % "1.7.12" % "test"
    ),
    deployFull := Utils.deployPackage("lib/tiscaf.jar").value,
    test := {} // disable tests for tiscaf
  )

lazy val lfcatalog = (project in file("lfcatalog")).
  settings(commonSettings("lfcatalog")).
  settings(
    scalaSource in Compile := baseDirectory.value / "src",
    unmanagedJars in Compile += Utils.lib.toJava / "tiscaf.jar",
    unmanagedJars in Compile += Utils.lib.toJava / "scala-xml.jar",
    deployFull := Utils.deployPackage("lfcatalog/lfcatalog.jar").value
  )

// experimental projects that are not part of any tests: marpa-mmt, hets-mmt

// jars to be used in Compile but not in the fat jar
val jeditJars = Seq(
  "Console.jar",
  "ErrorList.jar",
  "Hyperlinks.jar",
  "jedit.jar",
  "SideKick.jar",
  "jsr.jar"
)

lazy val jedit = (project in file("jEdit-mmt")).
  dependsOn(api, lf).
  settings(commonSettings("jEdit-mmt"): _*).
  settings(
    scalaSource in Compile := baseDirectory.value / "src",
    resourceDirectory in Compile := baseDirectory.value / "src/resources",
    unmanagedJars in Compile ++= jeditJars map (baseDirectory.value / "lib" / _),
    deploy := Utils.deployPackage("main/MMTPlugin.jar").value,
    deployFull := Utils.deployPackage("main/MMTPlugin.jar").value,
    install := Utils.installJEditJars
  )
