import PostProcessApi._
import sbt.Keys._
import sbtunidoc.Plugin.UnidocKeys.unidoc

publish := {}

scalaVersion := "2.11.7"

// = genration of API documentation

// configuration of unidoc, used by our apidoc target

unidocSettings

scalacOptions in(ScalaUnidoc, unidoc) ++=
  "-diagrams" +:
    Opts.doc.title("MMT") ++:
    Opts.doc.sourceUrl("file:/€{FILE_PATH}.scala")

target in(ScalaUnidoc, unidoc) := file("../apidoc")

// our targets

lazy val postProcessApi =
  taskKey[Unit]("post process generated api documentation wrt to source links.")

postProcessApi := postProcess(streams.value.log)

lazy val cleandoc =
  taskKey[Unit]("remove api documentation.")

cleandoc := delRecursive(streams.value.log, file("../apidoc"))

lazy val apidoc =
  taskKey[Unit]("generate post processed api documentation.")

apidoc := postProcessApi.value

apidoc <<= apidoc.dependsOn(cleandoc, unidoc in Compile)

// definition of our custom, project-specific targets

val deploy =
  TaskKey[Unit]("deploy", "copies packaged jars for MMT projects to deploy location.")

val deployFull =
  TaskKey[Unit]("deployFull", "copies all (including tiscaf and lfcatalog) packaged jars to deploy location.")


// settings to be reused by all projects
def commonSettings(nameStr: String) = Seq(
  organization := "info.kwarc.mmt",
  version := "1.0.1",
  scalaVersion := "2.11.7",
  name := nameStr,
  sourcesInBase := false,
  scalaSource in Compile := baseDirectory.value / "src",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  unmanagedBase := baseDirectory.value / "jars",
  isSnapshot := true,
  publishTo := Some(Resolver.file("file", Utils.deploy.toJava/"main")),
  exportJars := true,
  autoAPIMappings := true,
  connectInput in run := true,
  fork := true,
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
    case PathList("rootdoc.txt") => MergeStrategy.discard // 2 versions from from scala jars
	case PathList("META-INF","MANIFEST.MF") => MergeStrategy.discard // should never be merged anyway
    case x => MergeStrategy.singleOrError // work around weird behavior of default strategy, which renames files for no apparent reason
	/*case x => 
	  val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)*/
  }
)
// settings to be reused for MMT projects -- currently includes everything except tiscaf and lfcatalog
def mmtProjectsSettings(nameStr: String) = commonSettings(nameStr) ++ Seq(
  deploy <<= deployPackage("main/" + nameStr + ".jar"),
  deployFull <<= deployPackage("main/" + nameStr + ".jar")
)

// individual projects

lazy val tiscaf = (project in file("tiscaf")).
  settings(commonSettings("tiscaf"): _*).
  settings(
    scalaSource in Compile := baseDirectory.value / "src/main/scala",
    scalaSource in Test := baseDirectory.value / "src/main/scala",
    libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test",
        "net.databinder.dispatch" %% "dispatch-core" % "0.11.3" % "test",
        "org.slf4j" % "slf4j-simple" % "1.7.12" % "test"
    ),
    deployFull <<= deployPackage("lib/tiscaf.jar")
  )

lazy val lfcatalog = (project in file("lfcatalog")).
  settings(commonSettings("lfcatalog") ++ oneJarSettings: _*).
  settings(
    unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "tiscaf.jar",
  	unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "scala-xml.jar",
    deployFull <<= deployPackage("lfcatalog/lfcatalog.jar")
  )

lazy val api = (project in file("mmt-api")).
  settings(mmtProjectsSettings("mmt-api"): _*).
  settings(
    scalaSource in Compile := baseDirectory.value / "src" / "main",
    unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "tiscaf.jar",
    unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "scala-compiler.jar",
    unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "scala-reflect.jar",
    unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "scala-parser-combinators.jar",
    unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "scala-xml.jar"
  )
  

lazy val lf = (project in file("mmt-lf")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-lf"): _*).
  settings(
    unmanagedJars in Compile += Utils.deploy.toJava / "lfcatalog" / "lfcatalog.jar",
    unmanagedJars in Test += Utils.deploy.toJava / "lib" / "scala-parser-combinators.jar",
    unmanagedJars in Test += Utils.deploy.toJava / "lib" / "tiscaf.jar",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"
  )

lazy val leo = (project in file("mmt-leo")).
  dependsOn(lf, api).
  settings(mmtProjectsSettings("mmt-leo"): _*).
  settings(
    scalaSource in Test := baseDirectory.value / "test",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test",
    libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.4"
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

lazy val lfs = (project in file("mmt-lfs")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-lfs"): _*)

lazy val mizar = (project in file("mmt-mizar")).
  dependsOn(api, lf, lfs).
  settings(mmtProjectsSettings("mmt-mizar"): _*)

lazy val frameit = (project in file("frameit-mmt")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("frameit-mmt"): _*)

lazy val pvs = (project in file("mmt-pvs")).
  dependsOn(api, lf, lfx).
  settings(mmtProjectsSettings("mmt-pvs"): _*)

lazy val metamath = (project in file("mmt-metamath")).
  dependsOn(api, lf, SourceDeps.mmScala).
  settings(mmtProjectsSettings("mmt-metamath"): _*)

lazy val lfx = (project in file("mmt-lfx")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-lfx"): _*)

lazy val tps = (project in file("mmt-tps")).
  dependsOn(api,lf).
  settings(mmtProjectsSettings("mmt-tps"): _*)

lazy val odk = (project in file("mmt-odk")).
  dependsOn(api, lf, lfx).
  settings(mmtProjectsSettings("mmt-odk"): _*)

lazy val specware = (project in file("mmt-specware")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-specware"): _*)

lazy val stex = (project in file("mmt-stex")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-stex"): _*)

/*
lazy val guidedTours = (project in file("mmt-guidedTours")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-guidedTours"): _*)
*/

lazy val webEdit = (project in file("mmt-webEdit")).
  dependsOn(stex).
  settings(mmtProjectsSettings("mmt-webEdit"): _*)

lazy val planetary = (project in file("planetary-mmt")).
  dependsOn(stex).
  settings(mmtProjectsSettings("planetary-mmt"): _*)

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
    unmanagedJars in Compile += Utils.deploy.toJava / "lib" / "scala-parser-combinators.jar"
  )

// wrapper project that depends on most other projects
// the deployed jar is stand-alone and can be used as a unix shell script
lazy val mmt = (project in file("fatjar")).
  dependsOn(tptp, stex, pvs, specware, webEdit, oeis, odk, jedit, latex, openmath).
  settings(mmtProjectsSettings("fatjar"): _*).
  settings(
    exportJars := false,
    publish := {},
    deploy <<= assembly in Compile map deployTo("mmt.jar"),
    mainClass in assembly := Some("info.kwarc.mmt.api.frontend.Run"),
    assemblyExcludedJars in assembly := {
      val cp = (fullClasspath in assembly).value
      cp filter {j => jeditJars.contains(j.data.getName)}
    },
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = Some(Seq("#!/bin/bash", """exec /usr/bin/java -Xmx2048m -jar "$0" "$@"""")))
  )

// jars to be used in Compile but not in the fat jar
val jeditJars = Seq(
  "Console.jar",
  "ErrorList.jar",
  "Hyperlinks.jar",
  "jedit.jar",
  "SideKick.jar"
)

val install =
  TaskKey[Unit]("install", "copies jedit jars to local jedit installation folder.")

lazy val jedit = (project in file("jEdit-mmt")).
  dependsOn(api,lf).
  settings(commonSettings("jEdit-mmt"): _*).
  settings(
    resourceDirectory in Compile := baseDirectory.value / "src/resources",
    unmanagedJars in Compile ++= jeditJars map (baseDirectory.value / "lib" / _),
    deploy <<= deployPackage("main/MMTPlugin.jar"),
    deployFull <<= deployPackage("main/MMTPlugin.jar"),
    install := Utils.installJEditJars
  )
