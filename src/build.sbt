import Utils.utils
import sbt.Keys.{scalacOptions, _}

import scala.io.Source

ThisBuild / utils := Utils((src / baseDirectory).value)

// If in doubt, always use utils.root or the other File properties on utils to construct
// paths!
// This ensures other SBT projects being able to use MMT as a subproject in a multiproject SBT
// build, see https://github.com/UniFormal/MMT/pull/449.

// =================================
// META-DATA and Versioning
// =================================
ThisBuild / version := {
  Source.fromFile(baseDirectory.value / "mmt-api/resources/versioning/system.txt").getLines.mkString.trim
}

val now = {
  import java.text.SimpleDateFormat
  import java.util.Date
  new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new Date())
}

packageOptions in Global ++= Seq(
  // Specification Version can be any string, and hence includes the build time
  Package.ManifestAttributes("Implementation-Version" -> (version.value + s" (built $now)")),

  // implementation version *has to be* ascii digits seperated by ascii periods
  Package.ManifestAttributes("Specification-Version" -> version.value),

  // custom build-time attribute
  Package.ManifestAttributes("Build-Time" -> now)
)


ThisBuild / organization := "info.kwarc.mmt"
lazy val mmtMainClass = "info.kwarc.mmt.api.frontend.Run"

// =================================
// GLOBAL SETTINGS
// =================================

// !!!WARNING!!!
// If you update scalaVersion, also
//   (1) update apiJars and redownload updated deps
//   (2) verify whether there is a Scala paradise plugin available on Maven central for the new Scala version
//       Search for "paradise" way to below to find the dependency "org.scalamacros" % "paradise_****" in this build.sbt file.
//
Global / scalaVersion := "2.13.5"
Global / scalacOptions := Seq(
  "-feature",
  "-language:postfixOps", "-language:implicitConversions", "-language:reflectiveCalls", "-language:existentials",

  "-deprecation", // turn on deprecation warnings

  // turn down the severity of specific warnings
  "-Wconf:msg=early initializers are deprecated*:i",                 // Info all "early initializers are deprecated" (need to fix in scala3)
  "-Wconf:cat=other-match-analysis:i",                               // Info all "non-exhaustive match" warnings
  "-Wconf:msg=Exhaustivity analysis reached max recursion depth*:s", // Disable "Exhaustivity analysis reached max recursion depth"

  "-Wconf:msg=.*MMT_TODO.*:i",                                        // Info all the MMT_TODOs
  // "-Wconf:msg=.*MMT_TODO.*:s",                                     // to temporarily disable

  // "-Xno-patmat-analysis", // to temporarily disable
  // "-Xmax-classfile-name", "128", // fix long classnames on weird filesystems // does not exist anymore since scala 2.13.*
  "-sourcepath", baseDirectory.value.getAbsolutePath // make sure that all scaladoc source paths are relative
)

Global / parallelExecution := false
Global / javaOptions ++= Seq("-Xmx2g")

publish := {}
Test / fork := true
Test / testOptions  += Tests.Argument("-oI")

// =================================
// DEPENDENCIES
// =================================

def scala_library : Def.SettingsDefinition = libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value
def scala_compiler : Def.SettingsDefinition = libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
def parser_combinators : Def.SettingsDefinition = libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators_2.13" % "1.2.0-M1"
def scala_xml : Def.SettingsDefinition = libraryDependencies +=  "org.scala-lang.modules" %% "scala-xml_2.13" % "2.0.0-M3"
def xz : Def.SettingsDefinition = libraryDependencies +=  "org.tukaani" % "xz" % "1.8"
def parallel_collections : Def.SettingsDefinition = libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections_2.13" % "1.0.0"

def api_deps = {
  Seq(scala_library,scala_compiler,parser_combinators,
    scala_xml,xz,parallel_collections
  )
}

def akka_http = libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-core_2.13" % "10.2.10",
  "com.typesafe.akka" %% "akka-actor-typed_2.13" % "2.7.0-M1"
)
def scalatest : Def.SettingsDefinition = libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
def java8compat : Def.SettingsDefinition = libraryDependencies +=  "org.scala-lang.modules" %% "scala-java8-compat_2.13" % "1.0.2"
def lsp4j = libraryDependencies ++= Seq(
  "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.14.0",
  "org.eclipse.lsp4j" % "org.eclipse.lsp4j.websocket" % "0.14.0",
  "org.eclipse.jetty.websocket" % "javax-websocket-server-impl" % "9.4.46.v20220331",
)
def jgit : Def.SettingsDefinition = libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "6.1.0.202203080745-r"
def slf4j = libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30"
def py4j = libraryDependencies += "net.sf.py4j" % "py4j" % "0.10.7"
def jline = "org.jline" % "jline" % "3.18.0"
def lucene = libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-queryparser" % "9.2.0",
  "org.apache.lucene" % "lucene-grouping" % "9.2.0"
)

// =================================
// DEPLOY TASKS
// =================================

val deploy = TaskKey[Unit]("deploy", "copies packaged jars for MMT projects to deploy location.")
val deployLFCatalog = TaskKey[Unit]("deployLFCatalog", "builds a stand-alone lfcatalog.jar")
val install = TaskKey[Unit]("install", "copies jedit jars to local jedit installation folder.")
val testSetup = TaskKey[Unit]("testSetup", "tests the MMT :setup command")

// =================================
// DOCUMENTATION TASKS
// =================================
ScalaUnidoc / unidoc / scalacOptions  ++=
  "-diagrams" +:
    Opts.doc.title("MMT") ++:
    Opts.doc.sourceUrl({
      val repo = System.getenv("GITHUB_REPOSITORY")
      s"https://github.com/${if (repo != null) repo else "UniFormal/MMT"}/blob/master/src€{FILE_PATH}.scala"
    })
ScalaUnidoc / unidoc / target := file("../apidoc")

lazy val cleandoc = taskKey[Unit]("remove api documentation.")
cleandoc := Utils.delRecursive(streams.value.log, file("../apidoc"))

lazy val apidoc = taskKey[Unit]("generate post processed api documentation.")
apidoc := Unit
apidoc := apidoc.dependsOn(cleandoc, src / Compile / unidoc ).value

// =================================
// SHARED SETTINGS
// =================================

/** settings shared by all projects */
def commonSettings(nameStr: String) = Seq(
  name := nameStr,
  sourcesInBase := false,
  autoAPIMappings := true,
  exportJars := true,
  // libraryDependencies += scalatest,
  fork := true,
  assembly / test := {},
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF","services", _*) => MergeStrategy.first
    case
      PathList("rootdoc.txt") | // 2 versions from from scala jars
      PathList("META-INF", _*) => // should never be merged anyway
      MergeStrategy.discard
    // work around Florian's obsession with unmanaged jars
    // otherwise, we wouldn't need this
    case _ => MergeStrategy.first
  },
  // errors for assembly only
  assembly / logLevel := Level.Error
)

/** settings reused by MMT projects */
def mmtProjectsSettings(nameStr: String) = commonSettings(nameStr) ++ Seq(
  Compile / scalaSource := baseDirectory.value / "src",

  Test / scalaSource := baseDirectory.value / "test" / "scala",
  Compile / resourceDirectory := baseDirectory.value / "resources",

  unmanagedBase := baseDirectory.value / "lib",

  publishTo := Some(Resolver.file("file", utils.value.deploy.toJava / " main")),

  install := {},
  deploy := Utils.deployPackage("main/" + nameStr + ".jar").value,
  testSetup := utils.value.testSetup
)

// =================================
// EXCLUDED PROJECTS
// =================================
import VersionSpecificProject._

lazy val excludedProjects = {
  Exclusions()
    .java7(repl, odk)
}

// =================================
// Main MMT Projects
// =================================

// the aggregating meta project, used for manual testing
// and building api documentation
lazy val src = (project in file(".")).
  enablePlugins(ScalaUnidocPlugin).
  exclusions(excludedProjects).
  aggregatesAndDepends(
    mmt, api,
    lf, owl, mizar, /*frameit,*/ mathscheme, pvs, tps, imps, isabelle, odk, specware, stex, mathhub, latex, openmath, oeis, repl, coq, glf,
    /*tiscaf,*/ lfcatalog,
    jedit, intellij,buildserver
  ).
  settings(
    ScalaUnidoc / unidoc / unidocProjectFilter := excludedProjects.toFilter,
    // add the test folder to the test sources
    // but don't actually run any of them
    Test / scalaSource := baseDirectory.value / "test",
    test := {}
  )

// This is the main project. 'mmt/deploy' compiles all relevants subprojects, builds a self-contained jar file, and puts into the deploy folder, from where it can be run.
lazy val mmt = (project in file("mmt")).
  exclusions(excludedProjects).
  dependsOn(stex, pvs, specware, oeis, odk, jedit, latex, openmath, mizar, imps, isabelle, repl, mathhub, python, intellij, coq, glf, lsp, buildserver).
  settings(mmtProjectsSettings("mmt"): _*).
  settings(
    exportJars := false,
    publish := {},
    deploy := Def.taskDyn {
      val jar = (Compile / assembly).value
      val u = utils.value
      Def.task {
        Utils.deployTo(u.deploy / "mmt.jar")(jar)
      }
    }.value,
    assembly / assemblyExcludedJars := {
      val cp = (assembly / fullClasspath).value
      cp filter { j => jeditJars.contains(j.data.getName) }
    },
    Compile / mainClass := Some(mmtMainClass),
    run / connectInput := true,
    assembly / mainClass := Some(mmtMainClass)
  )


// =================================
// MMT Projects: central projects
// =================================

// MMT is split into multiple subprojects to that are managed independently.



// The kernel upon which everything else depends. Maintainer: Florian
lazy val api = (project in file("mmt-api")).
  settings(mmtProjectsSettings("mmt-api"): _*).
  //dependsOn(tiscaf).
  dependsOn(lfcatalog).
  settings(
    Compile / scalacOptions ++= Seq("-language:existentials"),
    Compile / scalaSource := baseDirectory.value / "src" / "main"
  ).settings(api_deps:_*).settings(akka_http)


// Some foundation-specific extensions. Maintainer: Florian
lazy val lf = (project in file("mmt-lf")).
  dependsOn(api % "compile -> compile; test -> test").
  //dependsOn(tiscaf).
  dependsOn(lfcatalog).
  settings(mmtProjectsSettings("mmt-lf"): _*)

// =================================
// MMT Projects: plugins for using MMT in other applications
// =================================

// jars to be used in Compile but not in the fat jar
val jeditJars = Seq(
  "Console.jar",
  "ErrorList.jar",
  "Hyperlinks.jar",
  "jedit.jar",
  "SideKick.jar",
  "jsr.jar"
)

// using MMT inside jEdit. Maintainer: Florian
lazy val jedit = (project in file("jEdit-mmt")).
  dependsOn(api, lf).
  settings(commonSettings("jEdit-mmt"): _*).
  settings(
    Compile / scalaSource := baseDirectory.value / "src",
    Compile / resourceDirectory := baseDirectory.value / "src/resources",
    Compile / unmanagedJars ++= jeditJars map (baseDirectory.value / "lib" / _),
    deploy := Utils.deployPackage("main/MMTPlugin.jar").value,
    install := utils.value.installJEditJars
  )

// MMT IntelliJ-Plugin. Maintainer: Dennis
lazy val intellij = (project in file("intellij-mmt")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("intellij-mmt"): _*)

// MMT build server. Maintainer: Dennis
lazy val buildserver = (project in file("mmt-buildserver")).
  dependsOn(api, lf, stex).
  settings(mmtProjectsSettings("mmt-buildserver"): _*).
  settings(jgit,slf4j)
/*.
  settings(
    libraryDependencies ++= Seq(
      "io.methvin" %% "directory-watcher-better-files" % "0.15.1"
    )
  )*/

lazy val coq = (project in file("mmt-coq")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-coq"): _*)

lazy val lsp = (project in file("mmt-lsp")).
  dependsOn(api,lf).
  settings(mmtProjectsSettings("mmt-lsp"): _*).
  settings(lsp4j,java8compat)


// using MMT as a part of LaTeX. Maintainer: Florian
lazy val latex = (project in file("latex-mmt")).
  dependsOn(stex).
  settings(mmtProjectsSettings("latex-mmt"): _*)

// using MMT in the OpenDreamKit project, includes language plugins for various systems such as GAP or Sage. Maintainer: Dennis
lazy val odk = (project in file("mmt-odk")).
  dependsOn(api, lf % "compile -> compile; test -> test").
  settings(mmtProjectsSettings("mmt-odk"): _*)

// MMT-Mathhub backend. Maintainer: Tom
lazy val mathhub = (project in file("mathhub-mmt")).
  dependsOn(api).
  settings(mmtProjectsSettings("mathhub-mmt"): _*)

// using MMT in the planetary/MathHub systems. Orginally developed by Mihnea, functional but should be reviewed
lazy val planetary = (project in file("planetary-mmt")).
  dependsOn(stex).
  settings(mmtProjectsSettings("planetary-mmt"): _*)

// GLF (Grammatical Framework etc.). Maintainer: Frederik
lazy val glf = (project in file("mmt-glf")).
  dependsOn(api, lf, repl).
  settings(mmtProjectsSettings("mmt-glf"): _*)

// using MMT from Python via Py4J, maintainer: Florian
lazy val python = (project in file("python-mmt")).
  dependsOn(api, odk).
  settings(mmtProjectsSettings("python-mmt"): _*).
  settings(py4j)

// graph optimization. Maintainer: Michael Banken
lazy val got = (project in file("mmt-got")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-got"): _*)

// =================================
// MMT projects: additional (optional) functionality that is factored out into separate projects due to dependencies
// =================================

// auto-completion in the shell. Maintainer: Tom
lazy val repl = (project in file("mmt-repl")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-repl")).
  settings(
    libraryDependencies ++= Seq(jline)
  )

// =================================
// MMT Projects: plugins for working with other languages in MMT
// =================================

// plugin for reading OWL. Originally developed by Füsun
lazy val owl = (project in file("mmt-owl")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-owl"): _*).
  settings(
    Compile / unmanagedJars += baseDirectory.value / "lib" / "owlapi-bin.jar"
  )

// plugin for reading Mizar. Originally developed by Mihnea
lazy val mizar = (project in file("mmt-mizar")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-mizar"): _*)


// use of MMT in the frameit system, here for ease of deployment but not part of the main mmt target
// reponsible: Navid
// finch is an HTTP server library (https://github.com/finagle/finch), a FrameIT dependency
/*
val finchVersion = "0.32.1"
// Circe is a JSON library (https://circe.github.io/circe/), a FrameIT dependency
val circeVersion = "0.13.0"
lazy val frameit = (project in file("frameit-mmt"))
  .dependsOn(api, lf, odk)
  .settings(mmtProjectsSettings("frameit-mmt"): _*)
  .settings(
    libraryDependencies ++= Seq(
      //  a server infrastructure library
      "com.twitter" %% "twitter-server" % "20.12.0",

      // an incarnation of an HTTP server library for the above infrastructure
      "com.github.finagle" %% "finchx-core" % finchVersion,
      // with ability to automatically encode/decode JSON payloads via the circe library below
      "com.github.finagle" %% "finchx-circe" % finchVersion,
      "com.github.finagle" %% "finchx-generic" % finchVersion,

      // and with testing abilities
      "com.github.finagle" %% "finchx-test" % finchVersion % "test",
      "com.github.finagle" %% "finchx-json-test" % finchVersion % "test",

      "org.scalatest" %% "scalatest" % "3.2.3" % "test",

      // a JSON library
      "io.circe" %% "circe-generic" % circeVersion,
      // with extras to support encoding/decoding a case class hierarchy
      "io.circe" %% "circe-generic-extras" % circeVersion,
      "io.circe" %% "circe-parser"  % circeVersion,
    ),

    Compile / scalacOptions ++= Seq(
     // "-Xplugin-require:macroparadise"
      "-Ymacro-annotations"
    ),

    deploy := Def.taskDyn {
      val jar = (Compile / assembly).value
      Def.task {
        Utils.deployTo(utils.value.deploy / "frameit.jar")(jar)
      }
    }.value,
    Compile / mainClass  := Some("info.kwarc.mmt.frameit.communication.server.Server"),
    assembly / mainClass := Some("info.kwarc.mmt.frameit.communication.server.Server")
  )
*/
	

// plugin for mathscheme-related functionality. Obsolete
lazy val mathscheme = (project in file("mmt-mathscheme")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-mathscheme"): _*)

// plugin for reading PVS. Maintainer: Dennis
lazy val pvs = (project in file("mmt-pvs")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-pvs"): _*)

// plugin for reading metamath
/*lazy val mmscala = RootProject(uri("https://github.com/UniFormal/mm-scala#master"))
lazy val metamath = (project in file("mmt-metamath")).
  dependsOn(api, lf, mmscala).
  settings(mmtProjectsSettings("mmt-metamath"): _*) */

// plugin for reading isabelle. Author: Makarius Wenzel
// This only works if an Isabelle environment is present. If not, we use an empty dummy project.
lazy val isabelle_root =
System.getenv().getOrDefault("ISABELLE_ROOT", System.getProperty("isabelle.root", ""))
lazy val isabelle_jars =
  if (isabelle_root == "") Nil else List(file(isabelle_root) / "lib" / "classes" / "isabelle.jar")
lazy val isabelle =
  (project in file(if (isabelle_root == "") "mmt-isabelle/dummy" else "mmt-isabelle")).
    dependsOn(api, lf).
    settings(mmtProjectsSettings("mmt-isabelle"): _*).
    settings(Compile / unmanagedJars ++= isabelle_jars)

// plugin for reading TPS
lazy val tps = (project in file("mmt-tps")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-tps"): _*)

// plugin for reading IMPS. Maintainer: Jonas
lazy val imps = (project in file("mmt-imps")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-imps"): _*)

// plugin for reading specware. Maintainer: Florian
lazy val specware = (project in file("mmt-specware")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-specware"): _*)

// plugin for reading stex. Maintainer: Dennis
lazy val stex = (project in file("mmt-stex")).
  dependsOn(api,odk,lsp).
  settings(
    mmtProjectsSettings("mmt-stex"),
    jgit,slf4j,lucene,

    /*
    Compile / unmanagedJars += baseDirectory.value / "lib" / "lucene-query-9.2.0.jar",
    Compile / unmanagedJars += baseDirectory.value / "lib" / "lucene-grouping-9.2.0.jar",
    Test / unmanagedJars += baseDirectory.value / "lib" / "lucene-core-9.2.0.jar",
    Test / unmanagedJars += baseDirectory.value / "lib" / "lucene-query-9.2.0.jar",
    Test / unmanagedJars += baseDirectory.value / "lib" / "lucene-queryparser-9.2.0.jar",
    Test / unmanagedJars += baseDirectory.value / "lib" / "lucene-sandbox-9.2.0.jar",
    Test / unmanagedJars += baseDirectory.value / "lib" / "lucene-grouping-9.2.0.jar",

     */

      /*Compile / unmanagedJars += utils.value.lib.toJava / "jgit.jar",
      Compile / unmanagedJars += utils.value.lib.toJava / "slf4j.jar",
      Test / unmanagedJars += utils.value.lib.toJava / "jgit.jar",*/
    /*libraryDependencies ++= Seq(
      "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"
    ),*/
    Compile / unmanagedJars += baseDirectory.value / "lib" / "RusTeX.jar"
  )


// plugin for writing OpenMath CDs. Maintainer: Florian
lazy val openmath = (project in file("mmt-openmath")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-openmath"): _*)

// plugin for reading the OEIS
lazy val oeis = (project in file("mmt-oeis")).
  dependsOn(planetary).
  settings(mmtProjectsSettings("mmt-oeis"): _*).
  settings(parser_combinators)

// =================================
// DEPENDENT PROJECTS (projects that are used by mmt-api)
// =================================

// this is a dependency of MMT that is copied into the MMT repository for convenience; it only has to be rebuilt when updated (which rarely happens)
/*
lazy val tiscaf = (project in file("tiscaf")).
  settings(commonSettings("tiscaf"): _*).
  settings(
    Compile / scalacOptions ++= Seq("-language:reflectiveCalls"),
    Compile / scalaSource := baseDirectory.value / "src/main/scala",
    scala_compiler,
    test := {} // disable tests for tiscaf
  )
 */

// this is a dependency of Twelf if used in conjunction with the module system; it is automatically started when using the Twelf importer

lazy val lfcatalog = (project in file("lfcatalog")).
  //dependsOn(tiscaf).
  settings(commonSettings("lfcatalog")).
  settings(
    Compile / scalaSource := baseDirectory.value / "src",
    publishTo := Some(Resolver.file("file", utils.value.deploy.toJava / " main")),
    deployLFCatalog := Def.taskDyn {
      val jar = (Compile / assembly).value
      val u = utils.value
      Def.task {
        Utils.deployTo(u.deploy / "lfcatalog" / "lfcatalog.jar")(jar)
      }
    }.value,
    scala_xml
  )


// =================================
// deleted projects that are still accessible in the history
// deleted from the devel branch 2022-03-23
//  mmt-leo, mmt-got, mmt-tptp, mmt-interviews, planetary-mmt
//  mmt-webEdit: using MMT in editing frontends, orginally developed by Mihnea (?), functional but obsolete
//  mmt-argsemcomp: argumentation semantics by Max
//
// deleted some other time
// mmt-reflection
//
// deleted from the devel branch 2021-11-17
// hets-mmt: Aivaras's work for integrating with Hets, owned by DFKI but has become obsolete.
// marpa-mmt
// mmt-guidedTours
// mmt-lfs: merged into mmt-lf
// =================================
