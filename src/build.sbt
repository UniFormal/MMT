import Utils.utils
import sbt.Keys.{scalacOptions, _}

import scala.io.Source

utils in ThisBuild := Utils((baseDirectory in src).value)

// If in doubt, always use utils.root or the other File properties on utils to construct
// paths!
// This ensures other SBT projects being able to use MMT as a subproject in a multiproject SBT
// build, see https://github.com/UniFormal/MMT/pull/449.

// =================================
// META-DATA and Versioning
// =================================
version in ThisBuild := {
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


organization in ThisBuild := "info.kwarc.mmt"
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
scalaVersion in Global := "2.12.9"
scalacOptions in Global := Seq(
  "-feature", "-language:postfixOps", "-language:implicitConversions", "-deprecation",
  "-Xmax-classfile-name", "128", // fix long classnames on weird filesystems
  "-sourcepath", baseDirectory.value.getAbsolutePath // make sure that all scaladoc source paths are relative
)

parallelExecution in Global := false
javaOptions in Global ++= Seq("-Xmx2g", "-Xss4096m")

publish := {}
fork in Test := true
testOptions in Test += Tests.Argument("-oI")

// =================================
// DEPLOY TASKS
// =================================

val deploy = TaskKey[Unit]("deploy", "copies packaged jars for MMT projects to deploy location.")
val deployLFCatalog = TaskKey[Unit]("deployLFCatalog", "builds a stand-alone lfcatalog.jar")
val install = TaskKey[Unit]("install", "copies jedit jars to local jedit installation folder.")

// =================================
// DOCUMENTATION TASKS
// =================================
scalacOptions in(ScalaUnidoc, unidoc) ++=
  "-diagrams" +:
    Opts.doc.title("MMT") ++:
    Opts.doc.sourceUrl({
      val repo = System.getenv("TRAVIS_REPO_SLUG")
      s"https://github.com/${if (repo != null) repo else "UniFormal/MMT"}/blob/master/src€{FILE_PATH}.scala"
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
  libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test",
  fork := true,
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
    case
      PathList("rootdoc.txt") | // 2 versions from from scala jars
      PathList("META-INF", _*) => // should never be merged anyway
      MergeStrategy.discard
    // work around Florian's obsession with unmanaged jars
    // otherwise, we wouldn't need this
    case _ => MergeStrategy.first
  },
  // errors for assembly only
  logLevel in assembly := Level.Error
)

/** settings reused by MMT projects */
def mmtProjectsSettings(nameStr: String) = commonSettings(nameStr) ++ Seq(
  scalaSource in Compile := baseDirectory.value / "src",

  scalaSource in Test := baseDirectory.value / "test" / "scala",
  resourceDirectory in Compile := baseDirectory.value / "resources",

  unmanagedBase := baseDirectory.value / "lib",

  publishTo := Some(Resolver.file("file", utils.value.deploy.toJava / " main")),

  install := {},
  deploy := Utils.deployPackage("main/" + nameStr + ".jar").value
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

// the aggregating meta project, used for manual testing
// and building api documentation
lazy val src = (project in file(".")).
  enablePlugins(ScalaUnidocPlugin).
  exclusions(excludedProjects).
  aggregatesAndDepends(
    mmt, api,
    lf, concepts, tptp, owl, mizar, frameit, mathscheme, pvs, metamath, tps, imps, isabelle, odk, specware, stex, mathhub, planetary, interviews, latex, openmath, oeis, repl, got, coq, glf,
    tiscaf, lfcatalog,
    jedit, intellij, argsemcomp
  ).
  settings(
    unidocProjectFilter in(ScalaUnidoc, unidoc) := excludedProjects.toFilter,
    // add the test folder to the test sources
    // but don't actually run any of them
    scalaSource in Test := baseDirectory.value / "test",
    test := {}
  )

// This is the main project. 'mmt/deploy' compiles all relevants subprojects, builds a self-contained jar file, and puts into the deploy folder, from where it can be run.
lazy val mmt = (project in file("mmt")).
  exclusions(excludedProjects).
  dependsOn(tptp, stex, pvs, specware, oeis, odk, jedit, latex, openmath, imps, isabelle, repl, concepts, interviews, mathhub, python, intellij, coq, glf, lsp).
  settings(mmtProjectsSettings("mmt"): _*).
  settings(
    exportJars := false,
    publish := {},
    deploy := Def.taskDyn {
      val jar = (assembly in Compile).value
      val u = utils.value
      Def.task {
        Utils.deployTo(u.deploy / "mmt.jar")(jar)
      }
    }.value,
    assemblyExcludedJars in assembly := {
      val cp = (fullClasspath in assembly).value
      cp filter { j => jeditJars.contains(j.data.getName) }
    },
    mainClass in Compile := Some(mmtMainClass),
    connectInput in run := true,
    mainClass in assembly := Some(mmtMainClass)
  )


// =================================
// MMT Projects: central projects
// =================================

// MMT is split into multiple subprojects to that are managed independently.

def apiJars(u: Utils) = Seq(
  "scala-compiler.jar",
  "scala-library.jar",
  "scala-parser-combinators.jar",
  "scala-xml.jar",
  "xz.jar",
).map(u.lib.toJava / _)

// The kernel upon which everything else depends. Maintainer: Florian
lazy val api = (project in file("mmt-api")).
  settings(mmtProjectsSettings("mmt-api"): _*).
  dependsOn(tiscaf).
  dependsOn(lfcatalog).
  settings(
    scalacOptions in Compile ++= Seq("-language:existentials"),
    scalaSource in Compile := baseDirectory.value / "src" / "main",
    unmanagedJars in Compile ++= apiJars(utils.value),
    unmanagedJars in Test ++= apiJars(utils.value),
  )


// Some foundation-specific extensions. Maintainer: Florian
lazy val lf = (project in file("mmt-lf")).
  dependsOn(api % "compile -> compile; test -> test").
  dependsOn(tiscaf).
  dependsOn(lfcatalog).
  settings(mmtProjectsSettings("mmt-lf"): _*).
  settings(
    //    libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "2.12.8" % "test",
  )

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
    scalaSource in Compile := baseDirectory.value / "src",
    resourceDirectory in Compile := baseDirectory.value / "src/resources",
    unmanagedJars in Compile ++= jeditJars map (baseDirectory.value / "lib" / _),
    deploy := Utils.deployPackage("main/MMTPlugin.jar").value,
    install := utils.value.installJEditJars
  )

// MMT IntelliJ-Plugin. Maintainer: Dennis
lazy val intellij = (project in file("intellij-mmt")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("intellij-mmt"): _*)

lazy val coq = (project in file("mmt-coq")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-coq"): _*)

lazy val lsp = (project in file("mmt-lsp")).
  dependsOn(api,lf).
  settings(mmtProjectsSettings("mmt-lsp"): _*).
  settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "lsp4j.jar").
  settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "jsonrpc.jar").
  settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "gson.jar").
  settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "compat.jar").
  // settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "websocket-api.jar").
  settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "xtext.jar").
  settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "guava.jar")

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

/* using MMT in the editing frontends. Orginally developed by Mihnea (?), functional but presumably obsolete
lazy val webEdit = (project in file("mmt-webEdit")).
  dependsOn(stex).
  settings(mmtProjectsSettings("mmt-webEdit"): _*)
*/

// GLF (Grammatical Framework etc.). Maintainer: Frederik
lazy val glf = (project in file("mmt-glf")).
  dependsOn(api, lf, repl).
  settings(mmtProjectsSettings("mmt-glf"): _*)

// MMT in the interview server. Maintainer: Teresa
lazy val interviews = (project in file("mmt-interviews")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-interviews"): _*)

// using MMT from Python via Py4J, maintainer: Florian
lazy val python = (project in file("python-mmt")).
  dependsOn(api, odk).
  settings(mmtProjectsSettings("python-mmt"): _*).
  settings(unmanagedJars in Compile += baseDirectory.value / "lib" / "py4j0.10.7.jar")

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
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.1.2"
    )
  )

// alignment-based concept browser. Maintainer: Dennis
lazy val concepts = (project in file("concept-browser")).
  dependsOn(api).
  dependsOn(tiscaf).
  dependsOn(lfcatalog).
  settings(mmtProjectsSettings("concept-browser"): _*).
  settings(
    libraryDependencies ++= Seq(
      "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"
    ),
    unmanagedJars in Compile += utils.value.lib.toJava / "scala-xml.jar"
  )

// =================================
// MMT Projects: plugins for working with other languages in MMT
// =================================

// plugin for reading TPTP
lazy val tptp = (project in file("mmt-tptp")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-tptp"): _*).
  settings(
    unmanagedJars in Compile += baseDirectory.value / "lib" / "leo.jar"
  )

// plugin for reading OWL. Originally developed by Füsun
lazy val owl = (project in file("mmt-owl")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-owl"): _*).
  settings(
    unmanagedJars in Compile += baseDirectory.value / "lib" / "owlapi-bin.jar"
  )

// plugin for reading Mizar. Originally developed by Mihnea
lazy val mizar = (project in file("mmt-mizar")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-mizar"): _*)

// Finch is an HTTP server library (https://github.com/finagle/finch), a FrameIT dependency
val finchVersion = "0.32.1"
// Circe is a JSON library (https://circe.github.io/circe/), a FrameIT dependency
val circeVersion = "0.13.0"


lazy val frameit = (project in file("frameit-mmt"))
  .dependsOn(api, lf)
  .settings(mmtProjectsSettings("frameit-mmt"): _*)
  .settings(
    libraryDependencies ++= Seq(
      //  a server infrastructure library
      "com.twitter" %% "twitter-server" % "20.7.0",

      // an incarnation of an HTTP server library for the above infrastructure
      "com.github.finagle" %% "finchx-core" % finchVersion,
      // with ability to automatically encode/decode JSON payloads via the circe library below
      "com.github.finagle" %% "finchx-circe" % finchVersion,
      "com.github.finagle" %% "finchx-generic" % finchVersion,

      // and with testing abilities
      "com.github.finagle" %% "finchx-test" % finchVersion % "test",
      "com.github.finagle" %% "finchx-json-test" % finchVersion % "test",

      "org.scalatest" %% "scalatest" % "3.2.0" % "test",

      // a JSON library
      "io.circe" %% "circe-generic" % circeVersion,
      // with extras to support encoding/decoding a case class hierarchy
      "io.circe" %% "circe-generic-extras" % circeVersion,
      "io.circe" %% "circe-parser"  % circeVersion,
    ),

    scalacOptions in Compile ++= Seq(
      "-Xplugin-require:macroparadise"
    ),

    // in order for @ConfiguredJsonCodec from circe-generic-extras (a FrameIT dependency above) to work
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  )

// plugin for mathscheme-related functionality. Obsolete
lazy val mathscheme = (project in file("mmt-mathscheme")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-mathscheme"): _*)

// plugin for reading PVS. Maintainer: Dennis
lazy val pvs = (project in file("mmt-pvs")).
  dependsOn(api, lf).
  settings(mmtProjectsSettings("mmt-pvs"): _*)

// plugin for reading metamath
lazy val mmscala = RootProject(uri("https://github.com/UniFormal/mm-scala#master"))
lazy val metamath = (project in file("mmt-metamath")).
  dependsOn(api, lf, mmscala).
  settings(mmtProjectsSettings("mmt-metamath"): _*)

// plugin for reading isabelle. Author: Makarius Wenzel
// This only works if an Isabelle environment is present. If not, we use an empty dummy project.
lazy val isabelle_root =
System.getenv().getOrDefault("ISABELLE_ROOT", System.getProperty("isabelle.root", ""))
lazy val isabelle_jars =
  if (isabelle_root == "") Nil else List(file(isabelle_root) / "lib" / "classes" / "Pure.jar")
lazy val isabelle =
  (project in file(if (isabelle_root == "") "mmt-isabelle/dummy" else "mmt-isabelle")).
    dependsOn(api, lf).
    settings(mmtProjectsSettings("mmt-isabelle"): _*).
    settings(unmanagedJars in Compile ++= isabelle_jars)

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

// plugin for reading stex. Originally developed by Mihnea, currently dormant but functional
lazy val stex = (project in file("mmt-stex")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-stex"): _*)

// plugin for writing OpenMath CDs. Maintainer: Florian
lazy val openmath = (project in file("mmt-openmath")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-openmath"): _*)

// plugin for reading the OEIS
lazy val oeis = (project in file("mmt-oeis")).
  dependsOn(planetary).
  settings(mmtProjectsSettings("mmt-oeis"): _*).
  settings(
    unmanagedJars in Compile += utils.value.lib.toJava / "scala-parser-combinators.jar"
  )

// plugin for computing argumentation semantics
lazy val argsemcomp = (project in file("mmt-argsemcomp")).
  dependsOn(api).
  settings(mmtProjectsSettings("mmt-argsemcomp"): _*).
  settings(
    libraryDependencies ++= Seq("com.spotify" % "docker-client" % "latest.integration",
    "org.slf4j" % "slf4j-simple" % "1.7.26", "net.sf.jargsemsat" % "jArgSemSAT" % "0.1.5")
  )

// =================================
// DEPENDENT PROJECTS (projects that are used by mmt-api)
// =================================

// this is a dependency of MMT that is copied into the MMT repository for convenience; it only has to be rebuilt when updated (which rarely happens)
lazy val tiscaf = (project in file("tiscaf")).
  settings(commonSettings("tiscaf"): _*).
  settings(
    scalacOptions in Compile ++= Seq("-language:reflectiveCalls"),
    scalaSource in Compile := baseDirectory.value / "src/main/scala",
    libraryDependencies ++= Seq(
      //      "net.databinder.dispatch" %% "dispatch-core" % "0.11.3" % "test",
      "org.slf4j" % "slf4j-simple" % "1.7.12" % "test",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    ),
    test := {} // disable tests for tiscaf
  )

// this is a dependency of Twelf if used in conjunction with the module system; it is automatically started when using the Twelf importer
lazy val lfcatalog = (project in file("lfcatalog")).
  dependsOn(tiscaf).
  settings(commonSettings("lfcatalog")).
  settings(
    scalaSource in Compile := baseDirectory.value / "src",
    publishTo := Some(Resolver.file("file", utils.value.deploy.toJava / " main")),
    deployLFCatalog := Def.taskDyn {
      val jar = (assembly in Compile).value
      val u = utils.value
      Def.task {
        Utils.deployTo(u.deploy / "lfcatalog" / "lfcatalog.jar")(jar)
      }
    }.value,
    unmanagedJars in Compile += utils.value.lib.toJava / "scala-xml.jar"
  )

// =================================
// experimental projects that are not part of the build file: 
//
// hets-mmt: Aivaras's work for integrating with Hets, owned by DFKI but has become obsolete.
// marpa-mmt:
// mmt-guidedTours: obsolete
// mmt-leo: obsolete
// mmt-lfs: obsolete (has been merged into mmt-lf)
// mmt-reflection: obsolete (but worth keeping until it is superseded by foundations that handle it properly)
// =================================
