import com.github.retronym.SbtOneJar
import sbtunidoc.Plugin.UnidocKeys.unidoc
import PostProcessApi._

publish := {}

scalaVersion := "2.11.6"

unidocSettings

scalacOptions in (ScalaUnidoc, unidoc) ++=
  "-diagrams" +:
  Opts.doc.title("MMT") ++:
  Opts.doc.sourceUrl("file:/€{FILE_PATH}.scala")

commands += postProcessApi

target in (ScalaUnidoc, unidoc) := file("../doc/api")

def commonSettings(nameStr: String) = Seq(
  organization := "info.kwarc.mmt",
  version := "1.0.1",
  scalaVersion := "2.11.6",
  name := nameStr,
  sourcesInBase := false,
  scalaSource in Compile := baseDirectory.value / "src",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  unmanagedJars in Compile := Seq.empty,
  isSnapshot := true,
  publishTo := Some(Resolver.file("file", new File("../deploy/main"))),
  mainClass in (Compile, run) := Some("info.kwarc.mmt.api.frontend.Run"),
  exportJars := true,
  autoAPIMappings := true,
  connectInput in run := true,
  fork := true
)

lazy val tiscaf = (project in file("tiscaf")).
  settings(commonSettings("tiscaf"): _*)

lazy val api = (project in file("mmt-api/trunk")).
  dependsOn(tiscaf).
  settings(commonSettings("mmt-api"): _*).
  settings(
    scalaSource in Compile := baseDirectory.value / "src/main",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.3")
  )

lazy val lfcatalog = (project in file("lfcatalog/trunk")).
  dependsOn(tiscaf).
  settings(commonSettings("lfcatalog") ++ SbtOneJar.oneJarSettings: _*).
  settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  )

lazy val lf = (project in file("mmt-lf")).
  dependsOn(api, lfcatalog).
  settings(commonSettings("mmt-lf"): _*)

lazy val stex = (project in file("stex-mmt")).
  dependsOn(api).
  settings(commonSettings("mmt-stex"): _*)

lazy val tptp = (project in file("mmt-tptp")).
  dependsOn(api, lf).
  settings(commonSettings("mmt-tptp"): _*).
  settings(
    unmanagedJars in Compile += baseDirectory.value / "lib" / "tptp-parser.jar",
    libraryDependencies += "antlr" % "antlr" % "2.7.7"
  )

// just a wrapper project
lazy val mmt = (project in file("mmt-exts")).
  dependsOn(tptp, stex).
  settings(commonSettings("mmt-exts"): _*).
  settings(
    exportJars := false,
    publish := {}
  )

lazy val jedit = (project in file("jEdit-mmt")).
  dependsOn(api).
  settings(commonSettings("jEdit-mmt"): _*).
  settings(
    // artifactName := { (_, _, _) => "MMTPlugin.jar" },
    unmanagedJars in Compile ++= Seq(
      "Console.jar",
      "ErrorList.jar",
      "Hyperlinks.jar",
      "jedit.jar",
      "SideKick.jar") map (baseDirectory.value / "lib" /)
  )

lazy val owl = (project in file("mmt-owl")).
  dependsOn(api, lf).
  settings(commonSettings("mmt-owl"): _*).
  settings(
    libraryDependencies += "net.sourceforge.owlapi" % "owlapi-apibinding" % "3.5.2"
  )

lazy val lfs = (project in file("mmt-lfs")).
  dependsOn(api, lf).
  settings(commonSettings("mmt-lfs"): _*)

lazy val mizar = (project in file("mmt-mizar")).
  dependsOn(api, lf, lfs).
  settings(commonSettings("mmt-mizar"): _*)

lazy val frameit = (project in file("frameit-mmt")).
  dependsOn(api, lfcatalog).
  settings(commonSettings("frameit-mmt"): _*)

lazy val pvs = (project in file("mmt-pvs")).
  dependsOn(api).
  settings(commonSettings("mmt-pvs"): _*)

lazy val specware = (project in file("mmt-specware")).
  dependsOn(api).
  settings(commonSettings("mmt-specware"): _*)
