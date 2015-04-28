def commonSettings(nameStr: String) = Seq(
  organization := "info.kwarc.mmt",
  version := "1.0.1",
  scalaVersion := "2.11.6",
  name := nameStr,
  sourcesInBase := false,
  scalaSource in Compile := baseDirectory.value / "src",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  mainClass in (Compile, run) := Some("info.kwarc.mmt.api.frontend.Run")
)

lazy val tiscaf = (project in file("tiscaf")).
  settings(commonSettings("tiscaf"): _*)

lazy val mmtApi = (project in file("mmt-api/trunk")).
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
  settings(commonSettings("lfcatalog"): _*).
  settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  )

lazy val mmtLf = (project in file("mmt-lf")).
  dependsOn(mmtApi, lfcatalog).
  settings(commonSettings("mmt-lf"): _*)

lazy val mmtStex = (project in file("stex-mmt")).
  dependsOn(mmtApi).
  settings(commonSettings("mmt-stex"): _*)

lazy val mmtTptp = (project in file("mmt-tptp")).
  dependsOn(mmtApi, mmtLf).
  settings(commonSettings("mmt-tptp"): _*)

lazy val mmt = (project in file(".")).
  dependsOn(mmtTptp, mmtStex).
  settings(commonSettings("mmt"): _*)

lazy val jedit = (project in file("jEdit-mmt")).
  dependsOn(mmtApi).
  settings(commonSettings("jEdit-mmt"): _*)
