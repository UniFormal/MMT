lazy val commonSettings = Seq(
  organization := "info.kwarc.mmt",
  version := "1.0.1",
  scalaVersion := "2.11.6",
  scalaSource in Compile := baseDirectory.value / "src"
)

lazy val tiscaf = (project in file("tiscaf")).
  settings(commonSettings: _*).
  settings(
    name := "tiscaf"
  )

lazy val mmtApi = (project in file("mmt-api/trunk")).
  dependsOn(tiscaf).
  settings(commonSettings: _*).
  settings(
    name := "mmt-api",
    scalaSource in Compile := baseDirectory.value / "src/main",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.3")
  )

lazy val lfcatalog = (project in file("lfcatalog/trunk")).
  dependsOn(tiscaf).
  settings(commonSettings: _*).
  settings(
    name := "lfcatalog",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  )

lazy val mmtLf = (project in file("mmt-lf")).
  dependsOn(mmtApi, lfcatalog).
  settings(commonSettings: _*).
  settings(
    name := "mmt-lf"
  )

lazy val mmtStex = (project in file("stex-mmt")).
  dependsOn(mmtApi).
  settings(commonSettings: _*).
  settings(
    name := "mmt-stex"
  )

lazy val mmtTptp = (project in file("mmt-tptp")).
  dependsOn(mmtApi, mmtLf).
  settings(commonSettings: _*).
  settings(
    name := "mmt-tptp"
  )

lazy val jedit = (project in file("jEdit-mmt")).
  dependsOn(mmtApi).
  settings(commonSettings: _*).
  settings(
    name := "jEdit-mmt"
  )
