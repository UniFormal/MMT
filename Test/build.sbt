import sbt.Keys.libraryDependencies

lazy val mmt = RootProject(file("C:/mmt2/MMT/src"))

lazy val playground = Project(id = "playground", base = file(".")).settings(
  name := "playground",
  version := "0.1",
  scalaVersion := "2.12.8",
  scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation"),
  // Add further desired libraryDependencies here
  // e.g. libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.3.0",
).dependsOn(mmt).aggregate(mmt)