import sbt._
import Keys._
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.osgi.OsgiKeys

object TiscafProject extends Build {

  val tiscaf = (Project("tiscaf", file(".")) settings (
    organization := "org.gnieh",
    name := "tiscaf",
    version := "0.8-SNAPSHOT",
    scalaVersion in ThisBuild := "2.10.0",
    crossScalaVersions := Seq("2.9.3", "2.10.0"),
    libraryDependencies ++= dependencies,
    resourceDirectories in Compile := List(),
    features)
      settings(osgiSettings: _*)
      settings(
        OsgiKeys.exportPackage := Seq(
          "tiscaf",
          "tiscaf.*"
        ),
        OsgiKeys.additionalHeaders := Map (
          "Bundle-Name" -> "Tiscaf HTTP Server"
        ),
        OsgiKeys.privatePackage := Seq(),
        OsgiKeys.requireBundle <<= scalaVersion apply { v =>
          Seq(
            "scala-library;bundle-version=" + v
          )
        }
      )
      settings (publishSettings : _*))

  def features = scalacOptions <++= scalaVersion map { v => if (v.startsWith("2.10"))
      Seq("-deprecation", "-language:_")
    else
      Seq("-deprecation")
  }

  def dependencies = Seq(
    "org.scalatest" %% "scalatest" % "2.0.M5" % "test" cross CrossVersion.binaryMapped {
      case "2.9.3" => "2.9.0"
      case v => v
    },
    "net.databinder.dispatch" %% "dispatch-core" % "0.9.5" % "test" cross CrossVersion.binaryMapped {
      case "2.9.3" => "2.9.2"
      case "2.10.0" => "2.10"
    }
  )

  def publishSettings : Seq[Setting[_]] = Seq(
    // If we want on maven central, we need to be in maven style.
    publishMavenStyle := true,
    publishArtifact in Test := false,
    // The Nexus repo we're publishing to.
    publishTo <<= version { (v : String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    // Maven central cannot allow other repos. We're ok here because the artifacts we
    // we use externally are *optional* dependencies.
    pomIncludeRepository := { x => false },
    // Maven central wants some extra metadata to keep things 'clean'.
    pomExtra := (
      <licenses>
        <license>
          <name>LGPL</name>
          <url>http://www.gnu.org/licenses/lgpl.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:gnieh/tiscaf.git</url>
        <connection>scm:git:git@github.com:gnieh/tiscaf.git</connection>
      </scm>
      <developers>
        <developer>
          <id>satabin</id>
          <name>Lucas Satabin</name>
        </developer>
        <developer>
          <id>agaydenko</id>
          <name>Andrew Gaydenko</name>
        </developer>
      </developers>))

  /** Example projects */
  lazy val timeserver =
    Project("timeserver", new File("examples/time")) dependsOn(tiscaf)

}
