resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0") // for code style

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3") // for building documentation
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0") // for building a fat jar file

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.2.4") // for developing in eclipse (untested)

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.1")


addSbtPlugin("ch.epfl.scala" % "sbt-scala3-migrate" % "0.4.2")