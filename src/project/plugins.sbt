resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0") // for code style

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1") // for building documentation
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6") // for building a fat jar file

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.2.4") // for developing in eclipse (untested)
