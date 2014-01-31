name := "mmt-api"

version := "1.0"

val deploy = "../../../../deploy/"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

scalaSource in Compile := baseDirectory.value / "src/main"

unmanagedJars in Compile ++= {
    val base = baseDirectory.value
    val baseDirectories =  (base / deploy / "lib")
    val customJars = (baseDirectories ** "*.jar") +++ (base / "lib" / "tiscaf.jar")
    customJars.classpath
}
