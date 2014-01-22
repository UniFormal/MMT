name := "mmt-api"

version := "1.0"

val deploy = "../../../../deploy/"

scalaSource in Compile := baseDirectory.value / "src/main"

unmanagedJars in Compile ++= {
    val base = baseDirectory.value
    val baseDirectories =  (base / deploy / "lib")
    val customJars = (baseDirectories ** "*.jar") +++ (base / "lib" / "tiscaf.jar")
    customJars.classpath
}
