package info.kwarc.mmt.sql.codegen

case class ProjectPaths(outputRootDir: String, relBackendPackage: String, relFrontend: String, relDbPackage: String) {
  def backendPackagePath = s"$outputRootDir/$relBackendPackage"
  def frontendPath = s"$outputRootDir/$relFrontend"
  def dbPackagePath = s"$backendPackagePath/$relDbPackage"
}
