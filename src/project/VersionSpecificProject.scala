import sbt._


case class VersionSpecificProject(project: Project, excludes: Exclusions) {
  def exclusions(newExclusions : Exclusions) : VersionSpecificProject = VersionSpecificProject(project, newExclusions)

  def aggregate(projects: Project*) : Project = {
    def toProjectReference(p : Project) : ProjectReference = p
    project
      .aggregate(excludes(projects.toList).map(toProjectReference(_)) :_*)
  }

  def dependsOn(projects: Project*) : Project = {
    def toClassPathDep(p: Project) : ClasspathDep[ProjectReference] = p
    project.dependsOn(excludes(projects.toList).map(toClassPathDep(_)) :_*)
  }
}

object VersionSpecificProject {
  implicit def fromProject(project: Project) : VersionSpecificProject = VersionSpecificProject(project, Exclusions())
  implicit def toProject(vProject: VersionSpecificProject): Project = vProject.project
}

case class Exclusions(lst: Project*) {
  private def javaVersion(versions: List[String], exclusions: List[Project]) : Exclusions = {
    val theVersion = System.getProperty("java.version")
    if(versions.exists(v => theVersion.startsWith(s"$v."))){:::(exclusions)} else this
  }
  def java7(exclusions: Project*): Exclusions = javaVersion(List("1.7", "7"), exclusions.toList)
  def java8(exclusions: Project*): Exclusions = javaVersion(List("1.8", "8"), exclusions.toList)
  def java9(exclusions: Project*): Exclusions = javaVersion(List("1.9", "9"), exclusions.toList)

  def :::(lst2: List[Project]) = Exclusions(lst.toList ::: lst2 : _*)
  def :::(other: Exclusions) = Exclusions(lst.toList ::: other.lst.toList :_*)

  def toFilter : ScopeFilter.ProjectFilter = {
    def toProjectReference(p : Project) : ProjectReference = p
    inAnyProject -- inProjects(lst.map(toProjectReference) :_*)
  }

  def excludes(project: Project) : Boolean = lst.contains(project)
  def apply(projects: List[Project]) : List[Project] = projects.filterNot(this.excludes)
}