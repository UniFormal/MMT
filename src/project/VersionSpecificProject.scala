import sbt._

/**
  * Represents a project which excludes a given list of projects from aggregation and dependency
  * @param project Project begin wrapped
  * @param excludes Object representing exclusions
  */
case class VersionSpecificProject(project: Project, excludes: Exclusions) {
  /** sets an [[Exclusions]] object to exclude specific projects */
  def exclusions(newExclusions : Exclusions) : VersionSpecificProject = VersionSpecificProject(project, newExclusions)

  /** aggregates a list of projects in this project, excluding the configured [[Exclusions]]s */
  def aggregate(projects: ProjectReference*) : Project = {
    project
      .aggregate(excludes(projects.toList) :_*)
  }

  /** marks this project as depending on a given set of projects */
  def dependsOn(projects: ProjectReference*) : Project = {
    def toClassPathDep(p: ProjectReference) : ClasspathDep[ProjectReference] = p
    project.dependsOn(excludes(projects.toList).map(toClassPathDep(_)) :_*)
  }
}

object VersionSpecificProject {
  implicit def fromProject(project: Project) : VersionSpecificProject = VersionSpecificProject(project, Exclusions())
  implicit def toProject(vProject: VersionSpecificProject): Project = vProject.project
}

case class Exclusions(lst: ProjectReference*) {
  private def javaVersion(versions: List[String], exclusions: List[ProjectReference]) : Exclusions = {
    val theVersion = System.getProperty("java.version")
    if(versions.exists(v => theVersion.startsWith(s"$v."))){:::(exclusions)} else this
  }
  def java7(exclusions: ProjectReference*): Exclusions = javaVersion(List("1.7", "7"), exclusions.toList)
  def java8(exclusions: ProjectReference*): Exclusions = javaVersion(List("1.8", "8"), exclusions.toList)
  def java9(exclusions: ProjectReference*): Exclusions = javaVersion(List("1.9", "9"), exclusions.toList)

  def :::(lst2: List[ProjectReference]) = Exclusions(lst.toList ::: lst2 : _*)
  def :::(other: Exclusions) = Exclusions(lst.toList ::: other.lst.toList :_*)

  def toFilter : ScopeFilter.ProjectFilter = {
    inAnyProject -- inProjects(lst :_*)
  }

  /** check if two references are equal */
  private def equals(left: ProjectReference, right: ProjectReference) : Boolean = {
    // either they are equal directly
    (left == right) || {
      val leftUri = Reference.uri(left)
      val rightUri = Reference.uri(right)
      // or their valid URIs are equal
      leftUri.isDefined && rightUri.isDefined && leftUri.get == rightUri.get
    }
  }

  def excludes(project: ProjectReference) : Boolean = lst.exists(equals(_, project))
  def apply(projects: List[ProjectReference]) : List[ProjectReference] = projects.filterNot(this.excludes)

  def map[B](f: ProjectReference => B) : Seq[B] = lst.map(f)
  def foreach[U](f: ProjectReference => U) : Exclusions = {lst.foreach[U](f); this }
}
