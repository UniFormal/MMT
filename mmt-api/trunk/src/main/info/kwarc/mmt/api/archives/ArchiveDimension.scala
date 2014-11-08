package info.kwarc.mmt.api.archives

sealed abstract class ArchiveDimension(override val toString: String)
case object source extends ArchiveDimension("source")
case object content extends ArchiveDimension("content")
case object narration extends ArchiveDimension("narration")
case object relational extends ArchiveDimension("relational")
case object errors extends ArchiveDimension("errors")

case class Dim(path: String*) extends ArchiveDimension(path.mkString("/")) {
  def /(s: String) = Dim(path :+ s :_*)
}