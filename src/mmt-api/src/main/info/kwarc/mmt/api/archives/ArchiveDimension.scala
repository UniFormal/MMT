package info.kwarc.mmt.api.archives

/**
 * an identifier for a subfolder of an archive
 * 
 * use "archive / dim" to obtain the location of this folder in an archive
 * 
 * see the package object for predefined dimensions 
 */
abstract class ArchiveDimension

/**
 * a dimension with an archive-specific location
 * 
 * @param key the property that can be used to define the location
 * @param defaultOpt the default location if the property is not set (equal to "key" if absent)  
 */
case class RedirectableDimension(key: String, defaultOpt: Option[ArchiveDimension] = None) extends ArchiveDimension {
   override def toString: String = key
   def default = defaultOpt.getOrElse(Dim(key))
}

/** a dimension with a fixed location */
case class Dim(path: String*) extends ArchiveDimension {
   override def toString = path.mkString("/")
   def /(s: String) = Dim(path :+ s :_*)
}