package info.kwarc.mmt.api

/** superclass for all semantic objects, i.e., objects that live in the semantic domain provided Scala */ 
trait SemanticObject {
  lazy val mpath = SemanticObject.javaToMMT(getClass.getCanonicalName)
}

object SemanticObject {
  /** converts a java class name of a Scala object into an MMT URI */
  def javaToMMT(cls: String) = {
    val segs = utils.stringToList(cls, "\\.")
    val nJ = segs.last
    val names = if (!nJ.endsWith("$")) List(segs.last.trim)
    else utils.stringToList(segs.last, "$").init
    /*
    if (!nJ.endsWith("$"))
        throw GeneralError("object name should end in $: " + cls)
    val names = utils.stringToList(segs.last, "$").init */
    DPath(utils.URI("scala", segs.init.reverse.mkString("."))) ? names
  }
  
  /** converts an MMT URI into a java class name of a Scala object */
  def mmtToJava(m: MPath, isClass: Boolean = false) = {
    val uri = m.parent.uri
    val auth = uri.authority.getOrElse("")
    val authParts = utils.stringToList(auth, "\\.").reverse
    val path = uri.path
    val nameParts = m.name.map(_.toPath).mkString("$") + (if (isClass) "" else "$")
    (authParts ::: path ::: List(nameParts)).mkString(".")
  }
}
