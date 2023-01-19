package info.kwarc.mmt.api

/** superclass for all semantic objects, i.e., objects that live in the semantic domain provided Scala */
trait SemanticObject {
  /** the MMT URI of this object, derived from its Scala name: scala://package?name */
  def mpath = SemanticObject.javaToMMT(getClass.getName)

  /** errors in Java initializers are hard to debug; therefore, objects should put initialization code here, which will be called by MMT
   *  empty by default, may throw errors
   */
  def init: Unit = {}
}

object SemanticObject {
  /** converts a java class name of a Scala object into an MMT URI */
  def javaToMMT(cls: String) = {
    val segs = utils.stringToList(cls, "\\.")
    val nJ = segs.last
    var nameS = segs.last
    if (nameS.endsWith("$")) nameS = nameS.init
    val nameSegs = utils.stringToList(nameS, "$")
    DPath(utils.URI("scala", segs.init.reverse.mkString("."))) ? nameSegs
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
