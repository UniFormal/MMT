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
    if (!nJ.endsWith("$"))
        throw GeneralError("object name should end in $: " + cls)
    val names = utils.stringToList(segs.last, "$").init
    DPath(utils.URI("scala", segs.init.reverse.mkString("."))) ? names
  }
  
  /** converts an MMT URI into a java class name of a Scala object */
  def mmtToJava(m: MPath) = {
    val auth = m.parent.uri.authority.getOrElse("")
    val nameParts = m.name.map(_.toPath + "$")
    revertDotList(auth) + "." + nameParts
  }
    
  /** reverts a dot-separated list, seen as a string */
  private def revertDotList(s: String) = utils.stringToList(s, "\\.").reverse.mkString(".")
}