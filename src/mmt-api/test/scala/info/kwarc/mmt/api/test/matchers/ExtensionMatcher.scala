package info.kwarc.mmt.api.test.matchers


trait ExtensionMatcher extends MMTMatcher {

  /** the set of extensions to load */
  val extensions: List[String]

  /** check that all extensions are properly loaded */
  def shouldLoadExtensions: Unit = extensions.foreach(addExtension)

  /** check that an extension gets loaded properly */
  def addExtension(name: String): Unit = {
    it should s"extension $name" in {
      controller.extman.addExtension(name, Nil)
    }
  }
}
