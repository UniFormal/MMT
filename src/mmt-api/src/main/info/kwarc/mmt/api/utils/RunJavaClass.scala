package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.utils.MMTSystem.IsFat

object RunJavaClass {
  /**
    * Runs a java class (i.e. one with a main method) inside a new java process
    * Inherits the classpath from the current J
    */
  def apply(clz: String, cargs: List[String]): ProcessBuilder = {
    // get the classpath, which is either the jar that is provided
    val cp = System.getProperty("java.class.path")

    val args = List(
      System.getProperty("java.home") + java.io.File.separator + "bin" + java.io.File.separator + "java",
      "-cp",
      cp,
      clz,
    ) ::: cargs

    new ProcessBuilder(args:_*)
  }
  /** Same as apply, but without arguments */
  def apply(clz: String): ProcessBuilder = apply(clz, Nil)
}
