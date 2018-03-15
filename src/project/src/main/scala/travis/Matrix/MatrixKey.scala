package travis.Matrix

import travis.yaml._

/**
  * Represents a single configurable Travis CI Setting that can have multiple values to be expanded
  *
  * @param name   name of the setting to be expanded
  * @param value  value to be stored in the YAML file
  * @param expand Boolean indicating if this value should be expanded or not
  * @tparam T [[YAML]] type of the value to be stored
  */
sealed abstract class MatrixKey[+T <: YAML](val name: String, val value: T, val expand: Boolean = true) {
  /** the key used in the struct when expanding this [[MatrixKey]] */
  val structKey: String = name

  /** a pair representing the expanded value of this key */
  def toPair: (String, T) = (structKey, value)

  /** returns a new [[MatrixKey]] of the same type that will never be expanded */
  def asStatic: MatrixKey[T] = new MatrixKey[T](name, value, false) {}

  /** returns a new [[MatrixKey]] of the same type, but assigned within a different group */
  def group(group: String): MatrixKey[T] = new MatrixKey[T](group, value, expand) {override val structKey: String = name}

  /** returns a new [[MatrixKey]] with the value v instead of the current value */
  def value[S <: YAML](v : S) : MatrixKey[S] = new MatrixKey[S](name, v, expand = expand){
    override val structKey: String = MatrixKey.this.structKey
  }
}

// Linux
case object Linux extends MatrixKey[YAMLString]("os", "linux")

case object Precise extends MatrixKey[YAMLString]("dist", "precise") // 12.04
case object Trusty extends MatrixKey[YAMLString]("dist", "trusty") // 14.04

case object SudoTrue extends MatrixKey[YAMLBoolean]("sudo", true)
case object SudoFalse extends MatrixKey[YAMLBoolean]("sudo", false)
case object SudoRequired extends MatrixKey[YAMLString]("sudo", "required")


// OS X
case object OSX extends MatrixKey[YAMLString]("os", "osx")
sealed abstract class OSXVersion(version: String) extends MatrixKey[YAMLString]("osx_version", version)
case object XCode92 extends OSXVersion("xcode9.2") // OS X 10.12
case object XCode91 extends OSXVersion("xcode9.1") // OS X 10.12
case object XCode90 extends OSXVersion("xcode9") // OS X 10.12
case object XCode83 extends OSXVersion("xcode8.3") // OS X 10.12
case object XCode80 extends OSXVersion("xcode8") // OS X 10.11
case object XCode73 extends OSXVersion("xcode7.3") // OS X 10.11
case object XCode64 extends OSXVersion("xcode6.4") // OS X 10.10

/** the language being used */
case class Language(language : String) extends MatrixKey[YAMLString]("language", language)

/** a set of environment variables to be set */
case class Env(env: Map[String, String]) extends MatrixKey[YAMLSequence]("env", YAMLSequence.from(env.map(kv => YAMLString.fromString(s"${kv._1}=${kv._2}")).toSeq :_*))

// Languages
/** the versions of the JDK being used */
sealed abstract class JDK(version: String) extends MatrixKey[YAMLString]("jdk", version)
case object IBMJava8 extends MatrixKey[YAMLString]("jdk", "ibmjava8")
case object OpenJDK6 extends MatrixKey[YAMLString]("jdk", "openjdk6")
case object OpenJDK7 extends MatrixKey[YAMLString]("jdk", "openjdk7")
case object OpenJDK8 extends MatrixKey[YAMLString]("jdk", "openjdk8")
case object OracleJDK8 extends MatrixKey[YAMLString]("jdk", "oraclejdk8")
case object OracleJDK9 extends MatrixKey[YAMLString]("jdk", "oraclejdk9")

/** the versions of Scala being used */
case class Scala[YAMLString](version: String) extends MatrixKey("scala", YAMLString.fromString(version))

/** internal matrix keys */
sealed abstract class InternalMatrixKey[+T <: YAML](override val name: String, override val value: T) extends MatrixKey[T](name, value, false)
private[travis] case class StageKey(stage: String) extends InternalMatrixKey[YAMLString]("stage", stage)
private[travis] case class ScriptKey(script: List[String]) extends InternalMatrixKey[YAMLSequence]("script", script.map(YAMLString.fromString))
private[travis] case class DescriptionKey(info: String) extends InternalMatrixKey[YAMLSequence]("env", YAMLSequence.from(YAMLString.fromString("INFO='" + info + "'")))
