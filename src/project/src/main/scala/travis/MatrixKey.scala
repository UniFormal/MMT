package src.main.scala.travis

import src.main.scala.yaml._

/**
  * Represents a single configurable Travis CI Setting that can have multiple values to be expanded
  *
  * @param name name of the setting to be expanded
  * @param value value to be stored in the YAML file
  * @param expand Boolean indicating if this value should be expanded or not
  * @tparam T [[YAML]] type of the value to be stored
  */
sealed abstract class MatrixKey[+T <: YAML](val name : String, val value: T, val expand: Boolean = true) {
  /** returns a new [[MatrixKey]] of the same type that will never be expanded */
  def static: MatrixKey[T] = new MatrixKey[T](name, value, false){}
}

object MatrixKey {
  def includeGlobals(locals: List[MatrixKey[YAML]], globals: List[MatrixKey[YAML]]) : List[MatrixKey[YAML]] = {
    globals.filterNot(gk => locals.exists(_.name == gk.name)) ::: locals
  }
}

/** a set of environment variables to be set */
case class Env(env: Map[String, String]) extends MatrixKey[YAMLStructure]("env", env.mapValues(YAMLString.fromString))

/** the versions of the JDK being used */
case class JDK(version: JDKVersion.Value) extends MatrixKey[YAMLString]("jdk", version.toString)
object JDKVersion extends Enumeration {
  val IBMJava8 : Value = Value("ibmjava8")

  val OpenJDK6 : Value = Value("openjdk6")
  val OpenJDK7 : Value = Value("openjdk7")
  val OpenJDK8 : Value = Value("openjdk8")

  val OracleJDK8 : Value = Value("oraclejdk8")
  val OracleJDK9 : Value = Value("oraclejdk9")
}

/** the versions of Scala being used */
case class Scala[YAMLString](version: String) extends MatrixKey("scala", YAMLString.fromString(version))

/** internal matrix keys */
sealed abstract class InternalMatrixKey[+T <: YAML](override val name: String, override val value: T) extends MatrixKey[T](name, value, false)
private[travis] case class StageKey(stage : String) extends InternalMatrixKey[YAMLString]("stage", stage)
private[travis] case class ScriptKey(script : List[String]) extends InternalMatrixKey[YAMLSequence]("script", script.map(YAMLString.fromString))
private[travis] case class DescriptionKey(info : String) extends InternalMatrixKey[YAMLSequence]("env", YAMLSequence.from(YAMLString.fromString("INFO='" + info + "'")))
