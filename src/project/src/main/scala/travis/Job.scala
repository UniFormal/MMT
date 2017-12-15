package src.main.scala.travis

import src.main.scala.yaml.{YAML, YAMLSequence, YAMLString, YAMLStructure}

/**
  * A single Job being run by travis
  *
  * @param description A short, human-readable, description of the job
  * @param script A script to executed when running the job
  * @param keys a set of keys that will be expanded in the build matrix
  */
case class Job(description: String, script: String)(keys: MatrixKey[YAML]*) {
  lazy val defaults : MatrixMap = MatrixMap(
    ScriptKey(script),
    DescriptionKey(description)
  )

  /** expands this job into a concrete list [[MatrixMap]] assignments */
  def expand(stage: Stage, config: Config): List[MatrixMap] = {
    val withGlobals = MatrixKey.includeGlobals(keys.toList, config.globals.toList)
    defaults ++ MatrixMap.expand(withGlobals)
  }
}