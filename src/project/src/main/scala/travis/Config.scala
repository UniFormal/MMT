package src.main.scala.travis

import src.main.scala.yaml.{YAML, YAMLSequence, YAMLStructure}

/**
  * Represenmts
  * @param prefix A structure to be added as a prefix to all settings
  * @param globals A set of global matrix keys expanded with each MatrixKey
  * @param stages A set of stages
  */
case class Config(prefix: Map[String, List[String]], globals: MatrixKey[YAML]*)(stages: Stage*) {
  private val yamlPrefix: YAMLStructure = prefix.mapValues(k => ScriptKey(k).value)
  def toYAML: YAMLStructure = {
    val stagesDesc : YAMLSequence = stages.map(_.toStageDesc).map(YAMLSequence.from(_)).foldLeft(YAMLSequence.empty)( _ ++ _)
    Map(
      "stages" -> stagesDesc,
      "jobs" -> YAMLStructure(Map(
        "include" -> stages.map(_.toJobList(this)).foldLeft(YAMLSequence.empty)(_ ++ _)
      ), None)
    ) ++ yamlPrefix
  }
  def serialize : String = toYAML.serialize
}