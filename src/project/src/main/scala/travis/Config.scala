package src.main.scala.travis

import src.main.scala.yaml.{YAML, YAMLSequence, YAMLStructure}

/**
  * Represenmts
  * @param globals
  * @param stages
  */
case class Config(globals: MatrixKey[YAML]*)(stages: Stage*) {
  def toYAML: YAMLStructure = {
    val stagesDesc : YAMLSequence = stages.map(_.toStageDesc).map(YAMLSequence.from(_)).foldLeft(YAMLSequence.empty)( _ ++ _)
    Map(
      "stages" -> stagesDesc,
      "jobs" -> YAMLStructure(Map(
        "include" -> stages.map(_.toJobList(this)).foldLeft(YAMLSequence.empty)(_ ++ _)
      ), None)
    )
  }
  def serialize : String = toYAML.serialize
}