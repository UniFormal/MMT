package src.main.scala.travis

import src.main.scala.yaml.{YAMLSequence, YAMLString, YAMLStructure}

/**
  * Represents a single stage of Travis Testing
  * @param name the name of the current stage
  * @param description a (human-readable) description of the current stage
  * @param condition an (optional) condition to check when running the stage
  * @param jobs the jobs this stage consists of
  */
case class Stage(name: String, description: String, condition: Option[String] = None)(jobs: Job*) {
  /** expands this stage into a travis map representing the stage */
  def toStageDesc: YAMLStructure = {
    Map(("name", YAMLString.fromString(name))) ++ condition.map(c => Map(("if", YAMLString.fromString(c)))).getOrElse(Map())
  }

  /** expands this job into a concrete list of travis.yml maps representing the job */
  def toJobList(config: Config): YAMLSequence = {
    val jobList : List[MatrixMap] = jobs.flatMap(_.expand(this, config)).toList
    val jobStructList : List[YAMLStructure] = ((jobList.head ++ StageKey(name)) :: jobList.tail).map(_.toStructure)
    YAMLSequence.fromSequence(jobStructList).withComment(description).asInstanceOf[YAMLSequence]
  }
}
