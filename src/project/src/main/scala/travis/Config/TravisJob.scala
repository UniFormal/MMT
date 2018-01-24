package travis.Config

import travis.Matrix._
import travis.yaml.{YAMLSequence, YAMLString}

/**
  * A single Job being run by travis
  *
  * @param description A short, human-readable, description of the job
  * @param script      A script to executed when running the job
  * @param keySet      a set of keys that will be expanded in the build matrix
  * @param expansion   How to handle expansion of defaults
  */
case class TravisJob(description: String, script: List[String], keySet: MatrixSet = MatrixSet(), expansion: ExpansionHandling = DefaultExpansionHandling) {
  private val descriptionKey = DescriptionKey(description)
  private val scriptKey = ScriptKey(script)

  /** expands this job into a concrete list [[]] assignments */
  def expand(stage: TravisStage, config: TravisConfig): List[MatrixAssignment] = {
    expansion((keySet << config.globals).expand).map(ma => {
      ma
        .%(descriptionKey, { ys: YAMLSequence => descriptionKey.value ++ ys })
        .%(scriptKey, { ys: YAMLSequence => scriptKey.value })
    })
  }
}

/** Describes how to handle expansion of defaults */
sealed abstract class ExpansionHandling {
  def apply(list: List[MatrixAssignment]): List[MatrixAssignment]
}

/** use all default cases */
case object DefaultExpansionHandling extends ExpansionHandling {
  def apply(list: List[MatrixAssignment]): List[MatrixAssignment] = list
}

/** use only the first default case */
case object FirstExpansion extends ExpansionHandling {
  def apply(list: List[MatrixAssignment]): List[MatrixAssignment] = List(list.head)
}

/** use only the last default case */
case object LastExpansion extends ExpansionHandling {
  def apply(list: List[MatrixAssignment]): List[MatrixAssignment] = List(list.last)
}
