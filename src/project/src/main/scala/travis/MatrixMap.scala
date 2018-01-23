package src.main.scala.travis

import src.main.scala.yaml.{YAML, YAMLSequence, YAMLStructure}

/** Represents an assignment of [[MatrixKey]]s to actual assignment */
case class MatrixMap(values: List[MatrixKey[YAML]]) {
  lazy val toMap : Map[String, YAML] = values.groupBy(_.name).map(kv=>(kv._1, kv._2.head.value))
  lazy val toStructure: YAMLStructure = toMap

  def ++ (key: MatrixKey[YAML]) : MatrixMap = MatrixMap(key :: values)
  def ++ (map: MatrixMap) = MatrixMap(values ::: map.values)
  def ++ (maps: List[MatrixMap]) : List[MatrixMap] = maps.map(++)
}

object MatrixMap {
  def empty: MatrixMap = MatrixMap(Nil)
  def apply(values: MatrixKey[YAML]*) : MatrixMap = apply(values.toList)

  def toSequence(maps: List[MatrixMap]) : YAMLSequence = maps.map(_.toStructure)

  /** expands a list of [[MatrixKey]]s  into a list of concrete [[MatrixMap]] assignments */
  def expand(values: List[MatrixKey[YAML]]): List[MatrixMap] = {
    // partition into keys that should and should not be expanded
    val (matrixValues, staticValues) = values.partition(_.expand)

    val nonExpanded = MatrixMap(staticValues)

    val matrixArgs = matrixValues.groupBy(_.name).values.filter(_.nonEmpty).toList.sortBy(_.head.name)
    val expanded : List[List[MatrixKey[YAML]]] = cartesianProduct(matrixArgs)

    if(matrixArgs.nonEmpty) (if(matrixArgs.lengthCompare(1) == 0) expanded.transpose else expanded).map(MatrixMap(_) ++ nonExpanded) else List(nonExpanded)
  }

  /**
    * Compute the cartesian product of a list of lists
    * Adapted from https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Scala
    */
  private def cartesianProduct[T](lst: List[List[T]]): List[List[T]] = {

    /**
      * Prepend single element to all lists of list
      * @param e single elemetn
      * @param ll list of list
      * @param a accumulator for tail recursive implementation
      * @return list of lists with prepended element e
      */
    def pel(e: T,
            ll: List[List[T]],
            a: List[List[T]] = Nil): List[List[T]] =
      ll match {
        case Nil => a.reverse
        case x :: xs => pel(e, xs, (e :: x) :: a )
      }

    lst match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: _ =>
        x match {
          case Nil => Nil
          case _ =>
            lst.par.foldRight(List(x))( (l, a) =>
              l.flatMap(pel(_, a))
            ).map(_.dropRight(x.size))
        }
    }
  }
}
