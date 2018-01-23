package travis.Matrix

import travis.yaml.YAML

/**
  * A [[MatrixSet]] is a tree with each consisting of a list of [[MatrixKey]]s
  */
sealed abstract class MatrixSet {
  /** flatten this MatrixSet into a list of [[MatrixKey]]s */
  def flatten: List[UnexpandedMatrixAssignment]

  /** expands this set into a concrete list of [[MatrixAssignment]]s */
  def expand : List[MatrixAssignment] = flatten.flatMap(_.expand)

  /** adds a given set of globals to this MatrixSet, that is keys which are not set yet, will be added to each of the expanded elements */
  def <<(globals: MatrixSet) : MatrixSet

  /** adds a given set to this MatrixSet, to be expanded in addition to each existing value */
  def &&(set : MatrixSet) : MatrixSet

  /** adds a second option to this MatrixSet, to be expanded in addition to each existing value */
  def ||(set : MatrixSet) : AnyMatrixSet
}

object MatrixSet {
  /** Creates a new MatrixSet given a set of MatrixKeys */
  implicit def apply(keys: MatrixKey[YAML]*) : MatrixSet = ListMatrixSet(keys.toList)
}

/**
  * A [[MatrixSet]] leaf consisting of a list of [[MatrixKey]]s
  * @param lst
  */
case class ListMatrixSet(lst : List[MatrixKey[YAML]]) extends MatrixSet {
  def flatten: List[UnexpandedMatrixAssignment] = List(UnexpandedMatrixAssignment(lst))
  def <<(globals: MatrixSet) : MatrixSet = AnyMatrixSet(
    globals.flatten.map(v => {ListMatrixSet(
      UnexpandedMatrixAssignment(lst).withDefaults(v).values
    )})
  )
  def &&(set : MatrixSet) : MatrixSet = set match {
    case ListMatrixSet(lst2) => ListMatrixSet(lst ::: lst2)
    case AnyMatrixSet(options) => AnyMatrixSet(options.map(this && _))
  }
  def ||(set: MatrixSet) : AnyMatrixSet = AnyMatrixSet(List(this, set))
}

/**
  * A [[MatrixSet]] branch consisting of a list of [[MatrixSet]]s
  * @param options
  */
case class AnyMatrixSet(options: List[MatrixSet]) extends MatrixSet {
  def flatten: List[UnexpandedMatrixAssignment] = options.flatMap(_.flatten)
  def <<(globals: MatrixSet) : MatrixSet = AnyMatrixSet(options.map(_ << globals))
  def &&(set : MatrixSet) : MatrixSet = AnyMatrixSet(options.map(_ && set))
  def ||(set: MatrixSet) : AnyMatrixSet = AnyMatrixSet(options ::: List(set))
}

/** represents a single assignment in the simplified matrix set */
case class UnexpandedMatrixAssignment(values: List[MatrixKey[YAML]]) {
  /** checks if this assignment contains a given name */
  def contains(name : String) : Boolean = values.exists(_.name == name)

  /** checks if this assignment contains a given matrix assignment */
  def withDefaults(assignment: UnexpandedMatrixAssignment) : UnexpandedMatrixAssignment = UnexpandedMatrixAssignment(
    assignment.values.filterNot(f => contains(f.name)) ::: values
  )

  /** expands this into a list of concrete list of matrix assignments */
  def expand: List[MatrixAssignment] = {
    // partition into keys that should and should not be expanded
    val (matrixValues, staticValues) = values.partition(_.expand)

    val nonExpanded = MatrixAssignment(staticValues)

    val matrixArgs = matrixValues.groupBy(_.name).values.filter(_.nonEmpty).toList.sortBy(_.head.name)
    val expanded : List[List[MatrixKey[YAML]]] = cartesianProduct(matrixArgs)

    if(matrixArgs.nonEmpty) (if(matrixArgs.lengthCompare(1) == 0) expanded.transpose else expanded).map(MatrixAssignment(_) ++ nonExpanded) else List(nonExpanded)
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