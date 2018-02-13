package travis.Matrix

import travis.yaml._

/**
  * A concrete assignment of [[MatrixKey]]s
  * @param nameMap
  */
case class MatrixAssignment(nameMap: Map[String, MatrixKey[YAML]]){
  implicit def toMap: Map[String, YAML] = nameMap.toList.map(_._2.toPair).toMap

  def ++(ass: MatrixAssignment) = MatrixAssignment(nameMap ++ ass.nameMap)
  def ++(key: MatrixKey[YAML]) = MatrixAssignment(nameMap ++ Map((key.name, key)))

  /** Adds a new value of the given value to this [[MatrixAssignment]]
    *
    * @param name Name of value to mixin
    * @param f function that mixes in the new values
    * @param default the default value to be added if no values exists
    * @return
    */
  def %[T <: YAML](name: String, f: T => YAML, default: => MatrixKey[YAML]): MatrixAssignment = {
    // run the map of the given string
    var newMap : Map[String, MatrixKey[YAML]] = nameMap.mapValues(mk => {
      if(mk.name == name){
        try {
          mk.value[YAML](f(mk.value.asInstanceOf[T]))
        } catch {
          case _: java.lang.ClassCastException => mk
        }
      } else { mk }
    })

    // if there was no name in the newMap, we add the defaults
    if(!newMap.contains(name)){
      newMap = newMap ++ Map((name, default))
    }

    // and return the new MatrixAssignment
    MatrixAssignment(newMap)
  }

  /** Mixes ina  new MatrixKey */
  def %[T <: YAML](key: MatrixKey[T], f: T => YAML) : MatrixAssignment = %[T](key.name, f, key)

  lazy val toStructure: YAMLStructure = toMap
}

object MatrixAssignment {
  def apply(nameList: List[MatrixKey[YAML]]) = new MatrixAssignment(
    nameList.groupBy(_.name).mapValues({
      case List(h) => h
      case _ => throw new Exception("nameList should contain exactly one MatrixKey per _.name")
    })
  )
}
