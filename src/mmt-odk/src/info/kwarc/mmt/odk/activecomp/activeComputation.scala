package info.kwarc.mmt.odk.activecomp

import info.kwarc.mmt.api.{GlobalName, NamespaceMap, Path}
import info.kwarc.mmt.api.utils.{JSON, JSONObject, JSONString, MMT_TODO}

import scala.util.Try
import scala.xml.Node

/* Represents a single abstract computation*/
sealed abstract class activeComputation(val key : String, val desc : String) {
  /* evaluates a single active Computation for a given context */
  def apply(mathElement : Node, values: ACContext) : ACContext
}

object activeComputation {
  /** a list of all active computations */
  val all : List[activeComputation] = List(Evaluation())

  /** tries to parse an active computation into a string */
  def parse(key : String) : Option[activeComputation] = all.find(_.key == key)
}

object EMEPaths {
  val E : GlobalName = Path.parseS("http://mathhub.info/ODK/ActiveComputationDemo?Energy?Energy", NamespaceMap.empty)
  val m : GlobalName = Path.parseS("http://mathhub.info/ODK/ActiveComputationDemo?Mass?mass", NamespaceMap.empty)
  val c : GlobalName = Path.parseS("http://mathhub.info/ODK/ActiveComputationDemo?Lightspeed?cDef", NamespaceMap.empty)
}

/** An evaluation combination */
@deprecated("MMT_TODO: We should remove this in favor of the Jupyter stuff", since="forever")
case class Evaluation() extends activeComputation("eval", "Evaluate") {
  def apply(mathElement : Node, values: ACContext) : ACContext = {

    // HACK HACK HACK
    // This is hardcoded to the E = mc^2 example for now

    val E : Option[Double] = Try(values.values(EMEPaths.E).toDouble).toOption
    val m : Option[Double] = Try(values.values(EMEPaths.m).toDouble).toOption
    // c is 1

    if(E.isEmpty && m.isEmpty){
      throw new NotEnoughValues()
    } else {
      // since c is 1, e == m
      val result : Double = E.getOrElse(m.get)

      // and return the context
      ACContext(List((EMEPaths.E, result.toString), (EMEPaths.m, result.toString), (EMEPaths.c, "1")).toMap)
    }
  }
}

/* represents a context of an active computation */
case class ACContext(values: Map[GlobalName, String]) {
  /** turns this ACContext into a json object for exporting */
  def toJSON : JSON = JSONObject(values.toList.map(e => (JSONString(e._1.toPath), JSONString(e._2))))
}

object ACContext {
  /* Creates a JSON context from a string */
  def fromJSON (json : JSON) : ACContext = {
    val values = json.asInstanceOf[JSONObject].map.map(e => (Path.parseS(e._1.value, NamespaceMap.empty), e._2.asInstanceOf[JSONString].value)).toMap
    ACContext(values)
  }
}

/** Exception that is thrown when not enough values are available */
class NotEnoughValues extends Exception
