package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._

class RelationalManager(controller : Controller) {
  def extractors = controller.extman.get(classOf[RelationalExtractor])
  def allUnary : Set[Unary] = extractors.flatMap(_.allUnary).toSet
  def allBinary : Set[Binary] = extractors.flatMap(_.allBinary).toSet


  def parse(s: String, nsMap: NamespaceMap) : RelationalElement = {
    s.split(" ").toList match {
      case List(tp, ind) => Individual(Path.parse(ind.replace("%20"," "), nsMap), parseUnary(tp))
      case List(rel, subj, obj) =>
        Relation(parseBinary(rel), Path.parse(subj.replace("%20"," "), nsMap), Path.parse(obj.replace("%20"," "), nsMap))
      case _ => throw ParseError("not a valid relational element: " + s)
    }
  }
  def parseBinary(s: String) : Binary = allBinary.find(_.toString == s) match {
      case Some(i) => i
      case _ => throw ParseError("binary predicate expected, found: " + s)
   }

  def parseUnary(s : String) : Unary = s match {
      case s if s.startsWith("constant:") => IsConstant //TODO remove
      case s => allUnary.find(_.toString == s).getOrElse {
         throw ParseError("unary predicate expected, found: " + s)
      }
  }

  def extract(s : StructuralElement)(cont : RelationalElement => Unit) = {
    extractors.foreach(ex => ex(s)(cont))
  }
}
