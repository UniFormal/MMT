package info.kwarc.mmt.mizar.mizarobjects

/** abstract class for the various kinds of definitions in Mizar */
sealed abstract class Definition extends MizarDeclaration {
  def name:MizarGlobalName
  def arguments:List[MizarLocalVariable]
  def kind: String
  def partial: Option[Boolean]
}

case class FunctionDefinition(arguments:List[MizarLocalVariable], name: MizarGlobalName, ret: MizarType, definingFormula: DefiningFormula) extends Definition {
  def kind = {"function"}
  def partial = {Some(definingFormula.partial)}
}

case class PredicateDefinition(arguments:List[MizarLocalVariable], name: MizarGlobalName, definingFormula: DefiningFormula) extends Definition {
  def kind = {"predicate"}
  def partial = {Some(definingFormula.partial)}
}

case class AttributeDefinition(arguments:List[MizarLocalVariable], name: MizarGlobalName, domain: MizarType, definingFormula: DefiningFormula) extends Definition {
  def kind = {"attribute"}
  def partial = {Some(definingFormula.partial)}
}

case class ModeDefinition(arguments:List[MizarLocalVariable], name: MizarGlobalName, containedAttributes: List[AttributeDefinition], domain: MizarType, definingFormula: DefiningFormula) extends Definition {
  def kind = {"mode"}
  def partial = {Some(definingFormula.partial)}
}

case class StructureDefinition(arguments:List[MizarLocalVariable], name: MizarGlobalName, extendedStructures: List[StructureDefinition], fields: List[MizarType]) extends Definition {
  def kind = {"structure"}
  def partial={None}
}