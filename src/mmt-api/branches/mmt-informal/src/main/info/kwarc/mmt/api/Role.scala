package info.kwarc.mmt.api

/** Roles of expressions, corresponding to productions of the abstract grammar.
 * For simplicity all combinations of arguments are allowed, but only the possible results of the parsing functions constitute legal roles.
 * @param bracketable flag indicating whether rendering expressions with this role may produce brackets
 * @param toString name of the role
 * @param componentNames a list of mappings to access to components by name rather than index
 */
abstract class Role(val bracketable : Boolean, override val toString : String, componentNames : (String,Int)*) {
   def componentByName(s : String) : Option[Int] =
      componentNames.find(_._1 == s).map(_._2)
}
case object Role_Document            extends Role(false, "Document")
case object Role_DRef                extends Role(false, "DRef")
case object Role_MRef                extends Role(false, "MRef")
case object Role_DeclaredTheory      extends Role(false, "Theory", ("name", 0), ("meta", 1))
case object Role_DefinedTheory       extends Role(false, "Theory", ("name", 0), ("definiens", 1))
case object Role_View                extends Role(false, "View", ("name", 0), ("from", 1), ("to", 2))
case object Role_DefinedView         extends Role(false, "DefinedView")
case object Role_Notationset         extends Role(false, "Style")
case object Role_Structure           extends Role(false, "Structure")
case object Role_DefinedStructure    extends Role(false, "DefinedStructure")
case object Role_TGroup              extends Role(false, "TGroup")
case class  Role_Constant(role: Option[String])
  extends Role(false , "Constant" + (role match {case None => "" case Some(s) => ":" + s}), ("name",0), ("type",1), ("definition",2))
case object Role_Alias               extends Role(false, "Alias")
case object Role_Pattern             extends Role(false, "Pattern")
case object Role_Instance            extends Role(false, "Instance")
case object Role_ConAss              extends Role(false, "ConAss")
case object Role_StrAss              extends Role(false, "StrAss")
case object Role_PatAss              extends Role(false, "PatAss")
case object Role_Open                extends Role(false, "Open")
case object Role_Include             extends Role(false, "Include")
case object Role_StrToplevel         extends Role(false, "Toplevel")
case object Role_Notation            extends Role(false, "Notation")
case object Role_Variable            extends Role(false, "Variable")
case object Role_ModRef              extends Role(false, "module")
case object Role_StructureRef        extends Role(false, "structure")
case object Role_ConstantRef         extends Role(false, "constant")
case object Role_ComplexConstantRef extends Role(false, "complex-constant")
case object Role_VariableRef         extends Role(false, "variable")
case object Role_hidden              extends Role(false, "Toplevel")
case class  Role_application(role: Option[String]) extends Role(true,  "application" + (role match {case None => "" case Some(s) => ":" + s}))
case object Role_attribution         extends Role(true,  "attribution")
case object Role_binding             extends Role(true,  "binding")
case object Role_value               extends Role(false, "value")
case object Role_reference           extends Role(false, "reference")
case object Role_foreign             extends Role(false, "foreign")
case object Role_index               extends Role(false, "index")
case object Role_context             extends Role(false, "context")
case object Role_substitution        extends Role(false, "substitution")
case object Role_termsub             extends Role(false, "termsub")
case object Role_ObjToplevel         extends Role(false, "toplevel")
case class  Role_Fragment(kind : String) extends Role(false, "fragment:" + kind)


/** helper object for roles */
object Role {
   /** parses a role from a string */
   def parse(s : String) = s match {
      case "Document" => Role_Document
      case "DRef" => Role_DRef
      case "MRef" => Role_MRef
      case "Theory" => Role_DeclaredTheory
      case "DefinedTheory" => Role_DefinedTheory
      case "View" => Role_View
      case "DefinedView" => Role_DefinedView
      case "Style" => Role_Notationset
      case "Structure" => Role_Structure
      case "TGroup" => Role_TGroup
      case "Constant" => Role_Constant(None)
      case s if s.startsWith("Constant:") => Role_Constant(Some(s.substring(9)))
      case "Variable" => Role_Variable
      case "Include" => Role_Include
      case "DefinedStructure" => Role_DefinedStructure
      case "Alias" => Role_Alias
      case "Pattern" => Role_Pattern
      case "Instance" => Role_Instance
      case "ConAss" => Role_ConAss
      case "StrAss" => Role_StrAss
      case "PatAss" => Role_PatAss
      case "Open" => Role_Open
      case "Notation" => Role_Notation
      case "Toplevel" => Role_StrToplevel
      case "module" => Role_ModRef
      case "structure" => Role_StructureRef
      case "constant" => Role_ConstantRef
      case "complex-constant" => Role_ComplexConstantRef
      case "variable" => Role_VariableRef
      case "hidden" => Role_hidden
      case "application" => Role_application(None)
      case s if s.startsWith("application:") => Role_application(Some(s.substring(12)))
      case "attribution" => Role_attribution
      case "binding" => Role_binding
      case "reference" => Role_reference
      case "substitution" => Role_substitution
      case "termsub" => Role_termsub
      case "context" => Role_context
      case "toplevel" => Role_ObjToplevel
      case "value" => Role_value

      case "index" => Role_index
      case "seqsub" => Role_termsub //TODO fix seqsub
      case "seqsubst" => Role_termsub //TODO fix seqsub
      case "seqitemlist" => Role_termsub //TODO fix seqitemlist
      case "seqvariable" => Role_termsub //TODO fix seqvariable
      case "SeqVariable" => Role_termsub //TODO fix seqvariable
      case "sequpto" => Role_termsub //TODO fix sequpto

      case s if s.startsWith("fragment:") => Role_Fragment(s.substring(9))
      case s => throw ParseError("illegal role: " + s)
   }
}