package jomdoc

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
case object Role_XRef                extends Role(false, "XRef")
case object Role_Theory              extends Role(false, "Theory", ("name", 0), ("meta", 1))
case object Role_View                extends Role(false, "View", ("name", 0), ("from", 1), ("to", 2))
case object Role_DefinedView         extends Role(false, "DefinedView")
case object Role_Notationset         extends Role(false, "Style")
case object Role_Structure           extends Role(false, "Structure")
case object Role_DefinedStructure    extends Role(false, "DefinedStructure")
case class  Role_Constant(univ : objects.Universe)
  extends Role(false , "Constant" + (univ.toString match {case "" => "" case s => ":" + s}), ("name",0), ("type",1), ("definition",2))
case object Role_Alias               extends Role(false, "Alias")
case object Role_Pattern             extends Role(false, "Pattern")
case object Role_Instance            extends Role(false, "Instance")
case object Role_ConAss              extends Role(false, "ConAss")
case object Role_StrAss              extends Role(false, "StrAss")
case object Role_Open                extends Role(false, "Open")
case object Role_Include             extends Role(false, "Include")
case object Role_StrToplevel         extends Role(false, "Toplevel")
case object Role_Notation            extends Role(false, "Notation")
case object Role_Variable            extends Role(false, "Variable")
case object Role_TheoryRef           extends Role(false, "theory")
case object Role_ViewRef             extends Role(false, "view")
case object Role_StructureRef        extends Role(false, "structure")
case object Role_ConstantRef         extends Role(false, "constant")
case object Role_VariableRef         extends Role(false, "variable")
case object Role_hidden              extends Role(false, "Toplevel")
case object Role_application         extends Role(true,  "application" )
case object Role_attribution         extends Role(true,  "attribution" )
case object Role_binding             extends Role(true,  "binding" )
case object Role_morphismapplication extends Role(true,  "morphism-application" )
case object Role_composition         extends Role(true,  "composition" )
case object Role_identity            extends Role(true,  "identity" )
case object Role_value               extends Role(false, "value")
case object Role_foreign             extends Role(false, "foreign")
case object Role_ObjToplevel         extends Role(false, "toplevel")
case class  Role_Fragment(kind : String) extends Role(false, kind)

/** helper object for roles */
object Role {
   /** parses a role from a string */
   def parse(s : String) = s match {
      case "Document" => Role_Document
      case "XRef" => Role_XRef
      case "Theory" => Role_Theory
      case "View" => Role_View
      case "DefinedView" => Role_DefinedView
      case "Style" => Role_Notationset
      case "Structure" => Role_Structure
      case "Constant" => Role_Constant(objects.Individual(None))
      case s if s.startsWith("Constant:") => Role_Constant(objects.Universe.parse(s.substring(9)))
      case "Variable" => Role_Variable
      case "Include" => Role_Include
      case "DefinedStructure" => Role_DefinedStructure
      case "Alias" => Role_Alias
      case "Pattern" => Role_Pattern
      case "Instance" => Role_Instance
      case "ConAss" => Role_ConAss
      case "StrAss" => Role_StrAss
      case "Open" => Role_Open
      case "Notation" => Role_Notation
      case "Toplevel" => Role_StrToplevel
      case "theory" => Role_TheoryRef
      case "view" => Role_ViewRef
      case "structure" => Role_StructureRef
      case "constant" => Role_ConstantRef
      case "variable" => Role_VariableRef
      case "hidden" => Role_hidden
      case "application" => Role_application
      case "attribution" => Role_attribution
      case "binding" => Role_binding
      case "morphism-application" => Role_morphismapplication
      case "composition" => Role_composition
      case "identity" => Role_identity
      case "toplevel" => Role_ObjToplevel
      case s if s.startsWith("fragment:") => Role_Fragment(s.substring(9))
      case s => throw ParseError("illegal role: " + s)
   }
}