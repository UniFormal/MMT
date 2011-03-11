package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._

/**
 * An object of type Unary represents a unary predicate on MMT paths in the MMT ontology.
 */
sealed abstract class Unary(val desc : String) {
   implicit def toQuery : Query = HasType(this)
   def apply(p : Path) = Individual(p, this)
}
case object IsDocument extends Unary("document")
case object IsTheory extends Unary("theory")
case object IsView extends Unary("view")
case object IsStyle extends Unary("style")
case object IsStructure extends Unary("structure")
case object IsConstant extends Unary("constant")
case object IsAlias extends Unary("alias")
case object IsConAss extends Unary("conass")
case object IsStrAss extends Unary("strass")
case object IsOpen extends Unary("open")
case object IsNotation extends Unary("notation")


object Unary {
   def parse(s: String) : Unary = s match {
      case "IsDocument" => IsDocument
      case "IsTheory" => IsTheory
      case "IsView" => IsView
      case "IsStyle" => IsStyle
      case "IsStructure" => IsStructure
      case "IsConstant" => IsConstant
      case "IsAlias" => IsAlias
      case "IsConAss" => IsConAss
      case "IsStrAss" => IsStrAss
      case "IsOpen" => IsOpen
      case "IsNotation" => IsNotation
      case _ => throw ParseError("unary predicate expected, found: " + s)
   }
}

/**
 * An object of type Binary represents a binary predicate between MMT paths in the MMT ontology.
 */
sealed abstract class Binary(val desc : String) {
   def apply(subj : Path, obj : Path) = Relation(this, subj, obj)
   implicit def toQuery : Query = ToObject(this)
   def unary_- = ToSubject(this)
   def unary_+ = ToObject(this)
}

//symbol - symbol
case object HasOccurrenceOfInType extends Binary("S refers to O in type")
case object HasOccurrenceOfInDefinition extends Binary("S refers to O in definition")
case object DependsOnTypeOf extends Binary("S depends on the type of O")
case object DependsOnDefiniensOf extends Binary("S depends on the definiens of O")
case object HasOccurrenceOfInTarget extends Binary("S refers to O")
case object IsAliasFor extends Binary("S is alias for O")
//module - module
case object HasOccurrenceOfInImport extends Binary("S imports from O")
//theory - theory 
case object HasMeta extends Binary("S has meta-theory O")
//link - theory, style - any
case object HasDomain extends Binary("S has domain O")
case object HasCodomain extends Binary("S has codomain O")
//parent - child (many-many relation because a declaration may be referenced in other documents)
case object Declares extends Binary("S contains declaration of O")

object Binary {
   def parse(s: String) : Binary = s match {
      case "HasOccurrenceOfInType" => HasOccurrenceOfInType
      case "HasOccurrenceOfInDefinition" => HasOccurrenceOfInDefinition
      case "HasOccurrenceOfInTarget" => HasOccurrenceOfInTarget
      case "IsAliasFor" => IsAliasFor
      case "HasOccurrenceOfInImport" => HasOccurrenceOfInImport
      case "HasMeta" => HasMeta
      case "HasDomain" => HasDomain
      case "HasCodomain" => HasCodomain
      case "Declares" => Declares
      case _ => throw ParseError("binary predicate expected, found: " + s)
   }
}

/** An ABoxDecl is a declaration in an ABox for the MMT ontology. */
abstract class ABoxDecl {
   val path : Path
   val toNode : scala.xml.Node
   val toLocutor : String
}

/**
 * An object of type Individual represents a unary predicate in the ABox.
 */
case class Individual(path : Path, tp : Unary) extends ABoxDecl {
   lazy val toNode = <individual path={path.toString} predicate={tp.toString}/>
   lazy val toLocutor = {
      val xpath = path match {
         case p : DPath => "/"
         case _ ? mod => "/*[name='" + mod + "']"
         case _ ? mod ?? sym => "/*[name='" + mod.flat + "']/*[name='" + sym.flat + "']"
      }
     "\"" + path + "\" : " + tp + " (url=\"" + path + "\", xpath=\"" + xpath + "\")"
   }
}

/**
 * An object of type Relation represents a binary predicate in the ABox.
 */
case class Relation(dep : Binary, subj : Path, obj : Path) extends ABoxDecl {
   val path = subj
   lazy val toNode = <relation subject={subj.toString} predicate={dep.toString} object={obj.toString}/>
   lazy val toLocutor = "\"" + subj + "\" :-" + dep + "-> \"" + obj + "\""   
}