package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._

/**
 * An object of type Unary represents a unary predicate on MMT paths in the MMT ontology.
 * The semantics of these objects is given by their name
 */
sealed abstract class Unary(val desc : String) {
   /** yields the corresponding relational item that classifies p */ 
   def apply(p : Path) = Individual(p, this)
}
case object IsDocument extends Unary("document")
case object IsTheory extends Unary("theory")
case object IsView extends Unary("view")
case object IsStyle extends Unary("style")
case object IsStructure extends Unary("structure")
case class IsConstant(s: Option[String]) extends Unary("constant" + s.mkString(":",null,""))
case object IsPattern extends Unary("pattern")
case object IsInstance extends Unary("instance")
case object IsAlias extends Unary("alias")
case object IsConAss extends Unary("conass")
case object IsStrAss extends Unary("strass")
case object IsOpen extends Unary("open")
case object IsNotation extends Unary("notation")

/** helper object for unary items */ 
object Unary {
   private val all = List(IsDocument,IsTheory,IsView,IsStyle,IsStructure,IsAlias,IsConAss,
                          IsStrAss,IsOpen,IsNotation,IsPattern,IsInstance)
   def parse(s: String) : Unary = s match {
      case "constant" => IsConstant(None)
      case s if s.startsWith("constant:") => IsConstant(Some(s.substring(9)))
      case s => all.find(_.toString == s).getOrElse {
         throw ParseError("unary predicate expected, found: " + s)
      }
   }
}

/**
 * An object of type Binary represents a binary predicate between MMT paths in the MMT ontology.
 * The semantics of these objects is given by their name
 */
sealed abstract class Binary(val desc : String) {
   /** yields the corresponding relational item that classifies p */ 
   def apply(subj : Path, obj : Path) = Relation(this, subj, obj)
   /** syntactic sugar for queries: ToSubject(this) */
   def unary_- = ToSubject(this)
   /** syntactic sugar for queries: ToObject(this) */
   def unary_+ = ToObject(this)
}

// module - module, symbol (assignment) - module
case object DependsOn extends Binary("S depends on O")
// theory - theory
case object HasMeta extends Binary("S has meta-theory O")
case object Includes extends Binary("S includes O")
//link - theory, style - any
case object HasDomain extends Binary("S has domain O")
case object HasCodomain extends Binary("S has codomain O")
// constant - constant (not used yet)
case object DependsOnTypeOf extends Binary("S depends on the type of O")
case object DependsOnDefiniensOf extends Binary("S depends on the definiens of O")
case object IsInstanceOf extends Binary("S is instance of O")

// constant (assignment) - constant (deprecated)
case object HasOccurrenceOfInType extends Binary("S refers to O in type")
case object HasOccurrenceOfInDefinition extends Binary("S refers to O in definition")
case object HasOccurrenceOfInTarget extends Binary("S refers to O")

//parent - child (many-many relation because a declaration may be referenced in other documents)
case object Declares extends Binary("S contains declaration of O")
// symbol - symbol, module - module
case object IsAliasFor extends Binary("S is alias for O") 

/** helper methods for Binary items */
object Binary {
   val all = List(HasOccurrenceOfInType,HasOccurrenceOfInDefinition,HasOccurrenceOfInTarget,
                          DependsOnTypeOf,DependsOnDefiniensOf,DependsOn,Includes,IsAliasFor,HasMeta,HasDomain,HasCodomain,Declares)
   def parse(s: String) : Binary = all.find(_.toString == s) match {
      case Some(i) => i
      case _ => HasOccurrenceOfInType // throw ParseError("binary predicate expected, found: " + s) //TODO temporary fix to parse obsolete data
   }
}

/**
 * An object of type Individual represents a unary predicate in the ABox.
 */
case class Individual(path : Path, tp : Unary) extends RelationalElement {
   def toNode = <individual path={path.toPath} predicate={tp.toString}/>
   def toPath = tp.toString + " " + path.toPath
}

/**
 * An object of type Relation represents a binary predicate in the ABox.
 * The path of a Relation is always the knowledge item reading which produced the Relation.
 */
case class Relation(dep : Binary, subj : Path, obj : Path) extends RelationalElement {
   val path = subj
   def toNode = <relation subject={subj.toPath} predicate={dep.toString} object={obj.toPath}/>
   override def toString = subj.toString + " " + dep.toString + " " + obj.toString
   def toPath = dep.toString + " " + subj.toPath + " " + obj.toPath
}
