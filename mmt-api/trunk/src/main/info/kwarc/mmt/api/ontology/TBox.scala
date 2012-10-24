package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import info.kwarc.mmt.api.moc._


/**
 * An object of type Unary represents a unary predicate on MMT paths in the MMT ontology.
 * The semantics of these objects is given by their name
 */
sealed abstract class Unary(override val toString : String) {
   /** yields the corresponding relational item that classifies p */ 
   def apply(p : Path) = Individual(p, this) 
}
case object IsDocument extends Unary("document")
case object IsTheory extends Unary("theory")
case object IsView extends Unary("view")
case object IsStyle extends Unary("style")
case object IsStructure extends Unary("structure")
case class IsConstant(rl: Option[String]) extends Unary("constant" + rl.mkString(":",null,""))
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

// module - module, component - component
case object DependsOn extends Binary("S depends on O")
// theory - theory
case object HasMeta extends Binary("S has meta-theory O")
case object Includes extends Binary("S includes O")
//link - theory, style - any
case object HasDomain extends Binary("S has domain O")
case object HasCodomain extends Binary("S has codomain O")
// constant - constant (not used yet)
case object IsInstanceOf extends Binary("S is instance of O")
//path - path
case object RefersTo extends Binary("S refers to O")
//parent - child (many-many relation because a declaration may be referenced in other documents)
case object Declares extends Binary("S contains declaration of O")
// symbol - symbol, module - module
case object IsAliasFor extends Binary("S is alias for O") 

/** helper methods for Binary items */
object Binary {
   val all = List(RefersTo,DependsOn,Includes,IsAliasFor,IsInstanceOf,HasMeta,HasDomain,HasCodomain,Declares)
   def parse(s: String) : Binary = all.find(_.toString == s) match {
      case Some(i) => i
      case _ => throw ParseError("binary predicate expected, found: " + s)
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

/** 
 * classes for changes to the relstore
 * will be returned by the checker when checking a change
 * and then applied by the controller to the relstore to mantain its adequacy
 */

abstract class RelationsChange extends Change

/** adds a list of relations to the relstore */
case class AddRelations(rels : List[RelationalElement]) extends RelationsChange {
  def toNode = <addrel>{rels.map(_.toNode)}</addrel>
}

/** removes from the relstore all relations for which p is subject */ 
case class DeleteRelations(p : Path) extends RelationsChange {
  def toNode = <delrel path={p.toPath}></delrel>
}

/** removes from the relstore all relations for which p is subject, then adds rels */
case class UpdateRelations(rels : List[RelationalElement], p : Path) extends RelationsChange {
  def toNode = <uprel path={p.toPath}>{rels.map(_.toNode)}</uprel>
}

/** replaces in the relstore, in all relations for which old is subject, old with nw */
case class RenameRelations(old : Path, nw : Path) extends RelationsChange {
  def toNode = <rerel old={old.toPath} new={nw.toPath}/>
}

/** does nothing */
case class IdenticalRelations() extends RelationsChange {
  def toNode = <idrel/>
}