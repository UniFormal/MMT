package info.kwarc.mmt.api.documents

import info.kwarc.mmt.api._
import utils._

/**
 * elements that have no semantics in themselves but help interpret other elements,
 * primarily used to guide reconstruction of information that is omitted in concrete syntax
 */ 
sealed abstract class InterpretationInstruction extends NarrativeElement {
  def parent: DPath
  def arguments: List[String]
  
  def getDeclarations = Nil
  def parentOpt = Some(parent)
  lazy val name = LocalName("instrunction_" + toString.hashCode.toString)
  def path = parent / name
 
  override def toString = (feature :: arguments).mkString(" ")
  def toNode = <instruction text={toString}/>
}  

/** defines the default namespace */
case class Namespace(parent: DPath, namespace: Path) extends InterpretationInstruction {
  val feature = "namespace"
  def arguments = List(namespace.toPath)
}

/** defines a namespace as an abbreviation for a Path */
case class NamespaceImport(parent: DPath, prefix: String, namespace: DPath) extends InterpretationInstruction {
  val feature = "import"
  def arguments = List(prefix, namespace.toPath)
}

/** defines a fixed meta-theory */
case class FixedMeta(parent: DPath, meta: MPath) extends InterpretationInstruction {
  val feature = "fixmeta"
  def arguments = List(meta.toPath)
}

object InterpretationInstruction {
  val namespace = "namespace"
  val namespaceImport = "import"
  val fixedmeta = "fixmeta"
  val all = List(namespace, namespaceImport, fixedmeta)
  def parse(parent: DPath, text: String, nsMap: NamespaceMap) = {
    val parts = stringToList(text)
    parts match {
      case this.namespace :: ns :: Nil => Namespace(parent, Path.parse(ns, nsMap)) 
      case this.namespaceImport :: pr :: ns :: Nil => NamespaceImport(parent, pr, Path.parseD(ns, nsMap))
      case this.fixedmeta :: mt :: Nil => FixedMeta(parent, Path.parseM(mt, nsMap))
      case _ => throw ParseError("unknown interpretaton instruction: " + text)
    }
  }
}