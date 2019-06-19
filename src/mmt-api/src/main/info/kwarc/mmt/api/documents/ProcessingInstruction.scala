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
  lazy val name = LocalName("instruction_" + toString.hashCode.toString)
  def path = parent / name
 
  override def toString = (feature :: arguments).mkString(" ")
  def toNode = <instruction text={toString}/>
}

/** keeps track of a list of interpretation instructions and performs lookups in them */
class InterpretationInstructionContext(initNsMap: NamespaceMap) {
  private var instructions: List[InterpretationInstruction] = Nil
  /** all interpretation instructions seen so far, reverse order */
  def getInstructions = instructions
  /** optimization: keep the result of chaining all namespace-relevant interpretation instructions */ 
  private var _namespaces = initNsMap
  /** the namespace mapping induced by the initial and the processed interpretation instructions */
  def namespaces = _namespaces
  
  /** looks up the fixed metatheory */
  def meta = instructions.mapFind {
    case fm: FixedMeta => Some(fm.meta)
    case _ => None
  }
  
  /** mutably takes note of an instruction */
  def process(ii: InterpretationInstruction) = {
    instructions ::= ii
    ii match {
      case Namespace(_, ns) => _namespaces = _namespaces.apply(ns)
      case NamespaceImport(_, pr,ns) => _namespaces = _namespaces.add(pr, ns.uri)
      case _ =>
    }
  }
  
  /** copies the mutable data structures */
  def copy() = {
    val iic = new InterpretationInstructionContext(_namespaces)
    iic.instructions = instructions
    iic
  }
}

object InterpretationInstructionContext {
  def apply():InterpretationInstructionContext = apply(NamespaceMap.empty)
  def apply(nsMap: NamespaceMap): InterpretationInstructionContext = new InterpretationInstructionContext(nsMap)
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

case class DocumentRule(parent: DPath, rulepath: MPath, rule: Option[Rule]) extends InterpretationInstruction {
  val feature = "documentrule"
  def arguments = List(rulepath.toPath)
}

object InterpretationInstruction {
  val namespace = "namespace"
  val namespaceImport = "import"
  val fixedmeta = "fixmeta"
  val documentrule = "rule"
  val all = List(namespace, namespaceImport, fixedmeta, documentrule)
  def parse(controller: frontend.Controller, parent: DPath, text: String, nsMap: NamespaceMap) = {
    val parts = stringToList(text)
    parts match {
      case this.namespace :: ns :: Nil => Namespace(parent, Path.parse(ns, nsMap)) 
      case this.namespaceImport :: pr :: ns :: Nil => NamespaceImport(parent, pr, Path.parseD(ns, nsMap))
      case this.fixedmeta :: mt :: Nil => FixedMeta(parent, Path.parseM(mt, nsMap))
      case this.documentrule :: r :: Nil =>
        val rP = Path.parseM(r, nsMap)
        val rl = Rule.loadRule(controller, None, objects.OMMOD(rP))
        val rlO = Some(rl)
        DocumentRule(parent, rP, rlO)
      case _ => throw ParseError("unknown interpretaton instruction: " + text)
    }
  }
}