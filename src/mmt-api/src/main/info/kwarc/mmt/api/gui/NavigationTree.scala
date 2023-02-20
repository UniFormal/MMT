package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import parser._
import utils._

import javax.swing.tree.DefaultMutableTreeNode

/** abstraction for rendering trees in different UIs */
abstract class NavigationTreeImplementation[T <: NavigationTree, N <: NavigationTreeElement] {
  /** makes a UI-tree from the MMT-info about it
   *  must satisfy makeTree(i).getInfo == i
   */
  def makeTree(info: NavigationTreeInfo): T
  /** makes a UI-node from the MMT-info about it
   *  must satisfy makeNode(i).getInfo == i
   */
  def makeNode(info: NavigationTreeElementInfo): N
  
  /** retrieves the lowest node of the tree whose region covers an offset */
  def getElementAtOffset(tree: T, offset: Int): N
  /** retrieves the lowest node of the tree whose region covers a range */
  def getElementAtRange(tree: T, from: Int, to: Int): N
}

/** an abstraction for the classes used by UI-specific tree implementations */
trait NavigationTree {
  def getInfo: NavigationTreeInfo
} 

/** an abstraction for the classes used by UI-specific tree implementations */
trait NavigationTreeElement {
  def getInfo: NavigationTreeElementInfo
} 

/** global information about a [[NavigationTree]] */
case class NavigationTreeInfo(src: File)

/** the semantic information to be stored in the nodes of a [[NavigationTreeElement]] */
/* TODO all (currently jEdit-specific) MMTAsset classes should become subclasses of this class
 * jEdit only needs one wrapper class around this one that extends SourceAsset
 * the makeNode method of `object SidekickTree extends NavigationTreeImplementation[SidekickParsedData,SourceAsset]` would apply the wrapper.
 * The parse method of MMTSidekick can be implemented generically in terms of an arbitrary NavigationTreeImplementation.
 */
abstract class NavigationTreeElementInfo {
  def region: SourceRegion
}

trait NavigationTreeElements {

}

abstract class NavigationTreeBuilder(controller:Controller) {
  def makeDocument(doc : Document,region : SourceRegion) : MMTElemAsset
  def makeNRef(uri : Path,region : SourceRegion) : MMTURIAsset
  def makeModule(mod : Module, region : SourceRegion) : MMTElemAsset
  def makeSection(s : String): MMTAuxAsset
  def makeDeclaration(dec : Declaration, region : SourceRegion) : MMTElemAsset
  def makeComponent(t : Term,cont : Context, parent : CPath, region: SourceRegion): MMTObjAsset
  def makeTerm(t : Term, pragmatic : Term, cont : Context, parent : CPath, label : String, region : SourceRegion) : MMTObjAsset
  def makeNotation(owner: ContentPath, cont: NotationContainer, comp: NotationComponentKey, region : SourceRegion) : MMTNotAsset
  def makeVariableInContext(con : Context, vd : VarDecl,parent: CPath, region: SourceRegion) : MMTObjAsset

  protected def moduleLabel(m: Module) = m.feature + " " + m.name.last
  protected def declarationLabel(dec : Declaration) = dec match {
    case Include(id) =>
      val s = id.df match {
        case Some(OMMOD(incl)) => incl.last
        case _ => id.from.last
      }
      val kw = if (id.isRealization) "realize" else "include"
      kw + " " + s
    case r: RuleConstant => r.feature
    case d => d.feature + " " + d.name.toStr(true)
  }
  protected def notationLabel(comp : NotationComponentKey) = comp match {
    case ParsingNotationComponent => "notation (parsing)"
    case PresentationNotationComponent => "notation (presentation)"
    case VerbalizationNotationComponent => "notation (verbalization)"
  }

  /** @param parentReg the region of the parent node in the tree
    * @param e the object at the child node
    * @return the region of e (if any and if a subregion of parent), else an invalid region
    */
  private def getRegion(parentReg: SourceRegion,e: metadata.HasMetaData) : SourceRegion = {
    SourceRef.get(e).flatMap {ref =>
      val reg = ref.region
      if (parentReg contains reg) Some(reg) else None
    }.getOrElse(SourceRegion.none)
  }

  /* build the sidekick outline tree: document node */
  def buildTreeDoc(node: DefaultMutableTreeNode, doc: Document): Unit = {
    val reg = SourceRef.get(doc).map(_.region) getOrElse SourceRegion.none
    val child = new DefaultMutableTreeNode(makeDocument(doc,reg))
    node.add(child)
    doc.getDeclarations foreach {
      case d: Document =>
        buildTreeDoc(child, d)
      case r: NRef =>
        val rReg = getRegion(reg,r)
        val rChild = new DefaultMutableTreeNode(makeNRef(r.target,rReg))
        r match {
          case d: DRef =>
            child.add(rChild)
          case m: MRef =>
            try {
              val mod = controller.localLookup.getModule(m.target)
              buildTreeMod(child, mod, Context.empty, reg)
            } catch {case e: Error =>
              // graceful degradation in case module could not be parsed
              child.add(rChild)
            }
          case s: SRef =>
        }
      case ii: InterpretationInstruction =>
      case oe: opaque.OpaqueElement =>
    }
  }

  /* build the sidekick outline tree: module node */
  private def buildTreeMod(node: DefaultMutableTreeNode, mod: Module, context: Context, parentReg: SourceRegion): Unit = {
    val reg = getRegion(parentReg,mod)
    val child = new DefaultMutableTreeNode(makeModule(mod,reg))
    node.add(child)
    buildTreeComps(child, mod, context, reg)
    mod match {
      case m: Module =>
        val defElab = m.getDeclarations.filter(_.getOrigin == ElaborationOfDefinition)
        if (defElab.nonEmpty) {
          val elabChild = new DefaultMutableTreeNode(makeSection("-- flat definition --"))
          child.add(elabChild)
          defElab foreach {d => buildTreeDecl(elabChild, m, d, context ++ m.getInnerContext, reg)}
        }
        val incls = m.getDeclarations.filter {d => Include.unapply(d).isDefined && d.isGenerated}
        if (incls.nonEmpty) {
          val inclsChild = new DefaultMutableTreeNode(makeSection("-- flat includes --"))
          child.add(inclsChild)
          incls foreach {d => buildTreeDecl(inclsChild, m, d, context ++ m.getInnerContext, reg)}
        }
        m.getPrimitiveDeclarations foreach {d => buildTreeDecl(child, m, d, context ++ m.getInnerContext, reg)}
    }
  }
  /** build the sidekick outline tree: declaration (in a module) node */
  private def buildTreeDecl(node: DefaultMutableTreeNode, parent: ContainerElement[_ <: Declaration], dec: Declaration, context: Context, parentReg: SourceRegion): Unit = {
    val reg = getRegion(parentReg,dec)
    dec match {
      case nm: NestedModule =>
        buildTreeMod(node, nm.module, context, reg)
        return
      case _ =>
    }
    val child = new DefaultMutableTreeNode(makeDeclaration(dec,reg))
    node.add(child)
    buildTreeComps(child, dec, context, reg)
    dec match {
      case dd: DerivedDeclaration =>
        val contextInner = context ++ controller.getExtraInnerContext(dd)
        dec.getDeclarations foreach {d => buildTreeDecl(child, dd, d, contextInner, reg)}
      case _ =>
    }
    // a child with all declarations elaborated from dec
    val elab = parent.getDeclarations.filter(_.getOrigin == ElaborationOf(dec.path))
    if (elab.nonEmpty) {
      val elabChild = new DefaultMutableTreeNode(makeSection("-- elaboration --"))
      child.add(elabChild)
      elab foreach {e => buildTreeDecl(elabChild, parent, e, context, parentReg)}
    }
  }

  /** add child nodes for all components of an element */
  private def buildTreeComps(node: DefaultMutableTreeNode, ce: ContentElement, context: Context, parentReg: SourceRegion): Unit = {
    ce.getComponents foreach {
      case DeclarationComponent(comp, cont: AbstractTermContainer) if cont.get.isDefined =>
        buildTreeComp(node, ce.path $ comp, cont.get.get, context, parentReg)
      case NotationComponent(comp, cont) =>
        buildTreeNot(node, ce.path, cont, comp, parentReg)
      case _ =>
    }
  }

  /** build the sidekick outline tree: component of a (module or symbol level) declaration */
  private def buildTreeComp(node: DefaultMutableTreeNode, parent: CPath, t: Term, context: Context, parentReg: SourceRegion): Unit = {
    val reg = getRegion(parentReg,t)
    val child = new DefaultMutableTreeNode(makeComponent(t,context,parent,reg))//,t,context,parent,parent.component.toString,reg))
    node.add(child)
    buildTreeTerm(child, parent, t, context, parentReg)
  }

  /** build the sidekick outline tree: notations */
  private def buildTreeNot(node: DefaultMutableTreeNode, owner: ContentPath, cont: NotationContainer, comp: NotationComponentKey, parentReg: SourceRegion): Unit = {
    val reg = getRegion(parentReg,cont(comp).get)
    val child = new DefaultMutableTreeNode(makeNotation(owner,cont,comp,reg))
    node.add(child)
  }

  /** build the sidekick outline tree: context node (each VarDecl is added individually) */
  private def buildTreeCont(node: DefaultMutableTreeNode, parent: CPath, con: Context, context: Context, parentReg: SourceRegion): Unit = {
    con mapVarDecls {case (previous, vd @ VarDecl(_, _, tp, df, _)) =>
      val reg = getRegion(parentReg,vd)
      val currentContext = context ++ previous
      val child = new DefaultMutableTreeNode(makeVariableInContext(currentContext,vd,parent,reg))
      node.add(child)
      (tp.toList:::df.toList) foreach {t =>
        buildTreeTerm(child, parent, t, currentContext, reg)
      }
    }
  }

  /** build the sidekick outline tree: (sub)term node */
  private def buildTreeTerm(node: DefaultMutableTreeNode, parent: CPath, t: Term, context: Context, parentReg: SourceRegion): Unit = {
    var extraLabel = ""
    val reg = getRegion(parentReg,t)
    if (reg == SourceRegion.none) {
      extraLabel = " [not in source]" // lack of source region indicates inferred subterms
    }
    val tP = controller.pragmatic.mostPragmatic(t)
    val label = tP match {
      case OMV(n) => n.toString + " (Var)"
      case OMS(p) => p.name.toString + " (?" + p.module.name + ")"
      case OMID(p) => p.name.toString
      case l: OMLITTrait => l.toString
      case OML(nm,_,_,_,_) => nm.toString
      case OMSemiFormal(_) => "unparsed: " + tP.toString
      case OMA(OMS(p),_) => p.name.toString + " (?" + p.module.name + ")"
      case OMBINDC(OMS(p),_,_) => p.name.toString + " (?" + p.module.name + ")"
      case ComplexTerm(op, _,_,_) => op.name.last.toStr(true)
      case OMA(OMID(p),_) => p.name.last.toStr(true)
      case OMBINDC(OMID(p),_,_) => p.name.last.toStr(true)
      case OMA(ct,args) => "OMA" // TODO probably shouldn't occur, but does
    }
    val asset = makeTerm(t,tP,context,parent,label+extraLabel,reg)// new MMTObjAsset(mmt, t, tP, context, parent, label+extraLabel, reg)
    val child = new DefaultMutableTreeNode(asset)
    node.add(child)
    tP match {
      case OML(_,tp,df,_,_) =>
        (tp.toList:::df.toList) foreach {t =>
          buildTreeTerm(child, parent, t, context, reg)
        }
      case OMBINDC(binder,cont, scopes) =>
        if (! binder.isInstanceOf[OMID])
          buildTreeTerm(child, parent, binder, context, reg)
        buildTreeCont(child, parent, cont, context, reg)
        scopes foreach {s =>
          buildTreeTerm(child, parent, s, context ++ cont, reg)
        }
      case OMA(fun, args) =>
        if (! fun.isInstanceOf[OMID])
          buildTreeTerm(child, parent, fun, context, reg)
        args.foreach(buildTreeTerm(child, parent, _, context, reg))
      case _ => t.subobjects foreach {
        case (_, o: Term) => buildTreeTerm(child, parent, o, context, reg)
        case _ =>
      }
    }
  }
}

/** Assets --------------- */


/** node in the sidekick outline tree: common ancestor class
  * @param name the label of the asset
  * @param region the source region of the asset
  */
trait MMTAsset {
  protected val label : String
  val region : SourceRegion

  /** the base URIto use in the context of this asset */
  def getScope : Option[MPath]
}

/**
  * a node for URIs
  */
trait MMTURIAsset extends MMTAsset {
  val path : Path
  protected val label = path.last
  def getScope = path match {
    case p: MPath => Some(p)
    case _ => None
  }
}

/** node for structural elements
  * @param elem the node in the MMT syntax tree
  */
trait MMTElemAsset extends MMTAsset {
  val elem : StructuralElement
  def path = elem.path
  def getScope : Option[MPath] = elem match {
    case _ : NarrativeElement => None
    case c : ContentElement => c match {
      case t: Theory => Some(t.path)
      case v: View => None //would have to be parsed to be available
      case d: Declaration => Some(d.path.module)
      case _ => None
    }
  }
}

/** node for objects
  */
trait MMTObjAsset extends MMTAsset {
  protected val controller : Controller
  val obj: Obj
  val pragmatic: Obj
  val context: Context
  val parent: CPath

  def getScope = parent.parent match {
    case cp: ContentPath => Some(cp.module)
    case _ => None
  }

  def getFullContext = getScope.map(Context(_)).getOrElse(Context.empty) ++ context

  /** tries to infer the type of this asset (may throw exceptions) */
  def inferType: Option[Term] = {
    obj match {
      case t: Term =>
        val thy = getScope.getOrElse(return None)
        checking.Solver.infer(controller, getFullContext, t, None)
      case _ => None
    }
  }
}

trait MMTNotAsset extends MMTAsset {
  protected val owner : ContentPath
  protected val not : TextNotation
  def getScope = Some(owner.module)
}

/** a dummy asset for structuring the tree */
trait MMTAuxAsset // extends enhanced.SourceAsset(label, -1, MyPosition(-1))}
