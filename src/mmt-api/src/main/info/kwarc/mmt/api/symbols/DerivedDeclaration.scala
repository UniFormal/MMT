package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import info.kwarc.mmt.api.libraries.ElaboratedElement
import objects._

import scala.xml.Elem

/** A [[DerivedDeclaration]] is a syntactically like a nested theory.
 *  Its semantics is defined by the corresponding [[StructuralFeature]]
 */
class DerivedDeclaration(h: Term, name: LocalName, val feature: String,
                         components: List[DeclarationComponent], mt : Option[MPath] = None) extends {
   private val t = new DeclaredTheory(h.toMPath.parent, h.toMPath.name/name, mt)
} with NestedModule(t) {
   // overriding to make the type stricter
  override def module: DeclaredModule = t

  override def getComponents = components
  override def toNode : Elem = {
    <derived feature={feature} name={name.toString} base={t.parent.toString}>
      {components.map(c => <component key={c.key.toString}>
      {c.value match {
        case t: TermContainer => t.get.get.toNode
        case p: MPathContainer => p.get.get.toNode
      }}
    </component>)}{t.getDeclarations map (_.toNode)}
    </derived>
  }
  // override def toNodeElab
  override def toNode(rh: presentation.RenderingHandler) {
    rh << s"""<derived feature="$feature" name="${t.name}" base="${t.parent}">"""
    components foreach (c => {
      rh << s"""<component key="${c.key}">"""
      c.value match {
        case t: TermContainer => t.get.get.toNode(rh)
        case p: MPathContainer => p.get.get.toNode(rh)
      }
      rh << s"""</component>"""
    })
    t.getDeclarations foreach(_.toNode(rh))
    rh << "</derived>"
  }
  override def toString = feature +
    {
      name match {
        case LocalName(ComplexStep(p) :: Nil) => ""
        case _ => " " + name
      }
    } + {
    if (components.nonEmpty) "(" + components.map(_.value.toString).mkString(",") + ")"
    else if (components.length == 1) " " + components.head.value.toString
    else ""
  } + {if (t.getDeclarations.nonEmpty) " =\n" + t.innerString else ""}
}


/**
 * a rule that legitimizes a [[StructuralFeature]]
 */
case class StructuralFeatureRule(feature: String, components : List[ComponentKey], mt : Option[MPath] = None, hasname : Boolean = true) extends Rule

/**
 * A StructureFeature defines the semantics of a [[DerivedDeclaration]]
 *
 * The semantics consists of a set of declarations that are injected into the parent theory after the [[DerivedDeclaration]]
 * These are called the 'outer declarations'
 */
abstract class StructuralFeature(val feature: String) extends FormatBasedExtension {
   def isApplicable(s: String) = s == feature

   /** called after checking components and inner declarations */
   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment): Unit

   /**
    * defines the outer perspective of a derived declaration
     *
     * @param parent the containing module
    * @param dd the derived declaration (pre: dd.feature == feature)
    */
   def elaborate(parent: DeclaredModule, dd: DerivedDeclaration): Elaboration

   def modules(d: DerivedDeclaration): List[Module]

}

/**
 * the return type of elaborating a [[DerivedDeclaration]] by a [[StructuralFeature]]
 */
abstract class Elaboration extends ElementContainer[Declaration] {
    def domain: List[LocalName]
    /**
     * default implementation in terms of the other methods
     * may be overridden for efficiency
     */
    def getDeclarations = {
      domain.map {n => getO(n).get}
    }

    def getMostSpecific(name: LocalName): Option[(Declaration,LocalName)] = {
      domain.reverse.foreach {n =>
         name.dropPrefix(n) foreach {suffix =>
           return getO(n) map {d => (d,suffix)}
         }
      }
      return None
    }
}

/**
 * Generative, definitional functors/pushouts with free instantiation
 * called structures in original MMT
 */
class GenerativePushout extends StructuralFeature("generative") {

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
      val dom = dd.getComponent(DomComponent) getOrElse {
        throw GetError("")
      } match {
        case tc: TermContainer => tc.get.getOrElse {
          throw GetError("")
        }
        case _ =>
          throw GetError("")
      }

      val context = parent.getInnerContext
      val body = controller.simplifier.getBody(context, dom)

      new Elaboration {
        /** the morphism dd.dom -> parent of the pushout diagram: it maps every n to dd.name/n */
        private val morphism = new DeclaredView(parent.parent, parent.name/dd.name, dom, parent.toTerm, false)

        private def translate(t: Term): Term = ??? //TODO

        /** precompute domain and build the morphism */
        val domain = body.getDeclarations.map {d =>
          val ddN = dd.name / d.name
          val assig = Constant(morphism.toTerm, d.name, Nil, None, Some(OMS(parent.path ? ddN)), None)
          morphism.add(assig)
          ddN
        }

        def getO(name: LocalName): Option[Declaration] = body.getO(name) map {
          case VarDecl(n,tp,df,nt) =>
             VarDecl(dd.name/n, tp map translate, df map translate, nt).toDeclaration(parent.toTerm)
          case c: Constant =>
             Constant(parent.toTerm, dd.name/c.name, c.alias map (dd.name / _), c.tp map translate, c.df map translate, c.rl, c.notC)
          case nm: NestedModule => ???
          case d => throw LocalError("unexpected declaration in body of domain: " + d)
        }

      }
   }

   def modules(d: DerivedDeclaration): List[Module] = Nil
   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment) {}
}

class InductiveDataTypes extends StructuralFeature("inductive") {
  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
     new Elaboration {
       def domain = {
         dd.module.domain
       }
       def getO(name: LocalName) = {
         dd.module.getO(name) map {d =>
           d //TODO
         }
       }
     }
  }

   def modules(d: DerivedDeclaration): List[Module] = Nil
   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment) {}
}

// Binds theory parameters using Lambda/Pi in an include-like structure
class BoundTheoryParameters(id : String, pi : GlobalName, lambda : GlobalName, applys : GlobalName)
  extends StructuralFeature(id) {

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) : Elaboration = {
    val dom = dd.getComponent(DomComponent) getOrElse {
      throw GetError("")
    } match {
      case tc: TermContainer => tc.get.getOrElse {
        throw GetError("")
      }
      case mc : MPathContainer => mc.get.getOrElse {
        throw GetError("")
      }
      case _ =>
        throw GetError("")
    }
    val context = parent.getInnerContext
    val body = controller.simplifier.getBody(context, dom) match {
      case t : DeclaredTheory => t
      case _ => throw GetError("Not a declared theory: " + dom)
    }
    controller.simplifier.apply(body)
    val vars = body.parameters
    /*
    if (vars.isEmpty) return new Elaboration {
      val includes = PlainInclude(parent.path,body.path) :: body.getIncludes.map(PlainInclude(parent.path,_))
      def domain = includes.map(_.name)
      def getO(name: LocalName): Option[Declaration] = includes.find(_.name == name)
    }
    */

    def bindPi(t : Term) = if (vars.nonEmpty) OMBIND(OMS(pi),vars,t) else t
    def bindLambda(t : Term) = if (vars.nonEmpty) OMBIND(OMS(lambda),vars,t) else t
    def applyPars(t : Term) = if (vars.nonEmpty) vars.foldLeft(t)((tm,v) =>
      OMA(OMS(applys),List(tm,OMV(v.name)))) else t

    new Elaboration {
      var consts : List[FinalConstant] = Nil

      val traverser = new StatelessTraverser {
        def traverse(t: Term)(implicit con : Context, init : State) : Term = t match {
          case OMS(p) if consts.exists(c => c.path == p) => applyPars(t)
          case _ => Traverser(this,t)
        }
      }
      val decls = body.getDeclarations.map(decl => decl match {
        case c : FinalConstant if !c.isGenerated =>
          Some(Constant(parent.toTerm,LocalName(c.parent) / c.name, c.alias.map(LocalName(c.parent) / _),
            c.tp.map(t => bindPi(traverser.apply(t,Context.empty))),
            c.df.map(t => bindLambda(traverser.apply(t,Context.empty))),c.rl,c.notC))
        case c : FinalConstant => Some(Constant(parent.toTerm,LocalName(c.parent) / c.name,
          c.alias.map(LocalName(c.parent) / _),
          c.tp,c.df,c.rl,c.notC))
        case s : DeclaredStructure =>
          val ns = DeclaredStructure(parent.toTerm,LocalName(s.parent) / s.name,s.from,s.isImplicit)
          ElaboratedElement.set(ns)
          Some(ns)
        case d : DerivedDeclaration =>
          //val nd = new DerivedDeclaration(parent.toTerm,LocalName(d.parent) / d.name,d.feature,d.getComponents)
          //ElaboratedElement.set(nd)
          //nd
          None
        case _ => throw new Exception("TODO: BoundTheoryParameters/Elaboration Declaration case " + decl.getClass)
      }) collect {case Some(x) => x}
      //body
      def domain: List[LocalName] = decls.map(_.name)
      def getO(name: LocalName): Option[Declaration] = decls.find(_.name == name)
    }

  }
  def modules(d: DerivedDeclaration): List[Module] = Nil
  def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment) {}
}