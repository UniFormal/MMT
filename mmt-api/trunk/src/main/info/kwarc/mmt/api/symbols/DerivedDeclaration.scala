package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import objects._

/** A [[DerivedDeclaration]] is a syntactically like a nested theory.
 *  Its semantics is defined by the corresponding [[StructuralFeature]] 
 */
class DerivedDeclaration(h: Term, name: LocalName, val feature: String, components: List[DeclarationComponent]) extends {
   private val t = new DeclaredTheory(h.toMPath.parent, h.toMPath.name/name, None)
} with NestedModule(t) {
   // overriding to make the type stricter
   override val module: DeclaredModule = t
}


class StructuralFeatureRule(val feature: String) extends Rule {
  val head = null
}

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
          val assig = Constant(morphism.toTerm, d.name, None, None, Some(OMS(parent.path ? ddN)), None)
          morphism.add(assig)
          ddN
        }
        
        def getO(name: LocalName): Option[Declaration] = body.getO(name) map {
          case VarDecl(n,tp,df,nt) =>
             VarDecl(dd.name/n, tp map translate, df map translate, nt).toDeclaration(parent.toTerm)
          case c: Constant =>
             Constant(parent.toTerm, dd.name/c.name, None, c.tp map translate, c.df map translate, None, c.notC)
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