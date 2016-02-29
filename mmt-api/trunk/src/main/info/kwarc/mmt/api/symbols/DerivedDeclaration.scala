package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import objects._

/** A [[DerivedDeclaration]] is a syntactically like a nested theory.
 *  Its semantics is defined by the corresponding [[StructuralFeature]]
 */
class DerivedDeclaration(h: Term, name: LocalName, val feature: String, components: List[DeclarationComponent]) extends
  /** the 'inner declarations', i.e., the declarations that are physically part of this declaration */
  NestedModule(new DeclaredTheory(h.toMPath.parent, h.toMPath.name/name, None)) {
  val theory = module.asInstanceOf[DeclaredTheory]
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
abstract class StructuralFeature(val key: String) extends FormatBasedExtension {
   def isApplicable(s: String) = s == key
   /** called after checking components and inner declarations */
   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment): Unit

   def elaborate(context: Context, dd: DerivedDeclaration): ElementContainer[Declaration]

   def modules(d: DerivedDeclaration): List[Module]

}

// Implements DeclaredStructure as DerivedDeclaration
class GenerativePushout extends StructuralFeature("structures") {

  def elaborate(context: Context, dd: DerivedDeclaration) = {
      val dom = dd.getComponent(DomComponent) getOrElse {
        throw GetError("")
      } match {
        case tc: TermContainer => tc.get.getOrElse {
          throw GetError("")
        }
        case _ =>
          throw GetError("")
      }
      val body = controller.simplifier.getBody(context, dom)

      new ElementContainer[Declaration] {
        def domain = body.getDeclarations.map {d => dd.name / d.name}

        def getMostSpecific(name: LocalName): Option[(Declaration,LocalName)] = ???

        def getO(name: LocalName) = {
          getMostSpecific(name) flatMap {case (d,ln) => if (ln.isEmpty) Some(d) else None}
        }

        def getDeclarations = {
          domain.toList.map {n => getO(n).get}
        }
      }
   }

   def modules(d: DerivedDeclaration): List[Module] = Nil
   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment) {}
}
