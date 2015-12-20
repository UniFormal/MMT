package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import objects._

class DerivedDeclaration(home: Term, name: LocalName, feature: String, components: List[DeclarationComponent]) extends {
   val theory = new DeclaredTheory(home.toMPath.parent, home.toMPath.name/name, None)
} with NestedModule(theory)

abstract class StructuralFeature(key: String) extends Extension {
   
   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment): Unit
   
   def declarationNames(d: DerivedDeclaration): List[LocalName]
   
   def declaration(ln: LocalName): Declaration
   
   def declarations(d: DerivedDeclaration): List[Declaration] = declarationNames(d).map(declaration)
   
   def moduleNames(d: DerivedDeclaration): List[MPath]

   def modules(d: DerivedDeclaration): List[Module]
}

abstract class GenerativePushout extends StructuralFeature("structure") {
   
   def domain(ds: DerivedDeclaration): List[LocalName]
   
   def declarations(ds: DerivedDeclaration): List[Declaration]
   
   def moduleNames(ds: DerivedDeclaration): List[MPath]

   def modules(ds: DerivedDeclaration): List[Module]
   
}