package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import checking._

class NamedInductiveTypes extends StructuralFeature("inductive") {
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

