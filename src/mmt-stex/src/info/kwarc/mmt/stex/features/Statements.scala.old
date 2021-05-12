package info.kwarc.mmt.stex.features
/*
import info.kwarc.mmt.api._
import modules._
import objects._
import notations._
import symbols._
import utils._
import checking._

object DefinitionFeature extends StructuralFeature("stex-definition") {

   /** default notation */
   def getHeaderNotation = List(LabelArg(1, LabelInfo.none), Delim(":"), SimpArg(1))

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     if (!dd.tpC.isDefined) {
       env.errorCont(InvalidElement(dd, "stex definition requires a type: reference to the definiendum"))
       return
     }
     val tp = dd.tpC.get.get
     val cu = CheckingUnit.byInference(None, Context(), tp)
     tp match {
       case OMID(definiendum) =>
         val checkResult = env.objectChecker.apply(cu, env.rules)(env.ce)
         if (!checkResult.solved) {
           env.errorCont(InvalidElement(dd, "invalid definition type, check failed"))
           return
         }
       case _ =>
         env.errorCont(InvalidElement(dd, "invalid definition type, expected reference to a definiendum"))
         return
     }

     if (dd.getDeclarations.size != 1) {
        env.errorCont(InvalidElement(dd, "stex definition requires exactly one declaration -- the definition"))
        return
     }
   }

   def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = new Elaboration {
     lazy val domain = {
       dd.getDeclarations.map {d =>
         dd.name / d.name
       }
     }

     def getO(n: LocalName): Option[Declaration] = {
       dd.module.getO(n.tail)
     }
   }
}

object ExampleFeature extends StructuralFeature("stex-example") {

   /** default notation */
   def getHeaderNotation = List(LabelArg(1, LabelInfo.none), Delim(":"), SimpArg(1))

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     if (!dd.tpC.isDefined) {
       env.errorCont(InvalidElement(dd, "stex eaxmple requires a type, the reference to the exemplified declaration"))
       return
     }
     val tp = dd.tpC.get.get
     val cu = CheckingUnit.byInference(None, Context(), tp)
     tp match {
       case OMID(definiendum) =>
         val checkResult = env.objectChecker.apply(cu, env.rules)(env.ce)
         if (!checkResult.solved) {
           env.errorCont(InvalidElement(dd, "invalid example type, check failed"))
           return
         }
       case _ =>
         env.errorCont(InvalidElement(dd, "invalid example type, expected reference to a declaration"))
         return
     }

     if (dd.getDeclarations.size == 0) {
        env.errorCont(InvalidElement(dd, "stex example requires at least one declaration"))
        return
     }
   }

   def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = new Elaboration {
     lazy val domain = {
       dd.getDeclarations.map {d =>
         dd.name / d.name
       }
     }

     def getO(n: LocalName): Option[Declaration] = {
       dd.module.getO(n.tail)
     }
   }
}

 */


