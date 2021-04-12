package info.kwarc.mmt.stex.features
/*
import info.kwarc.mmt.api._
import modules._
import objects._
import notations._
import symbols._
import utils._
import checking._

object PillarFeature extends StructuralFeature("Pillar") {
  /** default notation */
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none), Delim(":"), SimpArg(1))

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
     if (!dd.tpC.isDefined) {
       env.errorCont(InvalidElement(dd, "pilllar definition requires a type: references to the theories"))
       return
     }
     val tp = dd.tpC.get.get
     val cu = CheckingUnit.byInference(None, Context(), tp)
     tp match {
       case OMA(_, args) =>
         val checkResult = env.objectChecker.apply(cu, env.rules)(env.ce)
         if (!checkResult.solved) {
           env.errorCont(InvalidElement(dd, "invalid pillar type, check failed"))
           return
         }
         // checking that reference all point to modules
         val allMods = args.forall {
           case OMID(p : MPath) => true
           case _ => false
         }
         if (!allMods) {
           env.errorCont(InvalidElement(dd, "invalid pillar type, all pillar components must be modules"))
           return
         }

       case _ =>
         env.errorCont(InvalidElement(dd, "invalid pillar type, expected references to modules"))
         return
     }

     if (dd.getDeclarations.nonEmpty) {
        env.errorCont(InvalidElement(dd, "pillars should have no declarations, only type"))
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

   // assumes checked instance of pillar
   def getTheories(dd : DerivedDeclaration) : List[Theory] = {
     dd.tpC.get.get match {
       case OMA(_, args) =>
         var thys : List[Theory] = Nil
         args foreach {
           case OMID(p : MPath) =>
             controller.globalLookup.getModule(p) match {
               case dt : Theory => thys ::= dt
               case _ => //nothing to do
             }
           case _ => // nothing to do
         }
         thys
       case _ => Nil
     }

   }

}

object RealmFeature extends StructuralFeature("Realm") {
  /** default notation */
  def getHeaderNotation = List(LabelArg(1, LabelInfo.none), Delim(":"), SimpArg(1))

  def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment) {
    if (!dd.tpC.isDefined) {
      env.errorCont(InvalidElement(dd, "realm definition requires a type: references to the views between pillars"))
      return
    }
    val tp = dd.tpC.get.get
    val cu = CheckingUnit.byInference(None, Context(), tp)
    tp match {
      case OMA(_, args) =>
        val checkResult = env.objectChecker.apply(cu, env.rules)(env.ce)
        if (!checkResult.solved) {
          env.errorCont(InvalidElement(dd, "invalid realm type, object check failed"))
          return
        }
        // checking that reference all point to modules
        val allMods = args.forall {
          case OMID(p : MPath) => true
          case _ => false
        }
        if (!allMods) {
          env.errorCont(InvalidElement(dd, "invalid realm type, all realm components must be modules"))
          return
        }

      case _ =>
        env.errorCont(InvalidElement(dd, "invalid realm type, expected references to modules"))
        return
    }

     if (dd.getDeclarations.size == 0) {
        env.errorCont(InvalidElement(dd, "realms should have declarations: the pillars"))
        return
     }
     dd.getDeclarations.foreach {
       case s : DerivedDeclaration if s.feature == "Pillar" => // nothing to do
       case _ =>
          env.errorCont(InvalidElement(dd, "realms can only have Pillar instances as declarations"))
          return
     }
   }

   // assumes checked instance of realm
   def getViews(dd : DerivedDeclaration) : List[View] = {
     dd.tpC.get.get match {
       case OMA(_, args) =>
         var views : List[View] = Nil
         args foreach {
           case OMID(p : MPath) =>
             controller.globalLookup.getModule(p) match {
               case dv : View => views ::= dv
               case _ => //nothing to do
             }
           case _ => // nothing to do
         }
         views
       case _ => Nil
     }
   }

   def isEssential(symbol : GlobalName, views : List[View]) : Boolean = {
     views.exists(v => v.from.toMPath == symbol.module && v.isDeclared(symbol.name))
   }

   def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[uom.ExtendedSimplificationEnvironment] = None) = new Elaboration {

     lazy val pillars : Map[LocalName, DerivedDeclaration] = dd.getDeclarations.collect {
       case s : DerivedDeclaration if s.feature == "Pillar" => (s.name, s)
     }.toMap

     lazy val theories : Map[LocalName, List[Theory]] = {
       pillars.map(p => (p._1, PillarFeature.getTheories(p._2))).toMap
     }

     lazy val symbols : Map[LocalName, List[GlobalName]] = {
       theories map { p =>
         p._1 -> p._2.flatMap { t =>
           t.getDeclarations.map(d => d.path)
         }
       }
     }

     lazy val essentialSymbols : List[GlobalName] = {
       symbols.flatMap {ps => ps._2 flatMap { p =>
         if (isEssential(p, views)) {
           List(p)
         } else Nil
       }}.toList
     }

     lazy val views = getViews(dd)

     lazy val domain = {
       symbols.flatMap {p =>
         p._2 map { s =>
           if (essentialSymbols.contains(s)) {
             s.name
           } else {
             p._1 / s.name
           }
         }
       }.toList
     }

     def getO(n: LocalName): Option[Declaration] = {
       if (!theories.keys.toList.contains(LocalName(n.head))) { // essential symbol
         val thy = theories.values.flatten.find( t => t.isDeclared(n))
         thy.map(_.get(n))
       } else {
         val thy = theories(LocalName(n.head)).find( t => t.isDeclared(n.tail))
         thy.map(_.get(n.tail))
       }
     }
   }
}


 */