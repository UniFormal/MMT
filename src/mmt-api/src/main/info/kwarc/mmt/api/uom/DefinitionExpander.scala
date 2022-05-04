package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.Killable
import symbols._
import objects._
import objects.Conversions._

/**
 * traverses the syntax tree (depth first, argument order) and expands (only) the first defined constant it encounters
 *
 * does not expand in contexts and scopes at the moment
 */
class DefinitionExpander(controller: frontend.Controller) extends StatelessTraverser {
   private def expSym(p: GlobalName)(implicit con : Context): Option[Term] =
      controller.library.getO(ComplexTheory(con),LocalName(p.module) / p.name) match {
       // TODO make sure lookup goes through morphisms into the context here
      case Some(c: Constant) => c.dfC.getAnalyzedIfFullyChecked
      case _ => None
   }
   def traverse(t: Term)(implicit con : Context, init: Unit): Term = {
      t match {
         case DefinitionsExpanded(tE) if tE.freeVars.forall(v => con(v).df.isEmpty) =>
            // term was already expanded previously and none of its free variables has acquired a definition since then
            tE
         case ComplexTerm(p, args, cont, scopes) =>
            args.zipWithIndex foreach {case (Sub(l,a),i) =>
               val aE = traverse(a)
               if (aE hashneq a)
                  return ComplexTerm(p, args.take(i) ::: Sub(l,aE) :: args.drop(i+1), cont, scopes).from(t)
            }
            scopes.zipWithIndex foreach {case (s,i) =>
               val sE = traverse(s)(con ++ cont, init)
               if (sE hashneq s)
                  return ComplexTerm(p, args, cont, scopes.take(i) ::: sE :: scopes.drop(i+1))
            }
            DefinitionsExpanded(t)
         case o: OML => o // TODO traversal blocked because it would not recurse with the correct scope
         case OMS(p) => expSym(p) match {
            case Some(tE) => tE.from(t)
            case None => DefinitionsExpanded(t)
         }
         case OMV(n) => con(n).df match {
            case Some(tE) =>
              //TODO variable definitions that contain shadowed variables may not be expanded
              tE.from(t)
            case None => DefinitionsExpanded(OMV(n))
         }
         case _ => Traverser(this, t)
      }
   }
}

/**
 * UOM uses this to remember that all definitions inside a Term have been expanded to avoid recursing into it again
 */
object DefinitionsExpanded extends BooleanTermProperty(utils.mmt.baseURI / "clientProperties" / "uom" / "expanded")
