package info.kwarc.mmt.api

import frontend._
import objects._
import modules._
import symbols._
import uom._
import backend._
import scala.collection.mutable.{HashMap,HashSet}

/** the type of all Rules
 * 
 * All Rules have an apply method that is passed a Solver for callbacks.
 * The Solver does not implement any back-tracking. Therefore, rules may only use callbacks if their effects are required.
 */
trait Rule extends SemanticObject {
   override def toString = {
      "rule " + mpath
   }
}

/** parametric rules can be instantiated to obtain rules */ 
abstract class ParametricRule extends SemanticObject {
  /**
   * @param home the containing theory
   * @param args the parameters of the rule
   */
  def apply(controller: Controller, home: Term, args: List[Term]): Rule
}

/** a rule for syntax-driven algorithms, applicable to expressions with a certain head */
trait SyntaxDrivenRule extends Rule {
   /** an MMT URI that is used to indicate when the Rule is applicable */
   def head: GlobalName
   override def toString = {
      "rule " + mpath + " for " + head
   }
}

/** the [[SemanticType]] of all [[Rule]]s */
class RuleType(be: Backend) extends Atomic[Rule] {
   val cls = classOf[Rule]
   override def toString(u: Any) = unapply(u).get.mpath.toString

   def fromString(s: String): Rule = {
     try {
       val mp = Path.parseM(s, NamespaceMap.empty)
       be.loadObject(mp) match {
         case r: Rule => r
         case _ => throw ParseError("object exists but is not a rule")
       }
     } catch {case NotApplicable(msg) =>
       throw ParseError(msg)
     }
   }
   /** scala"QUALIFIED-CLASS-NAME" */
   override def lex = quotedLiteral("scala")
}

/**
 * [[Rule]]s as literals (to be used as definies of constants that represent rules) 
 */
class RuleLiterals(be: backend.Backend) extends RepresentedRealizedType[Rule](OMS(utils.mmt.mmtcd ? "rule"), new RuleType(be))

/** A RuleSet groups some Rule's. Its construction and use corresponds to algebraic theories. */
class RuleSet {
   private val rules = new HashSet[Rule]

   def declares(rs: Rule*) {rs foreach {rules += _}}
   def imports(rss: RuleSet*) {rss foreach {rules ++= _.rules}}
   
   def getAll = rules
   def get[R<:Rule](cls: Class[R]): HashSet[R] = rules flatMap {r =>
      if (cls.isInstance(r))
         List(r.asInstanceOf[R])
      else
         Nil
   }
   def getByHead[R<:checking.CheckingRule](cls: Class[R], head: ContentPath): HashSet[R] = get(cls) filter {r => r.head == head || (r.alternativeHeads contains head)}
   def getFirst[R<:checking.CheckingRule](cls: Class[R], head: ContentPath): Option[R] = getByHead(cls, head).headOption
   
   override def toString = rules.toList.map(_.toString).mkString(", ")

}

object RuleSet {
   def collectRules(controller: Controller, context: Context): RuleSet = {
      val imports = controller.library.visibleDirect(ComplexTheory(context))
      //TODO once rules are translatable, this should handle translations between rules
      //TODO even if rules are not translatable, the morphism bringing a theory into may happen to be the identity for some rule, which can then be collected here
      val rs = new RuleSet
      imports.foreach {
         case OMPMOD(p,_) =>
            controller.globalLookup.getO(p) match {
               case Some(t:DeclaredTheory) =>
                  // trying all declarations because some rules might be generated
                  t.getDeclarations.foreach {
                     case rc: RuleConstant =>
                        rc.df foreach {r => rs.declares(r)}
                     case _ =>
                  }
               case _ =>
            }
         case _ =>
      }
      rs
   }
}
