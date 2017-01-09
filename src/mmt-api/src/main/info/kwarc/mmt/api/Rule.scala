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
      "rule " + mpath.name
   }
   /** normally the singleton list of this rule; but rules may bundle additional rules as well */ 
   def getRules = List(this)
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
      "rule " + mpath.name + " for " + head.name //strangely, super.toString does not work here (Scala bug?)
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

/** A RuleSet groups some Rule's. */
abstract class RuleSet {self =>
   /** the underlying set of rules */
   def getAll: Iterable[Rule]
   
   def get[R<:Rule](cls: Class[R]): Iterable[R] = getAll flatMap {r =>
      if (cls.isInstance(r))
         List(r.asInstanceOf[R])
      else
         Nil
   }
   def getByHead[R<:checking.CheckingRule](cls: Class[R], head: ContentPath): Iterable[R] =
     get(cls) filter {r => r.head == head || (r.alternativeHeads contains head)}
   def getFirst[R<:checking.CheckingRule](cls: Class[R], head: ContentPath): Option[R] =
     getByHead(cls, head).headOption
   
   override def toString = getAll.toList.map(_.toString).mkString(", ")
   
   /** filters this set by a predicate */
   def filter(include: Rule => Boolean) = new RuleSet {
     def getAll = self.getAll.filter(include)
   }
}

/** standard implementation of using set of rules hashed by their head */
class MutableRuleSet extends RuleSet {
   private val rules = new HashSet[Rule]

   /* Its construction and use corresponds to algebraic theories. */ 
   def declares(rs: Rule*) {rs foreach {rules += _}}
   def imports(rss: RuleSet*) {rss foreach {rules ++= _.getAll}}

   def getAll = rules
}

object RuleSet {
   /** collects all rules visible to a context, based on what is currently loaded into memory */
   def collectRules(controller: Controller, context: Context): RuleSet = {
      val support = context.getIncludes
      val decls = support.flatMap {p =>
        controller.globalLookup.getDeclarationsInScope(OMMOD(p))
      }.distinct
      val rs = new MutableRuleSet
      val rci = new RuleConstantInterpreter(controller)
      decls.foreach {
         case rc: RuleConstant =>
            rc.df foreach {r => rs.declares(r.getRules :_*)}
         case _ =>
      }
      rs
   }
}