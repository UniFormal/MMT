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
   def providedRules = List(this)
   /** normally the empty list; but rules may list rules here that should be removed from the context
    *  this can be used to override imported rules
    */
   def shadowedRules: List[Rule] = Nil

 /** when multiple rules are applicable, rules with higher priorities are preferred
   *
   *  creating a new rule with higher priority can be used to effectively drop imported rules
   *
   */
   // TODO priority is used only in some situations so far, in particular for lexer extensions, type inference, or term-transformation
  def priority: Int = 0
}

object Rule {
  def loadRule(controller: Controller, homeO: Option[Term], rl: Term) = {
    val (rlP,rlArgs) = rl match {
        case OMPMOD(p,as) => (p,as)
        case _ => throw InvalidObject(rl, "cannot interpret as semantic object: " + rl)
    }
    controller.extman.addExtensionO(SemanticObject.mmtToJava(rlP, true), Nil) match {
        case Some(sf: StructuralFeature) =>
          if (rlArgs.nonEmpty) throw InvalidObject(rl, "too many arguments")
          sf.getRule
        case Some(_) => throw InvalidObject(rl, "extension exists but does not provide a rule: " + rl)
        case None =>
          val so = controller.backend.loadObjectO(rlP).getOrElse {
            throw InvalidObject(rl, "semantic object not found")
          }
          so match {
            case r: Rule =>
              if (rlArgs.nonEmpty) throw InvalidObject(rl, "too many arguments")
              r
            case r: ParametricRule =>
              val home = homeO getOrElse {
                throw ImplementationError("rule is parametric, but not home theory provided")
              }
              try {
                r(controller, home, rlArgs)
              } catch {case e: Exception =>
                throw InvalidObject(rl, "error while instantiating parametric rule").setCausedBy(e)
              }
            case _ => throw InvalidObject(rl, "semantic object exists but is not a rule: " + rl)
          }
      }
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
      "rule " + mpath.name + " for " + head.name //strangely, super.toString does not work here (Scala bug?)
   }
}

/** A RuleSet groups some Rule's. */
abstract class RuleSet extends Rule {self =>
  /** by making this a rule, MMT theories can add entire rule sets at once */
  override def providedRules: List[Rule] = super.providedRules ::: getAll.toList
  
  /** the underlying set of rules */
  def getAll: Iterable[Rule]

  /** get all rules of a certain type */
  def get[R<:Rule](cls: Class[R]): Iterable[R] = getAll flatMap {r =>
    if (cls.isInstance(r))
       List(r.asInstanceOf[R])
    else
       Nil
  }
  /** like get but ordered by descending priority */
  def getOrdered[R<:Rule](cls: Class[R]) = get(cls).toList.sortBy(r => - r.priority)

  /** get all rules a certain type with a certain head */
  def getByHead[R<:checking.CheckingRule](cls: Class[R], head: ContentPath): Iterable[R] =
    get(cls) filter {r => r.head == head || (r.alternativeHeads contains head)}

  /** get the first rule of a certain type with a certain head */
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

   def add(r : Rule*) = r.foreach(rules += _)

   /* Its construction and use corresponds to algebraic theories. */
   def declares(rs: Rule*): Unit = {rs foreach {rules += _}}
   def imports(rss: RuleSet*): Unit = {rss foreach {rules ++= _.getAll}}
   def shadow(rs: Rule*): Unit = {rs foreach {rules -= _}}

   def getAll = rules
}

object RuleSet {
  /**
    * Creates a [[RuleSet]] with the given rules.
    */
  def apply(rules: Rule*): RuleSet = {
    val rs = new MutableRuleSet
    rs.add(rules : _*)
    rs
  }

   /** collects all rules visible to a context */
  def collectRules(controller: Controller,context: Context): MutableRuleSet = {
    collectAdditionalRules(controller,None,context)
  }

  /**
    * incremental collection of rules: collectAdditionalRules(c, collectRules(c, con1), con2) = collectRules(c, con1++con2)
    */
  def collectAdditionalRules(controller: Controller,addTo: Option[RuleSet],context: Context): MutableRuleSet = {
    val support = context.getIncludes
    var shadowed: List[Rule] = Nil
    val rs = new MutableRuleSet
    addTo.foreach {a => rs.imports(a)}
    support.foreach {p =>
      //TODO this takes around 20% of processing time when reading mmt files - investigate if it is due to this method or due to some lower method (e.g., reading OMDoc files)
      controller.globalLookup.forDeclarationsInScope(OMMOD(p)) {case (p,m,d) => d match {
        case rc: RuleConstant =>
          rc.df foreach { r =>
            rs.declares(r.providedRules: _*)
            shadowed :::= r.shadowedRules
          }
        case _ =>
      }
      }
    }
     /* not faster:
      Theory.flatIterator(support, controller.globalLookup).foreach {
        case (rc: RuleConstant,None) =>
          rc.df foreach { r =>
            rs.declares(r.providedRules: _*)
            shadowed :::= r.shadowedRules
          }
        case _ =>
      }*/
      rs.shadow(shadowed:_*)
      rs
   }
}
