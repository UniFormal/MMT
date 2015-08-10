package info.kwarc.mmt.api

import frontend._
import objects._
import modules._
import symbols._
import scala.collection.mutable.{HashMap,HashSet}

/** the type of all Rules
 * 
 * All Rules have an apply method that is passed a Solver for callbacks.
 * The Solver does not implement any back-tracking. Therefore, rules may only use callbacks if their effects are required.
 */
trait Rule {
   /** an MMT URI that is used to indicate when the Rule is applicable */
   def head: GlobalName
   override def toString = {
      var name = getClass.getName
      if (name.endsWith("$"))
         name = name.substring(0,name.length-1)
      "rule " + name + " for " + head
   }
}

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
   def getByHead[R<:Rule](cls: Class[R], head: ContentPath): HashSet[R] = get(cls) filter {r => r.head == head}
   def getFirst[R<:Rule](cls: Class[R], head: ContentPath): Option[R] = getByHead(cls, head).headOption
   
   override def toString = rules.toList.map(_.toString).mkString(", ")

}

object RuleSet {
   def collectRules(controller: Controller, context: Context): RuleSet = {
      val imports = controller.library.visibleDirect(ComplexTheory(context))
      val rs = new RuleSet
      imports.foreach {
         case OMPMOD(p,_) =>
            controller.globalLookup.getO(p) match {
               case Some(t:DeclaredTheory) =>
                  // trying all declarations because some rules might be generated
                  t.getDeclarations.foreach {
                     case rc: RuleConstant =>
                           rs.declares(rc.df)
                     case _ => Nil
                  }
               case _ => Nil
            }
         case _ => Nil
      }
      rs
   }
}
