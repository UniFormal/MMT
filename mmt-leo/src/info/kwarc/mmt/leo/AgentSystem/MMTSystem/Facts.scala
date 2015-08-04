package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{Logger, Controller}
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.{ApplyGeneral, FunType}


import scala.collection.mutable




/** an approximation of the syntax tree of a [[Term]] that replaces subtrees beyond a certain depth with special leaves
 *  
 *  Shapes can be used as keys when indexing sets of terms, as in [[Facts]] 
 */
abstract class Shape
/** a non-replaced node in the syntax/shape tree,
 *  variable bindings are approximated by the shape of the type
 */
case class ComplexShape(op: GlobalName, children: List[Shape]) extends Shape
/** a leaf representing an atomic subterm (constant, variable, literal) */
case class AtomicShape(term: Term) extends Shape
/** a leaf representing a variable bound by a governing [[ComplexShape]] */
case class BoundShape(index: Int) extends Shape
/** a leaf representing a wild card used when matching terms against a certain shape */
case object Wildcard extends Shape

object Shape {
   /**
    * @param queryVars variables which yield shape [[Wildcard]]
    * @param context variables which yield [[BoundShape]]
    * @param t the term whose shape to compute
    * @param level the height of the shape's syntax tree
    * @return the approximation of the term that cuts all branches of the syntax tree at a certain depth 
    */
   def apply(queryVars: Context, context: Context, t: Term, level: Int): Shape = t match {
      case ComplexTerm(op, subs, cont, args) =>
        if (level == 0) return Wildcard
        var children: List[Shape] = Nil
        val subsSh = subs.foreach {s => children ::= Shape(queryVars, context, s.target, level-1)}
        val contSh = cont.mapVarDecls {case (sofar,vd) =>
           val t = vd.tp.getOrElse(OMV(vd.name))
           children ::= Shape(queryVars, context++sofar, t, level-1)
        }
        val argSh = args foreach {a =>
           children ::= Shape(queryVars, context++cont, a, level-1)
        }
        ComplexShape(op, children.reverse)
      case OMV(n) =>
         if (queryVars.isDeclared(n)) Wildcard
         else context.index(n) match {
            case None => AtomicShape(t)
            case Some(i) => BoundShape(i)
         } 
      case f => AtomicShape(f) //TODO Ask about shadowing warning
   }
   
   def matches(s: Shape, t: Shape): Boolean = (s, t) match {
     case (ComplexShape(op1, ch1), ComplexShape(op2, ch2)) =>
       op1 == op2 && (ch1 zip ch2).forall { case (x, y) => matches(x, y) }
     case (Wildcard, _) => true
     case (_, Wildcard) => true
     case _ => s == t
   }
}

/**
 * a fact is a sequent derived during forward search
 * @param goal antecedent of the fact (conclusion of the goal is irrelevant)
 * @param tm the proof term
 * @param tp the proved type
 */
case class Fact(goal: Goal, tm: Term, tp: Term) {
   override def toString = tp.toString + "\n     " + tm.toString
   def present(presentObj: Obj => String) = { 
      presentObj(tp) + " by " + presentObj(tm)
   }
}

/**
 * an atomic fact: a constant or a variable
 * @param tm the proof term
 * @param tp the proved type
 * @param rl the of the constant/variable
 */
case class Atom(tm: Term, tp: Term, rl: Option[String]) {
   def isConstant = tm.isInstanceOf[OMID]
   def isVariable = tm.isInstanceOf[OMV]
}

/*

class TransitiveClosureSet{
  type Operation = Int

  var g = Graph[Term,LDiEdge[Operation]]()

  def compare(a:Term,b:Term,order:Boolean=false):Option[Boolean] = {
    if (transClosureOf(a).contains(b)) return Some(true)
    if (order && transClosureOf(b).contains(a)) return Some(false)
    None
  }

  def transClosureOf(t1:Term):Set[Term] = getOtherTerms(t1)+t1

  def getOtherTerms(t1:Term):Set[Term] = {
    g find t1 match {
      case Some(g1) => g1.diSuccessors.asInstanceOf[Set[Term]]
        //.LDiSuccessors.asInstanceOf[Set[Term]]
      case None => Set.empty[Term]
    }
  }

  def add(t1:Term,t2:Term,op:Operation) = {g += LDiEdge(t1,t2)(op)}
}
*/

case class TermEntry(goal: Goal, tm: Term, tp: Term) {
  override def toString = tp.toString + "\n     " + tm.toString
  def present(presentObj: Obj => String) = {
    presentObj(tp) + " by " + presentObj(tm)
  }
}

class Terms(blackboard: MMTBlackboard)(implicit controller: Controller,oLP:String) extends Logger {

  val report =  controller.report
  def logPrefix = oLP + "#Terms"

  /** create a lookup for constant atom terms based on their type
    * Fisrt MMT[[Term]]=>LF type, Second MMT[[TermEntry]]=> LF term marked with goal
    * */
  private val terms = new HashMapToSet[Term,TermEntry]

  def initializeTerms(facts:Facts) = {
    facts.getConstantAtoms.foreach(addTerm(_,blackboard.goal))
    addVarAtoms(blackboard.goal)
  }
  //TODO add variable atoms as well

  def addVarAtoms(g:Goal) = g.varAtoms.foreach(addTerm(_,blackboard.goal))

  def addTerm(termEntry:TermEntry):Unit = {terms(termEntry.tp) += termEntry; log("Added Term:"+ termEntry) }
  def addTerm(a: Atom,g:Goal):Unit  = this addTerm TermEntry(g,a.tm,a.tp)
  def +=(termEntry:TermEntry):Unit  = addTerm(termEntry)
  def ++=(termEntries:List[TermEntry]):Unit  = termEntries.foreach(addTerm)
  def +=(a: Atom,g:Goal):Unit  = addTerm(a: Atom,g:Goal)

  /**Get all terms of a type */
  def getTermsOfType(tp:Term): mutable.HashSet[Term] = terms(tp).map(_.tm)
  /**Get all terms of a type above a specified goal*/
  def getTermsOfType(tp:Term,g:Goal): mutable.HashSet[Term] = terms(tp).filter(_.goal.isAbove(g)).map(_.tm)

}

/**
 * A database of facts obtained through forward proof search
 * 
 * For efficiency, each instance only searches for terms that are added when the context is enriched.
 * Therefore, each [[Goal]] g maintains one instance of Facts, which links to the instance of the g.parent.
 * Each instance knows the local context of its goal, and maintains only terms that use a local variable.
 * 
 * @param shapeDepth is the depth of the shape representation used to apprximate facts
 */
class Facts(blackboard: MMTBlackboard, shapeDepth: Int)(implicit c: Controller,oLP:String) extends Logger {
   val report =  c.report
   def logPrefix = oLP+"#Facts"

    def getFunctionalFacts:List[Fact] = Nil //TODO implement this

   private var constantAtoms : List[Atom] = Nil
   private[leo] def addConstantAtom(a: Atom) {
      constantAtoms ::= a
   }
   def getConstantAtoms = constantAtoms

   /**
    * the database of (non-atomic) facts, indexed by the shape of the type
    */
   private val facts = new HashMapToSet[Shape,Fact]
   /** the facts added in the current iteration */
   private var futureFacts : List[Fact] = Nil
   
   /**
    *  adds a fact to the database. The facts are not actually
    *  added immediately but queued for addition
    *  see integrateFutureFacts 
    *  
    *  facts are ignored if their proof does not use a free variable
    */
   def add(f: Fact) {
      if (f.tm.freeVars.nonEmpty)
         futureFacts ::= f
   }

  /** simplify a fact */
  private[leo] def simplifyFact(f: Fact): Fact = {
    val tpS = c.simplifier(f.tp, f.goal.fullContext, blackboard.rules)
    f.copy(tp = tpS)
  }


  /**
    * adds all queued facts to the database 
    */
  private[leo] def integrateFutureFacts(section:Option[FactSection]) ={
    val out = futureFacts.nonEmpty
    futureFacts foreach {f =>
       val fS = simplifyFact(f)
       log("new fact: " + fS.present(blackboard.presentObj))
       val sh = Shape(Nil, Nil, fS.tp, shapeDepth)
       facts(sh) += fS
    }
    futureFacts = Nil
    if (out && section.isDefined) {section.get.passiveAdd()}
  }
   
   /**
    * applies a function to (at least) all facts that match a query
    * @param queryVars those free variables of query to instantiate when matching
    * @param query the term to match against all facts
    * @param fun the function to apply
    * 
    * only approximate matching based on shapes is performed; fun must still perform a precise match
    */
   private def foreachFact(queryVars: Context, query: Term)(fun: Fact => Unit) {
      val querySh = Shape(queryVars, Nil, query, shapeDepth)
      facts.keys foreach {sh =>
         if (Shape.matches(querySh, sh)) {
            facts(sh) foreach {f =>
               if (f.goal.isFinished)
                  facts(sh) -= f // forget facts about finished goals
               else
                  fun(f)
            }
         }
      }
   }
   
   /**
    * matches a facts against a query
    * @param queryVars those free variables of query to instantiate when matching
    * @param query the term to match against the fact
    * @param f the fact to match against
    * @return the pair (s: queryVars -> f.goal.fullContext, t) such that query ^ s = fact.tp, if possible
    * 
    * s is partial if queryVars contains variables that do not occur in query
    */
   private def matchFact(queryVars: Context, query: Term, f: Fact): Option[(Substitution,Term)] = {
      val (queryFresh, freshSub) = Context.makeFresh(queryVars, f.goal.fullContext.map(_.name)) 
      val matcher = blackboard.makeMatcher(f.goal.fullContext, queryFresh)
      val matches = matcher(f.tp, query)
      if (matches) {
         val solution = matcher.getSolution
         // we need freshSub ^ solution but restricted to those variables that were solved
         val freshSubRestrict = freshSub.filter {case Sub(_, OMV(qF)) => solution.maps(qF)}
         Some((freshSubRestrict ^ solution, f.tm))
      } else
         None
   }
   
   /**
    * the set of facts whose type matches a given type and which are valid at a certain goal
    *  
    *  @param goal goal where facts must be valid
    *  @param queryVars free variables in the needed type that are to be matched
    *  @param query the needed type
    *  @return the pairs (sub, t) such that Fact(goal, tm, query ^ sub)
    *  
    *  If not all queryVars occur in query, then sub will be partial.  
    */
   def termsOfTypeAtGoal(goal: Goal, queryVars: Context, query: Term): List[(Substitution, Term)] = {
      var res: List[(Substitution, Term)] = Nil
      foreachFact(queryVars, query) {case f =>
         if (goal isBelow f.goal) {
            matchFact(queryVars, query, f) foreach {x => res ::= x}
         }
      }
      res
   }
   
   /**
    * the set of terms of the type required by the goal 
    */
   def solutionsOfGoal(goal: Goal): List[Term] = {
      var res: List[Term] = Nil
      foreachFact(Nil, goal.conc) {case f =>
         matchFact(Nil, goal.conc, f) foreach {case (_,x) => res ::= x}
      }
      res
   }
   
   /**
    * the set of facts whose type matches a given type and which are valid at a certain goal or some of its subgoals
    *  
    * retrieving facts at subgoals is useful for forward search: facts are only needed at leave nodes,
    * but computing them as high as possible avoids duplication of facts //TODO How?
    *  
    *  @param goal goal where facts must be valid
    *  @param queryVars free variables in the needed type that are to be matched
    *  @param query the needed type
    *  @return the triples (sub, t, g) such that Fact(g, tm, query ^ sub) and (g below goal); g is None if (goal below g) 
    *  
    *  If not all queryVars occur in query, then sub will be partial.  
    */
   def termsOfTypeBelowGoal(goal: Goal, queryVars: Context, query: Term): List[(Substitution, Term, Option[Goal])] = {
      var res: List[(Substitution, Term, Option[Goal])] = Nil
      foreachFact(queryVars, query) {case f =>
         if (goal isBelow f.goal) {
            matchFact(queryVars, query, f) foreach {case (sub,t) => res ::= (sub,t,None)}
         } else if (f.goal isBelow goal) {
            matchFact(queryVars, query, f) foreach {case (sub,t) => res ::= (sub,t,Some(f.goal))}
         }
      }
      res
   }
   
   def has(g: Goal, tp: Term): Option[Term] = {
      foreachFact(Nil, tp) {f =>
         if ((g isBelow f.goal) && (f.tp hasheq tp)) return Some(f.tm)
      }
      None
   }
   
   override def toString = {
      facts.toString
   }
}





