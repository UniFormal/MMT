package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import utils._

/*
/**
 * A database of facts obtained through forward proof search
 * 
 * For efficiency, each instance only searches for terms that are added when the context is enriched.
 * Therefore, each [[Goal]] g maintains one instance of Facts, which links to the instance of the g.parent.
 * Each instance knows the local context of its goal, and maintains only terms that use a local variable.
 * 
 * @param parent g.parent.facts
 * @param newContext g.context
 */
class Facts(parent: Option[Facts], newContext: Context) extends Iterable[Term] {
   /** the database: maps every type to a set of terms of that type
    *
    * invariant: facts(tp) is not empty (i.e., non-empty or undefined)  
    */
   private val facts = new HashMapToSet[Term,Term]
   /** the facts added in the previous iteration */
   private var newFacts : List[(Term,Term)] = Nil
   /** the facts added in the current iteration */
   private var futureFacts : List[(Term,Term)] = Nil
   
   /**
    * initializes the database by adding the context to the facts, called in class initializer
    */
   private def initFacts {
      newContext foreach {case VarDecl(n, Some(tp), _,_) =>
         add(OMV(n), tp)
      }
      integrateFutureFacts
   }
   initFacts

   /** 
    *  adds a fact to the database
    *  @param tm a term that is valid over the full context of this goal
    *  @param tp its type
    *  
    *  the facts are not actually added immediately but queued for addition
    *  see integrateNewFacts 
    */
   def add(tm: Term, tp: Term) {
      println("adding " + tm + " : " + tp)
      futureFacts ::= (tm,tp)
   }
   /**
    * adds all queued facts to the database 
    */
   def integrateFutureFacts {
      futureFacts foreach {case (tm,tp) => facts(tp) += tm}
      newFacts = futureFacts
      futureFacts = Nil
   }
   /** all facts added in the previous iteration */
   def getNewFacts = newFacts
   /** an iterator over all types inhabited at this context (including those of the parent goal) */
   def iterator: Iterator[Term] = facts.keys.iterator ++ parent.map(_.iterator).getOrElse(Nil)
   /** the set of terms of a given type
    *  
    *  post: non-empty if iterator.contains(tp)
    */
   def termsOfType(tp: Term): List[Term] =
      facts.getOrEmpty(tp).toList ::: parent.map(_.termsOfType(tp)).getOrElse(Nil)
   
   override def toString = iterator.map(_.toString).mkString("\n") 
}*/

abstract class Shape
case class ComplexShape(op: GlobalName, children: List[Shape]) extends Shape
case class AtomicShape(term: Term) extends Shape
case class BoundShape(index: Int) extends Shape
case object Wildcard extends Shape

object Shape {
   def apply(queryVars: Context, context: Context, t: Term, level: Int): Shape = t match {
      case ComplexTerm(op, subs, cont, args) => {
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
      }
      case OMV(n) =>
         if (queryVars.isDeclared(n)) Wildcard
         else context.index(n) match {
            case None => AtomicShape(t)
            case Some(i) => BoundShape(i)
         } 
      case t => AtomicShape(t)
   }
   
   def matches(s: Shape, t: Shape): Boolean = (s,t) match {
      case (ComplexShape(op1, ch1), ComplexShape(op2, ch2)) =>
         op1 == op2 && (ch1 zip ch2).forall{case (x,y) => matches(x,y)}
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
case class Fact(goal: Goal, tm: Term, tp: Term)

/**
 * A database of facts obtained through forward proof search
 * 
 * For efficiency, each instance only searches for terms that are added when the context is enriched.
 * Therefore, each [[Goal]] g maintains one instance of Facts, which links to the instance of the g.parent.
 * Each instance knows the local context of its goal, and maintains only terms that use a local variable.
 * 
 * @param parent g.parent.facts
 * @param newContext g.context
 */
class FactsDB(shapeDepth: Int) {
   def log(msg: => String) {}
   
   private var symbolAtoms : List[(Term, Term)] = Nil
   private[proving] def addSymbolAtom(tm: Term, tp: Term) {
      symbolAtoms ::= ((tm, tp))
   }
   def getSymbolAtoms = symbolAtoms
   
   /** the database: maps every type to a set of terms of that type
    *
    * invariant: facts(tp) is not empty (i.e., non-empty or undefined)  
    */
   private val facts = new HashMapToSet[Shape,Fact]
   /** the facts added in the current iteration */
   private var futureFacts : List[Fact] = Nil
   
   /**
    *  adds a fact to the database
    *  @param tm a term that is valid over the full context of this goal
    *  @param tp its type
    *  
    *  the facts are not actually added immediately but queued for addition
    *  see integrateNewFacts 
    */
   def add(f: Fact) {
      log("adding " + f.tm + " : " + f.tp)
      futureFacts ::= f
   }
   /**
    * adds all queued facts to the database 
    */
   private[proving] def integrateFutureFacts {
      futureFacts foreach {f =>
         val sh = Shape(Nil, Nil, f.tp, shapeDepth)
         facts(sh) += f
      }
      futureFacts = Nil
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
      val matcher = new Matcher(queryFresh)
      val matches = matcher(f.goal.fullContext, f.tp, query)
      if (matches)
         Some((freshSub ^ matcher.getSolution, f.tm))
      else
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
         if (goal below f.goal) {
            matchFact(queryVars, query, f) foreach {x => res ::= x}
         }
      }
      res
   }
   
   /**
    * the set of facts whose type matches a given type and which are valid at a certain goal or some of its subgoals
    *  
    * retrieving facts at subgoals is useful for forward search: facts are only needed at leave nodes,
    * but computing them as high as possible avoids duplication of facts 
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
         if (goal below f.goal) {
            matchFact(queryVars, query, f) foreach {case (sub,t) => res ::= (sub,t,None)}
         } else if (f.goal below goal) {
            matchFact(queryVars, query, f) foreach {case (sub,t) => res ::= (sub,t,Some(f.goal))}
         }
      }
      res
   }
   
   def has(g: Goal, tp: Term): Option[Term] = {
      foreachFact(Nil, tp) {f =>
         if ((g below f.goal) && (f.tp hasheq tp)) return Some(f.tm)
      }
      None
   }
   
   override def toString = facts.toString 
}
