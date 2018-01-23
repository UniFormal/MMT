package info.kwarc.mmt.api.proving


import info.kwarc.mmt.api._
import objects._
import utils._

/** an approximation of the syntax tree of a [[Term]] that replaces subtrees beyond a certain depth with special leaves
 *
 *  Shapes can be used as keys when indexing sets of terms, as in [[Facts]]
 */
abstract class Shape

/** a non-replaced node in the syntax/shape tree,
 *  variable bindings are approximated by the shape of the type
 */
case class ComplexShape(op: GlobalName, children: List[Shape]) extends Shape
/** a leaf representing an atomic subterm (constant, outer variable, literal) */
case class AtomicShape(term: Term) extends Shape
/** a leaf representing a variable bound by a governing [[ComplexShape]] */
case class BoundShape(index: Int) extends Shape
/** a leaf representing a wild card used when matching terms against a certain shape */
case object Wildcard extends Shape

object Shape {
   /**
    * computes the shape of a term
    * @param queryVars variables which yield shape [[Wildcard]]
    * @param context variables which yield [[BoundShape]]
    * @param t the term whose shape to compute
    * @param level the height of the shape's syntax tree
    * @return the shape that cuts all branches of the syntax tree at the given level
    */
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

   /** convenience for empty contexts; returns shapes without any wildcards */
   def apply(t: Term, levels: Int): Shape = apply(Context.empty, Context.empty, t, levels)

   /** true if two shapes may represent the same term */
   def matches(s: Shape, t: Shape): Boolean = ((s,t) match {
      case (ComplexShape(op1, ch1), ComplexShape(op2, ch2)) =>
         op1 == op2 && (ch1 zip ch2).forall{case (x,y) => matches(x,y)}
      case (Wildcard, _) => true
      case (_, Wildcard) => true
      case _ => s == t
   })
}

/** a hash set using shapes of a certain height as keys */
class ShapeIndexedSet[A](levels: Int, key: A => Term) extends ComplexHashSet[Shape, A](a => Shape(key(a), levels))
