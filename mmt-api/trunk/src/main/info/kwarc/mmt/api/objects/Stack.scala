package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._

/** A pair (theory, context) used by rules
 * @param theory the theory
 * @param context the context
 */
case class Frame(theory : Term, context : Context) {
   def ^(subs: Substitution) = Frame(theory, context ^ subs)
}

case class Stack(frames: List[Frame]) {
   def pop = Stack(frames.tail)
   def push(f: Frame) = Stack(f::frames)
   def theory = frames.head.theory
   def context = frames.head.context
   /** applies the same substitution to all contexts on this stack
    */
   def ^(subs: Substitution) = Stack(frames.map(_ ^ subs))
   def ++(con: Context) = Stack(Frame(theory, context ++ con) :: frames.tail)
}

object Stack {
   def apply(f: Frame) : Stack = Stack(List(f))
   def apply(t: MPath) : Stack = empty(OMMOD(t))
   def apply(t: Term, c: Context) : Stack = Stack(Frame(t, c))
   def apply(t: MPath, c: Context) : Stack = Stack(OMMOD(t), c)
   def empty(t: Term) : Stack = Stack(t, Context())
}

