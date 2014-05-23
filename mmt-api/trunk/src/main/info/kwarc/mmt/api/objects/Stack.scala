package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._

case class Stack(theory: Term, context: Context) {
   /** applies the same substitution to all contexts on this stack */
   def ^(subs: Substitution) = Stack(theory, context ^? subs)
   def ++(con: Context) = Stack(theory, context ++ con)
}

object Stack {
   def apply(t: MPath) : Stack = Stack(OMMOD(t), Context())
}

