package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._

case class Stack(context: Context) {
   /** applies the same substitution to all contexts on this stack */
   def ^(subs: Substitution) = Stack(context ^? subs)
   def ++(con: Context) = Stack(context ++ con)
}

object Stack {
   def apply(t: MPath) : Stack = Stack(Context(t))
   val empty = Stack(Context())
}