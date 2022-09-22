package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import objects._


class Stack {
   private var frames = List(Context.empty)

   def top = frames.head

   def setTop(con: Context): Unit = {
     frames = con :: frames.tail
   }

   def newVariable(vd: VarDecl): Unit = {
      setTop(top ++ vd)
   }
   def assign(n: LocalName, value: Term): Unit = {
      // currently implemented using mutation: maybe change in the future
      val vd = top(n)
      val vdN = vd.copy(df = Some(value))
      val topNew = top.before(n) ::: vdN :: top.after(n)
      frames = topNew :: frames.tail
   }
   def removeVariables(n: Int): Unit = {
      setTop(top.take(top.length - n))
   }

   def push: Unit = {
     frames ::= Context.empty
   }
   def pop: Unit = {
     if (frames.isEmpty)
       throw ImplementationError("empty stack")
     frames = frames.tail
   }
}
