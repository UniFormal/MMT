package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import objects._

import java.lang.ref.WeakReference

import Theory._

class Heap(id: Int, controller: Controller) extends Theory(utils.mmt.mmtbase / "heap", LocalName(id.toString), noMeta, noParams, noBase) {
   private var instanceId = 0

   def newInstance(context: Context, ofTerm: Term): Instance = {
      val of = controller.simplifier.materialize(context, ofTerm, None, None).asInstanceOf[Theory]
      val i = new Instance(path, of.path, instanceId)
      instanceId += 1

      controller.simplifier(of)
      of.getDeclarations.foreach {
         case c: Constant if c.rl contains "state" =>
           val cI = Constant(i.toTerm, ComplexStep(of.path) / c.name, Nil, None, c.df, None)
           i.add(cI)
         case _ =>
      }

      add(i)
      i
   }

   def getInstance(n: LocalName): Instance = {
     get(n) match {
       case i: Instance => i
       case _ => throw ExecutionError("not an instance " + n)
     }
   }
}
