package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import objects._
import symbols._
import frontend._

class RuntimeEnvironment(val heap: Heap, val stack: Stack, val rules: RuleSet) {
  val stdin = System.in
  val stdout = System.out
  lazy val execRules = rules.get(classOf[ExecutionRule])
}

class RuleBasedExecutor extends Executor {
  def apply(context: Context, prog: Term) = {
     val heap = new Heap(0, controller)
     controller.add(heap)
     val stack = new Stack
     val rules = RuleSet.collectRules(controller, context)
     val env = new RuntimeEnvironment(heap, stack, rules)
     val runtime = new Runtime(controller, env, logPrefix)
     log("executing " + prog + " with rules " + env.execRules.map(_.toString).mkString(", "))
     runtime.execute(prog)
  }
}

class Runtime(controller: Controller, env: RuntimeEnvironment, val logPrefix: String) extends ExecutionCallback with Logger {
   val report = controller.report

   def execute(progOrg: Term): Term = {
      val context = env.stack.top
      val prog = controller.simplifier(progOrg, context)
      prog match {
        // foundation-independent cases
        case l: OMLITTrait =>
          l
        case OMV(n) =>
          val vd = context(n)
          vd.df match {
             case Some(df) =>
               execute(df)
             case None =>
               prog
          }
        case OMMOD(p) =>
          prog
        case OMS(p) =>
          val d = controller.globalLookup.getO(p).getOrElse {
            throw ExecutionError("name does not exists: " + p)
          }
          d match {
            case c: Constant => c.df match {
               case Some(df) => execute(df)
               case None => prog
            }
            case d =>
              prog
          }
        // apply foundation-specific rule
        case t =>
          val rules = env.execRules.filter(_.applicable(t))
          rules foreach {r =>
            log("trying to apply rule " + r.toString)
            return r(this, env, t)
          }
          log("no applicable rule")
          executeChildren(prog)
      }
   }

   private def executeChildren(prog: Term): Term = {
     prog match {
       case ComplexTerm(p, subs, cont, args) =>
         val argsE = args map execute
         ComplexTerm(p, subs, cont, argsE)
       case t =>
         throw ExecutionError("cannot execute: " + t)
     }
   }
}
