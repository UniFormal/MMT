package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import objects._
import symbols._
import frontend._
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.Obj.getConstants
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.mutable

class RuntimeEnvironment(val heap: Heap, val stack: execution.Stack, val rules: RuleSet) {
  val stdin = System.in
  val stdout = System.out
  lazy val execRules = rules.get(classOf[ExecutionRule])
}

class RuleBasedExecutor() extends Executor {
  def apply(theory: Theory,context: Context, prog: Term) = {
     val heap = new Heap(0, controller)
     controller.add(heap)
     val stack = new execution.Stack
     val rules = RuleSet.collectRules(controller, context)
     val env = new RuntimeEnvironment(heap, stack, rules)
     // val definedas = gather_definitions(theory,context, rules)
     // print(definedas)
     val runtime = new Runtime(controller, env, logPrefix)
     log("executing " + prog + " with rules " + env.execRules.map(_.toString).mkString(", "))
     runtime.execute(prog)
  }
  def traverseTree(th:MPath,includedSet:mutable.Set[MPath],constants: mutable.Map[String,Constant]): Unit = {
    // TODO: change to tree structure
    if(includedSet.contains(th))
      return
    else
      includedSet.add(th)
    controller.get(th) match {
      case t:Theory =>
        for (incl <- t.getIncludes) traverseTree(incl,includedSet,constants)
        for (const <- t.getConstants) constants.put(const.name.toString,const)
    }

  }
  def gather_definitions(theory:Theory,con : Context, rules: MutableRuleSet) = {
    controller.simplifier(theory)
    val paths  = theory.getAllIncludes
    val tmp = paths map {q =>  controller.getTheory(q.from)
    }
    val constants: mutable.Map[String,Constant] = mutable.Map()
    traverseTree(theory.toTerm.toMPath,mutable.Set(),constants)
    val q  = constants map {case (p,x)=>x}
    // Todo: gather all Preprocessing Rules from the environment
    val buildRules = rules.get(classOf[RulePreprocessor])
    // here we now match against the _type_ not the term under the type!
    // the idea is to use these rules for the more static type information, rather than the code itsel
    // though MMT kind of blurs the line here
    val definedRules = q flatMap {x=>x match {
      case term => term.tp match {case Some(tp : Term) => buildRules.filter(_.applicable(tp)) map {r: RulePreprocessor=> r(term)}}
    }
    }
    print(definedRules)
    print(paths)
  }
}



class Runtime(controller: Controller, env: RuntimeEnvironment, val logPrefix: String) extends ExecutionCallback with Logger {
   val report = controller.report

   def execute(progOrg: Term): Term = {
      val context = env.stack.top
      val prog = controller.simplifier(progOrg, SimplificationUnit(context, true,false, true))
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
            return r(controller,this, env, t)
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
