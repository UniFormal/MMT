package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import objects._
import symbols._
import frontend._
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.immutable.List
import scala.collection.mutable
import scala.util.Sorting

class RuntimeEnvironment(val stack: execution.Stack, val rules: RuleSet) {
  val stdin = System.in
  val stdout = System.out
  lazy val execRules = rules.getOrdered(classOf[ExecutionRule])
  private var instanceCounter = -1
  def nextInstance = {instanceCounter += 1; instanceCounter}
}

class RuleBasedExecutor() extends Executor {
  def apply(theory: Theory,context: Context, prog: Term) = {
     // we don't want to deal with an explicit heap anymore
     // instead we embedd the objects onto the JVM heap
    val stack = new execution.Stack
     stack.setTop(context)
     val tmp = filterRules(RuleSet.collectRules(controller, context))
     val rules = RuleSet(Sorting.stableSort(tmp.providedRules, (x :Rule) => x.priority) :_*)
     //val mutableRules = new MutableRuleSet()
     //mutableRules.add(rules)

     val defined_rules = gather_definitions(theory,context, rules)
     val env = new RuntimeEnvironment(stack, rules)

    // print(definedas)
     val runtime = new Runtime(controller, env,defined_rules, logPrefix)
     log("executing " + prog + " with rules " + env.execRules.map(_.toString).mkString(", "))
     // val sol = Solver.infer(controller,context, prog, Some(rules))
     runtime.execute(prog)
  }
  def filterRules(rules: RuleSet): RuleSet = {
    val tmp = rules.getAll map (x=>x.getClass.getName)
    rules.filter(r => ! r.getClass.getName.contains("lf.Beta"))
  }
  def traverseTree(queue:mutable.ListBuffer[MPath],includedSet:mutable.Set[MPath],constants: mutable.ListBuffer[Theory]): Unit = {
    /**
    * breadth first search over theories
     * breadth first search is necessary to get the right prioritization between overridden definitions
     */
    if( queue.isEmpty)
      return
    val th: MPath = queue.remove(0)
    if(includedSet.contains(th))
      return
    else
      includedSet.add(th)
    controller.get(th) match {
      case t:Theory =>
        constants.append(t)
        val inc: List[MPath] = t.getIncludes
        queue.addAll(inc)
        // for (incl <- queue) traverseTree(incl,includedSet, queue,constants)
        // for (const <- t.getConstants) constants.put(const.name.toString,const)
    }
    traverseTree(queue, includedSet, constants)
  }
  def gather_definitions(theory:Theory,con : Context, rules: RuleSet) = {
    controller.simplifier(theory)
    val paths  = theory.getAllIncludes
    val tmp = paths map {q =>  controller.getTheory(q.from)
    }
    val theories: mutable.ListBuffer[Theory] = mutable.ListBuffer()
    val queue: mutable.ListBuffer[MPath] = mutable.ListBuffer()
    queue.append(theory.toTerm.toMPath)
    traverseTree(queue,mutable.Set(),theories)
    // convert to immutable map, just for safety
    val q  = theories flatMap {_.getConstants}
    val buildRules = rules.get(classOf[RulePreprocessor])
    // here we now match against the _type_ not the term under the type!
    // the idea is to use these rules for the more static type information, rather than the code itsel
    // though MMT kind of blurs the line here
    val definedRules = q flatMap {x=>x match {
      //case term => term match {
        case const : Constant => buildRules.filter(r => const.tp match{
          case Some(c: Term) => r.applicable(c)
          case None => false
        }) flatMap {r: RulePreprocessor=> r(const)}
        case otherwise => None
      //}
    }
    }
    print(definedRules)
    print(paths)
    definedRules
  }
}



class Runtime(controller: Controller, env: RuntimeEnvironment, defined_rules: Iterable[ExecutionRule], val logPrefix: String) extends ExecutionCallback with Logger {
   val report = controller.report

   def execute(progOrg: Term): Term = {
      val context = env.stack.top
      val prog = controller.simplifier(progOrg, SimplificationUnit(context, true,false, false),rules=env.rules)
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
          log("trying defined rules")
          val defs = defined_rules.filter(_.applicable(t))
          defs foreach { r =>
            log("trying to apply rule" + r.toString)
            return r(controller, this, env, t)
          }
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
       case OMBINDC(binder,context,scopes)=>
         // we can't recurse into the scopes, since those might have mutable variables
         // that should only be run when the rest of the term is normalized
         // i.e. a form of "call by reference"
         prog
       case ComplexTerm(p, subs, cont, args) =>
         val argsE = args map execute
         // we have to execute again, in case we now have more rules available
         // a prominent example of this is substituting in a function in a mutable reference:
         // you were unable to execute e.g. OMA(OMV(f),...) but now it's OMA(OMBINDC(..),...) which can be executed
         // this mirrors the basic structure of all rules: execute arguments, then execute term.
         // typededInstance OMID(?Counter)
         val cT = ComplexTerm(p, subs, cont, argsE)

         if(cT hasheq prog)
           cT
         else
          execute(cT)

       case _ => prog
       //case t =>
       //  throw ExecutionError("cannot execute: " + t)
     }
   }


}
