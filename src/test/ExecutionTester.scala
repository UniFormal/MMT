import CompTransTest.controller
import info.kwarc.mmt.api.{DPath, Error, ErrorHandler, Invalid, InvalidElement, InvalidObject, InvalidUnit, MPath, NamespaceMap, Path, RuleSet, SourceError, archives, checking, documents, execution, utils}
import info.kwarc.mmt.api.execution.{Executor, Heap, RuleBasedExecutor, Runtime, RuntimeEnvironment}
import info.kwarc.mmt.api.frontend.{Controller, LMHConf}
import info.kwarc.mmt.api.objects.{Context, Stack, Term}
import info.kwarc.mmt.api.parser.{Parser, ParsingUnit, SourceRef}
import info.kwarc.mmt.api.uom.ConstantScala
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.LambdaCongruence
import info.kwarc.mmt.test.testers._
import info.kwarc.mmt.api._
import frontend._
import objects._
import parser._
import utils._
import scala.collection.mutable.ListBuffer


object Executiontester extends MagicTest() {
  override def doFirst: Unit = {
    super.doFirst
  }
  def run : Unit = {
    //println(MagicTest.archiveRoot)
    //List(File(System.getProperty("user.home") / "MMT" / "myformalizations")find.(_exists).getOrElse(println("Does not exist"))
    hl("extension info.kwarc.mmt.api.execution.ExecuteFromShell")
    val doc = Path.parseD("latin:/", NamespaceMap.empty)
    val dom = controller.getTheory(doc ? "Program")
    val mp = dom.path
    println(mp)
    val rb = controller.extman.get(classOf[Executor]).head
    controller.getConfig.getEntries(classOf[LMHConf]).foreach { e => controller.addArchive(e.local)}

    val nsMap = controller.getNamespaceMap
    val iiC = new documents.InterpretationInstructionContext(nsMap)
    val context = Context(mp)
    val tm ="mainInt"
    val pu = ParsingUnit(SourceRef.anonymous(tm), context, tm, iiC)
    // hl("run " + mp + " main" )


    val parser = controller.extman.get(classOf[Parser], "mmt").get
    val err : ErrorHandler = new TestErrorBuffer(controller)
    val term = parser(pu)(err).toTerm
    println("##################################")
    println("Run typecheck")
    println("##################################")
    testTyping(term, context)
    println("##################################")
    println("test standalone")
    println("##################################")
    testPrint(term, context)
   println("##################################")
   println("Run integration")
   println("##################################")

   // rb(context, term)


    println("done executing")

  }

  def testTyping(term: Term, context : Context) : Unit = {
    val rules = RuleSet.collectRules(controller, context)
    println(rules)
    val progC = checking.Solver.check(controller, Stack(context), term) match{
      case Left((value,_)) => println(value)
      case Right(sol) => {
        println("")
        sol.logState("error")}
    }

  }

  def testPrint(term:Term, context: Context): Unit = {
    val heap = new Heap(0, controller)
    controller.add(heap)
    val stack = new execution.Stack
    val rules = RuleSet.collectRules(controller, context)
    val env = new RuntimeEnvironment(heap, stack, rules)
    val runtime = new Runtime(controller, env, logPrefix)
    runtime.execute(term)
  }
}

