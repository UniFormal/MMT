package scala.info.kwarc.mmt.api.test
import info.kwarc.mmt.api._
import frontend._
import objects._
import objects.Conversions

import org.scalatest._

@Ignore
class ProverSpec extends FlatSpec with Matchers {
  val controller = new Controller
  val testLocation = "src/test/resources/WebServerTest/"
  //controller.handleLine("log console")
  //controller.handleLine("log+ archive")
  //controller.handleLine("log+ build")
  //controller.handleLine("log+ controller") 
  
  controller.handleLine("archive add " + testLocation)
  //controller.handleLine("build test mmt-omdoc")  
  // term to string
  def tts(t: Obj) = controller.presenter.asString(t)
      
  val ip = new MMTInterpolator(controller)
  val mpath = Path.parseM("http://cds.omdoc.org/test/examples?FOLEQNatDed", utils.mmt.mmtbase)
  controller.handle(SetBase(mpath))
  
  import ip._
 
  val prover = new checking.Prover(controller)
  "A prover" should "detect if rules are applicable" in {
    val context = cont"[x: bool, y: bool]"
    val stack = Stack(Context(mpath) ++ context)
    val x = OMV("x")
    val y = OMV("y")
 
    val rules = checking.RuleBasedChecker.collectRules(controller, context)
    val goal = mmt"ded forall [z:univ] z=z"
    val apps = prover.applicable(goal, rules)(stack)
    apps foreach {a =>
      println(a.label + " : " + tts(a.apply()))
    } 
  }
}