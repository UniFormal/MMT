package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.Obj.getConstants
import info.kwarc.mmt.api.objects.{OMA, OMID, Term}
import info.kwarc.mmt.api.{GlobalName, Rule, SyntaxDrivenRule, checking, symbols}

import scala.collection.mutable
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox


class Compiler {
  def compileTerm(t:Term)={
    // 1. gather all CompilationRules
    // 2. order them by priority
    // 3. pick the first fitting rule
    // 4. execute the rule
    ???
  }
  def getType(t:Term) : Tree = {
    ???
  }
  def handleConstants(controller:Controller,compiledMap:mutable.Map[Theory,ConstantContainer],thy: Theory): ConstantContainer = {
    // 1. compile the contents
    val t : List[universe.Tree] = thy.getConstants map (x=> ConstantRule(this,x))
    // 2. get the name of the new theory
    val name: String = thy.path.toPath
    // 3. check the compileMap for the compiled parents
    val parentTheories = thy.getIncludesWithoutMeta map {x=>controller.getModule(x)}
    val res = parentTheories map {
      case t : Theory => compiledMap.get(t) match {
        case Some(x) => x // 3.1 it is already in the buffer
        case None => handleConstants(controller, compiledMap, t) // 3.2 recursive construction
      }
      case _ => ???
    }
    // 4. construct the container and add it to the memoization
    val cont = ConstantContainer(t, name, res)
    compiledMap.update(thy,cont)
    cont
  }

  def compile(controller: Controller, theory:Theory) ={
    val refl = universe.runtimeMirror(this.getClass.getClassLoader)
    val tmp = List(ApplyRule, ConstantRule)
    val memoization = mutable.Map[Theory,ConstantContainer]()
    val cont = handleConstants(controller, memoization, theory)


    val compiledClass = cont.assemble()
    // construct term, the object, and run it
    ???
    refl.mkToolBox().eval(compiledClass)
  }
}

case class ConstantContainer(contents: List[Tree], name :String, var includes: List[ConstantContainer]){
  def assemble(): Tree= {
    val parents = includes map {x=> x.assemble()}
    val ClassDef(mods, tn,ls,_) = q"trait TMP{ def test = 42}"
    val body: Template = Template(parents,noSelfType, contents)
    ClassDef(mods, TypeName(name),Nil,body)
  }
}

trait CompilerEndpoint{
  def getEndpointName : String
}

case class IrreducibleOperator(name : String, args : List[Any]){
  def apply(arg: Any*): IrreducibleOperator = IrreducibleOperator(name, arg.toList)

}
trait Compileable{
  def applicable(prog: Term) : Boolean
}

abstract class CompilationRule(priority: Int) extends Rule with Compileable with CompilerEndpoint {
  def apply(compiler: Compiler,controller:Controller, prog:Term):Tree
}

abstract class TypeRule(priority: Int) extends CompilationRule(priority: Int)

object DefaultTypeRule extends TypeRule(-1000){
  override def apply(compiler: Compiler, controller: Controller, prog: Term): universe.Tree = {
    val name = prog.toMPath.toPath
    q"IrreducibleOperator($name, Nil)"
  }

  override def applicable(prog: Term): Boolean = true

  override def getEndpointName: String = "scala"
}

object ApplyRule extends CompilationRule(-1000) {
  override def apply(compiler: Compiler, controller: Controller, prog: Term): universe.Tree = {
    prog match {
      case OMA(fun, args) =>
        val argsC = args map {x=>compiler.compileTerm(x)}
        val funC = compiler.compileTerm(fun)
        //q"$funC ( ..$argsC )"
      ???
    }
  }

  def applicable(prog: Term) : Boolean={
    prog match {
      case OMA(fun, args) =>true
      case _ => false
    }
  }
  override def getEndpointName: String = "Scala"
}

object ConstantRule {
  def apply(compiler: Compiler, prog: symbols.Constant): universe.Tree = {
    prog.df match {
      case None=>{
        ???
      }
      case Some(tm)=> {
        val funcname = TermName(prog.path.toPath)
        val body = compiler.compileTerm(tm)
        // q"def $funcname = { $body }"
        ???
      }
    }
  }
  def getEndpointName: String = "Scala"
}

