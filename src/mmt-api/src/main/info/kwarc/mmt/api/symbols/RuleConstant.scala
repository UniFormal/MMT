package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._
import checking._
import backend._
import utils._

import scala.xml.Node 

/**
 * A RuleConstant is a declaration whose value is an externally provided rule.
 * Its inner structure is not representable in or accessible to MMT,
 * but it exposes functionality that MMT can use and that defines the semantics of MMT.
 * 
 * @param df the rule this declaration provides
 */
class RuleConstant(val home : Term, val name : LocalName, val df: Rule) extends Declaration {
   def toNode = <ruleconstant name={name.toPath}/>
   override def toString = name.toString
   def getComponents = Nil //List((DefComponent, df))
   def getDeclarations = Nil
   type ThisType = RuleConstant
   def translate(newHome: Term, prefix: LocalName, translator: Translator) = {
     val dfT = translator.applyRule(df)
     new RuleConstant(newHome, prefix/name, dfT)
   }
   def merge(that: Declaration) = mergeError(that)
}

/**
 * loads a rule (by reflection) given by a LocalName corresponding to a java name of a [[Rule]] object or of a [[StructuralFeature]] class
 */
class RuleConstantInterpreter(controller: frontend.Controller) {
   def apply(name: LocalName, thy: MPath): RuleConstant = {
      val java = name.steps.mkString("$")
      val rule = controller.extman.addExtensionO(java, Nil) match {
        case Some(sf: StructuralFeature) => sf.getRule
        case Some(_) => throw ParseError("extension exists but does not provide a rule")
        case None => controller.backend.loadRule(java, thy?name)
      }
      new RuleConstant(OMMOD(thy), name, rule)
   }
}

import parser._
import modules._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

// TODO the rule name followed by arguments should be parsed into a single Term, which is given to the RuleConstantInterpreter
class RuleConstantParser extends ParserExtension {
   private lazy val rci = new RuleConstantInterpreter(controller)
   def isApplicable(se: StructuralElement, keyword: String) = {
      se.isInstanceOf[DeclaredTheory] && keyword == "rule"
   }
   def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String, con:Context = Context.empty) = {
      val (n, reg) = s.reader.readDeclaration
      val thy = se.asInstanceOf[DeclaredTheory].path
      val name = LocalName.parse(n, s.namespaces)
      val rc = rci(name, thy)
      controller add rc
      //rc foreach controller.add
      //println(se)
   }
}

@deprecated
object ParametricRuleConstantInterpreter {
   def fromNode(n: Node, thy: MPath): List[RuleConstant] = {
      val name = xml.attr(n, "name")
      fromString(name, thy)
   }
   def fromString(s: String, thy: MPath): List[RuleConstant] = {
      val inputs = s.split("""\s""").map(_.trim).filter(_.nonEmpty)
      val name = LocalName.parse(inputs.head, NamespaceMap(thy))
      val pars = inputs.tail.map(Path.parseS(_,NamespaceMap(thy)))

      val java = name.steps.mkString(".")

      // val applmethod = refclass.member(TermName("apply")).asMethod
      val rules = try {
         if (pars.isEmpty) {
            val cls = Class.forName(java + "$")
            cls.getField("MODULE$").get(null) match {
               case r: Rule => List(r)
               case r: RuleList => r.getRules
            }
         } else {
            val m = runtimeMirror(getClass.getClassLoader)
            val refclass = ClassTag(Class.forName(java + "$"))
            val claass = m.classSymbol(refclass.runtimeClass)
            val modul = claass.companionSymbol.asModule
            val im = m reflect m.reflectModule(modul).instance
            val appmethod = im.symbol.typeSignature.member(TermName("apply")).asMethod
            val ret = im.reflectMethod(appmethod).apply(pars: _*)
            println("Return: " + ret)
            ret match {
               case r: Rule => List(r)
               case r: RuleList => r.getRules
            }
         }
      } catch {
         case e: Exception =>
            throw BackendError("reflection error", thy ? name).setCausedBy(e)
      }
      println("ActualReturn: " + rules)
      rules map (rule => new RuleConstant(OMMOD(thy), name, rule))
   }
}

@deprecated
abstract class RuleList {
   def getRules : List[Rule]
}

case class testrule(s : GlobalName) extends ComputationRule(s) {
   def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term]
   = None
}

object testrule2 extends ComputationRule(Path.parseS("http://refltest.org/?refltest?a",NamespaceMap.empty)) {
   def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term]
   = None
}

case class testrulelist(s : GlobalName) extends RuleList {
   def getRules = List(testrule(s))
}