package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._
import checking._
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
}

object RuleConstantInterpreter {
   def fromNode(n: Node, thy: MPath): RuleConstant = {
      val name = xml.attr(n, "name")
      fromString(name, thy)
   }
   def fromString(s: String, thy: MPath): RuleConstant = {
      val name = LocalName.parse(s, NamespaceMap(thy))
      val java = name.steps.mkString(".")
      val rule = try {
         val cls = Class.forName(java + "$")
         cls.getField("MODULE$").get(null).asInstanceOf[Rule]
      } catch {
         case e: Exception =>
            throw BackendError("reflection error", thy ? name).setCausedBy(e)
      }
      new RuleConstant(OMMOD(thy), name, rule)
   }
}

import parser._
import modules._

class RuleConstantParser extends ParserExtension {
   def isApplicable(se: StructuralElement, keyword: String) = {
      se.isInstanceOf[DeclaredTheory] && keyword == "rule"
   }
   def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String, con:Context = Context.empty) = {
      val (n, reg) = s.reader.readDeclaration
      val thy = se.asInstanceOf[DeclaredTheory].path
      val rc = RuleConstantInterpreter.fromString(n.trim, thy)
      controller.add(rc)
   }
}