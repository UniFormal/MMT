package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._
import checking._
import utils._

import scala.xml.Node 

/** A RealizedConstant is a semantic entity whose precise structure is not accessible to the syntax
 *  but which exposes functionality that the syntax can make use of.
 */
abstract class RealizedConstant extends Declaration {
   def children = Nil
   def toNode = <realizedconstant name={name.toPath}/>
   override def toString = name.toString
   def getComponents = Nil
   def getDeclarations = Nil
}

/** A RealizedConstant representing the opaque realization of a type */
class RealizedTypeConstant(val home : Term, val name : LocalName, val real: RealizedType) extends RealizedConstant

/** A RealizedConstant representing a rule */
class RuleConstant(val home : Term, val name : LocalName, val df: Rule) extends RealizedConstant

object RuleConstantInterpreter {
   def fromNode(n: Node, thy: MPath): RuleConstant = {
      val name = xml.attr(n, "name")
      fromString(name, thy)
   }
   def fromString(s: String, thy: MPath): RuleConstant = {
      val name = LocalName.parse(s, thy)
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
   def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String) = { 
      val (n, reg) = s.reader.readDeclaration
      val thy = se.asInstanceOf[DeclaredTheory].path
      val rc = RuleConstantInterpreter.fromString(n.trim, thy)
      controller.add(rc)
   }
}