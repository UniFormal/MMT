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
class RuleConstant(val home : Term, val name : LocalName, val tp: Term, var df: Option[Rule]) extends Declaration {
   def toNode = <ruleconstant name={name.toPath}><type>{tp.toNode}</type></ruleconstant>
   override def toString = name.toString
   def getComponents = List(TypeComponent(new FinalTermContainer(tp))) // df?
   def getDeclarations = Nil
   type ThisType = RuleConstant
   /** translated rule must still be created from translated type */
   def translate(newHome: Term, prefix: LocalName, translator: Translator) = {
     new RuleConstant(newHome, prefix/name, translator.applyType(Context.empty, tp), None)
   }
   def merge(that: Declaration) = mergeError(that)
}

/**
 * loads a rule (by reflection) given by a LocalName corresponding to a java name of a [[Rule]] object or of a [[StructuralFeature]] class
 */
class RuleConstantInterpreter(controller: frontend.Controller) {
   /** interprets a term as a rule, which may have to be loaded
    *  @param thy the containing theory
    *  @param rl the term representing the rule
    */
   def createRule(thy: MPath, rl: Term): Rule = {
      val (rlP,rlArgs) = rl match {
        case OMPMOD(rlP, rlArgs) => (rlP, rlArgs)
        case _ => throw ParseError("cannot interpret as semantic object: " + rl)
      }
      controller.extman.addExtensionO(SemanticObject.mmtToJava(rlP, true), Nil) match {
        case Some(sf: StructuralFeature) =>
          if (rlArgs.nonEmpty) throw ParseError("too many arguments")
          sf.getRule
        case Some(_) => throw ParseError("extension exists but does not provide a rule: " + rl)
        case None =>
          val so = controller.backend.loadObject(rlP)
          so match {
            case r: Rule =>
              if (rlArgs.nonEmpty) throw ParseError("too many arguments")
              r              
            case r: ParametricRule =>
              r(rlArgs)
            case _ => throw ParseError("semantic object exists but is not a rule: " + rl)
          }
      }
   }
  
   def apply(thy: MPath, rl: Term): RuleConstant = {
      val rule = createRule(thy, rl)
      val OMPMOD(mp,_) = rl
      new RuleConstant(OMMOD(thy), LocalName(mp), rl, Some(rule))
   }  
}

import parser._
import modules._

// TODO the rule name followed by arguments should be parsed into a single Term, which is given to the RuleConstantInterpreter
class RuleConstantParser extends ParserExtension {
   private lazy val rci = new RuleConstantInterpreter(controller)
   def isApplicable(se: StructuralElement, keyword: String) = {
      se.isInstanceOf[DeclaredTheory] && keyword == "rule"
   }
   def apply(sp: KeywordBasedParser, s: ParserState, se: StructuralElement, keyword: String, con:Context = Context.empty) = {
      val (_,_,pr) = sp.readParsedObject(con, None)(s)
      val thy = se.asInstanceOf[DeclaredTheory].path
      if (!pr.isPlainTerm)
        throw ParseError("can only interpret plain terms as rules")
      val rc = rci(thy, pr.term)
      controller add rc
   }
}
