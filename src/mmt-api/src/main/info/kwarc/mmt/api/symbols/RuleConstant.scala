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
   val feature = "rule"
   def toNode = <ruleconstant name={name.toPath}><type>{tp.toNode}</type></ruleconstant>
   override def toString = name.toString
   def getComponents = List(TypeComponent(new FinalTermContainer(tp))) // df?
   def getDeclarations = Nil
   type ThisType = RuleConstant
   /** translated rule must still be created from translated type */
   def translate(newHome: Term, prefix: LocalName, translator: Translator, context : Context) = {
     new RuleConstant(newHome, prefix/name, translator.applyType(context, tp), None)
   }
   def merge(that: Declaration) = mergeError(that)
}

/**
 * loads a rule (by reflection) given by a LocalName corresponding to a java name of a [[Rule]] object or of a [[StructuralFeature]] class
 */
class RuleConstantInterpreter(controller: frontend.Controller) {
   /** computes the definiens of a rule constant by creating the rule
    */
   def createRule(rc: RuleConstant) {
      val rl = rc.tp
      val (rlP,rlArgs) = rl match {
        case OMPMOD(p,as) => (p,as)
        case _ => throw InvalidObject(rl, "cannot interpret as semantic object: " + rl)
      }
      val rule = controller.extman.addExtensionO(SemanticObject.mmtToJava(rlP, true), Nil) match {
        case Some(sf: StructuralFeature) =>
          if (rlArgs.nonEmpty) throw InvalidObject(rl, "too many arguments")
          sf.getRule
        case Some(_) => throw InvalidObject(rl, "extension exists but does not provide a rule: " + rl)
        case None =>
          val so = controller.backend.loadObjectO(rlP).getOrElse {
            throw InvalidObject(rl, "semantic object not found")
          }
          so match {
            case r: Rule =>
              if (rlArgs.nonEmpty) throw InvalidObject(rl, "too many arguments")
              r              
            case r: ParametricRule =>
              try {
                r(controller, rc.home, rlArgs)
              } catch {case e: Exception =>
                throw InvalidObject(rl, "error while instantiating parametric rule").setCausedBy(e)
              }
            case _ => throw InvalidObject(rl, "semantic object exists but is not a rule: " + rl)
          }
      }
      rc.df = Some(rule)
   }
  
   /**
    * @param thy the containing theory
    * @param rl the rule (i.e., the type of the rule constant)
    * @param create force creation of the rule (default: only if no arguments)
    */
   def apply(thy: MPath, rl: Term, create: Boolean): RuleConstant = {
      val (rlP,rlArgs) = rl match {
        case OMPMOD(rlP, rlArgs) => (rlP, rlArgs)
        case _ => throw ParseError("cannot interpret as semantic object: " + rl)
      }
      val name = if (rlArgs.isEmpty)
        LocalName(rlP)
      else
        LocalName(rlP) / rl.hashCode.toString //TODO compute better name
      val rc = new RuleConstant(OMMOD(thy), name, rl, None)
      // rules without arguments can be created immediately (to be used during parsing)
      if (rlArgs.isEmpty || create) createRule(rc)
      rc
   }  
}

import parser._
import modules._

class RuleConstantParser extends ParserExtension {
   private lazy val rci = new RuleConstantInterpreter(controller)
   def isApplicable(se: StructuralElement, keyword: String) = {
      se.isInstanceOf[DeclaredTheory] && keyword == "rule"
   }
   def apply(pea: ParserExtensionArguments) = {
      val (_,_,pr) = pea.parser.readParsedObject(pea.context, None)(pea.state)
      val thy = pea.se match {
        case t: DeclaredTheory => t.path
        case d: documents.Document =>
          d.contentAncestor match {
            case Some(t: DeclaredTheory) => t.path
            case _ => throw ParseError("rules only allowed in theories")
          }
        case _ => throw ParseError("rules only allowed in theories")
      }
      if (!pr.isPlainTerm)
        throw ParseError("can only interpret plain terms as rules, found: " + pr.toTerm)
      val rc = rci(thy, pr.term, true)
      // TODO set document home of rc
      Some(rc)
   }
}
