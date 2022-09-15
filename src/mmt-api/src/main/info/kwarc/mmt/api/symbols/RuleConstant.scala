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
// TODO this should be abolished in favor of defined constants, definiens is rule as scala id literals, normalized definiens is scala rule literal
class RuleConstant(val home : Term, val name : LocalName, val tpC: TermContainer, var df: Option[Rule]) extends Declaration {
   val feature = "rule"
   def tp = tpC.get
   def toNode = <ruleconstant name={name.toPath}>{if (tpC.isDefined) <type>{tp.get.toOBJNode}</type> else Nil}</ruleconstant>
   override def toString = name.toString + tp.map(" : " + _.toString).getOrElse("")
   def getComponents = List(TypeComponent(tpC)) // df?
   def getDeclarations = Nil
   type ThisType = RuleConstant
   /** translated rule must still be created from translated type */
   def translate(newHome: Term, prefix: LocalName, translator: Translator, context : Context) = {
     new RuleConstant(newHome, prefix/name, tpC map {t => translator.applyType(context, t)}, None)
   }
   def merge(that: Declaration) = mergeError(that)
}

object RuleConstant {
  def apply(home : Term, name : LocalName, tm: Term, df: Option[Rule]) = new RuleConstant(home, name, TermContainer(tm), df)
  def apply(home : Term, name : LocalName, tm: Option[Term], df: Option[Rule]) = new RuleConstant(home, name, TermContainer(tm), df)
}

/**
 * loads a rule (by reflection) given by a LocalName corresponding to a java name of a [[Rule]] object or of a [[StructuralFeature]] class
 */
class RuleConstantInterpreter(controller: frontend.Controller) {
   /** computes the definiens of a rule constant by creating the rule
    */
   def createRule(rc: RuleConstant): Unit = {
      val rl = rc.tp.getOrElse {
        return
      }
      val rule = Rule.loadRule(controller, Some(rc.home), rl)
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
      val rc = RuleConstant(OMMOD(thy), name, rl, None)
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
      se.isInstanceOf[Theory] && keyword == "rule"
   }
   def apply(pea: ParserExtensionArguments) = {
      val (_,_,pr) = pea.parser.readParsedObject(pea.context, None)(pea.state)
      val thy = pea.se match {
        case t: Theory => t.path
        case d: documents.Document =>
          d.contentAncestor match {
            case Some(t: Theory) => t.path
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
