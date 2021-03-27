package info.kwarc.mmt.mizar.newxml.syntax

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.mizar.newxml.translator.TranslationController

object Utils {
  def fullClassName(s: String) = {
    "info.kwarc.mmt.mizar.newxml.syntax."+s.replace("-", "_")
  }

  def makeSimpleGlobalName(aid: String, name: String) = TranslationController.getTheoryPath(aid.toLowerCase) ? LocalName(aid.toLowerCase+":"+name)
  def makeNewSimpleGlobalName(name: String) = makeSimpleGlobalName(TranslationController.currentAid, name)
  def makeGlobalName(aid: String, kind: String, ln: String) = makeSimpleGlobalName(aid, kind+ln)
  def makeNewGlobalName(kind: String, ln: String) = makeGlobalName(TranslationController.currentAid, kind, ln)
  def makeGlobalKindName(aid: String, globalKind: Char, ln: String) = makeGlobalName(aid, globalKind.toString, ln)
  private def MizarRedVarName(serialnr: Int): LocalName = LocalName(serialnr.toString)
  private def mapKind(kind: String): String = kind match {
    case "BoundVar" => "BV"
    case "Bound" => "B"
    case "Constant" => "C"
    case "ReservedVar" => "C"//"R"
    case "Scheme-Functor" => "SF"
    case "Scheme-Predicate" => "SP"
    case "Private-Predicate" => "PP"
    case "Private-Functor" => "PF"
    case "DefConstant" => "C"
  }
  def MizarVariableName(spelling: String, kind: String, serialnr: Int): LocalName = {
    LocalName(spelling) / LocalName(mapKind(kind)) / MizarRedVarName(serialnr)
  }

  /**
   * Internal representation of Properties class
   * @param _just (optional) the proof of the property
   */
  sealed abstract class MizarProperty(_property: String, _just:Option[Justification]) {
    def property = _property
    def just = _just
  }
  /**
   * Commutativity of binary functors f, i.e. it means:
   * for [x, y] f(x, y) = f(y, x)
   * @param _just (optional) the proof of the property
   */
  case class Commutativity(_just:Option[Justification]) extends MizarProperty("commutativity", _just:Option[Justification])
  /**
   * Idempotence of binary functors f, i.e. it means:
   * for [x] f(x, x) = x
   * @param _just (optional) the proof of the property
   */
  case class Idempotence(_just:Option[Justification]) extends MizarProperty("idempotence", _just:Option[Justification])
  /**
   * Involutiveness of binary functors f, i.e. it means:
   * for [x] f(f(x)) = x
   * @param _just (optional) the proof of the property
   */
  case class Involutiveness(_just:Option[Justification]) extends MizarProperty("involutiveness", _just:Option[Justification])
  /**
   * Projectivity of binary functors f, i.e. it means:
   * for [x] f(f(x)) = f(x)
   * @param _just (optional) the proof of the property
   */
  case class Projectivity(_just:Option[Justification]) extends MizarProperty("projectivity", _just:Option[Justification])

  /**
   * Reflexivity of binary predicates (relations) R, i.e. it means:
   * for [x] x R x
   * @param _just (optional) the proof of the property
   */
  case class Reflexivity(_just:Option[Justification]) extends MizarProperty("reflexivity", _just:Option[Justification])
  /**
   * Irreflexivity of binary predicates (relations) R, i.e. it means:
   * for [x] not x R x
   * @param _just (optional) the proof of the property
   */
  case class Irreflexivity(_just:Option[Justification]) extends MizarProperty("irreflexivity", _just:Option[Justification])
  /**
   * Symmetry of binary predicates (relations) R, i.e. it means:
   * for [x, y] x R y => y R x
   * @param _just (optional) the proof of the property
   */
  case class Symmetry(_just:Option[Justification]) extends MizarProperty("symmetry", _just:Option[Justification])
  /**
   * Assymmetry of binary predicates (relations) R, i.e. it means:
   * for [x, y] x R y => not y R x
   * @param _just (optional) the proof of the property
   */
  case class Assymmetry(_just:Option[Justification]) extends MizarProperty("assymmetry", _just:Option[Justification])
  /**
   * For binary predicates (relations) R, its meaning is:
   * for [x, y] x R y or y R x
   * @param _just (optional) the proof of the property
   */
  case class Connectedness(_just:Option[Justification]) extends MizarProperty("connectedness", _just:Option[Justification])
  /**
   * For modes and existential_registrations
   * only those modes (and subtypes, expanded into) can be used as types in fraenkel_terms
   * @param _just (optional) the proof of the property
   * @param _tp the type whose sethood to show
   */
  case class Sethood(_just:Option[Justification], _tp: Option[Type]) extends MizarProperty("sethood", _just:Option[Justification])
  def matchProperty(prop: String, _just:Option[Justification], _tp:Option[Type] = None) = prop match {
    case "commutativity" => Commutativity(_just)
    case "idempotence" => Idempotence(_just)
    case "involutiveness" => Involutiveness(_just)
    case "projectivity" => Projectivity(_just)
    case "reflexivity" => Reflexivity(_just)
    case "irreflexivity" => Irreflexivity(_just)
    case "symmetry" => Symmetry(_just)
    case "asymmetry" => Assymmetry(_just)
    case "connectedness" => Connectedness(_just)
    case "sethood" => Sethood(_just, _tp)
  }
  sealed abstract class DeclarationKinds extends PatternKinds
  case class ModeKind() extends DeclarationKinds
  case class StructureKind() extends DeclarationKinds
  case class FunctorKind() extends DeclarationKinds
  case class AttributeKind() extends DeclarationKinds
  case class PredicateKind() extends DeclarationKinds
  sealed abstract class PatternKinds
  case class StrictKind() extends PatternKinds
  case class AggregateKind() extends PatternKinds
  case class SelectorKind() extends PatternKinds
  case class ForgetfulKind() extends PatternKinds
  def shortKind(kind: PatternKinds): Char = kind match {
    case ModeKind() => 'M'
    case StructureKind() => 'L'
    case FunctorKind() => 'K'
    case AttributeKind() => 'V'
    case PredicateKind() => 'R'
    case StrictKind() => shortKind(StructureKind())//'V'
    case AggregateKind() => shortKind(StructureKind())//'G'
    case SelectorKind() => shortKind(StructureKind())//'J'
    case ForgetfulKind() => shortKind(StructureKind())//'U'
  }
}
