package info.kwarc.mmt.stex.rules

import info.kwarc.mmt.api.{ComplexStep, GlobalName, LocalName, MPath, Rule, RuleSet, utils}
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, Continue, EliminationRule, ExtendedCheckingEnvironment, ExtensionalityRule, FormationRule, History, InhabitableRule, IntroductionRule, Solver, SubtypingRule, TermHeadBasedEqualityRule, TypingRule, ValueSolutionRule}
import info.kwarc.mmt.api.frontend.NotFound
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{ModuleOrLink, Theory}
import info.kwarc.mmt.api.notations.{LabelArg, LabelInfo, Marker, SimpArg}
import info.kwarc.mmt.api.objects.{Context, Equality, EqualityContext, IsContext, OMA, OML, OMPMOD, OMS, OMV, Stack, StatelessTraverser, Subtyping, Term, Traverser, Typing, VarDecl}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, DerivedDeclaration, Elaboration, ParametricTheoryLike, StructuralFeature, StructuralFeatureRule}
import info.kwarc.mmt.api.uom.{ExtendedSimplificationEnvironment, Simplifiability, Simplify}
import info.kwarc.mmt.api.utils.MyList
import info.kwarc.mmt.lf.OfType
import info.kwarc.mmt.stex.SHTML

import scala.collection.mutable
import scala.util.Try

object ModelsOf {
  val tp = SHTML.meta_path ? "record type"
  val sym = SHTML.meta_path ? "module type"
  val term = OMS(sym)
  def apply(mp : MPath, args : Term*) = OMA(this.term,List(OMPMOD(mp,args.toList)))
  def apply(t : Term) = OMA(this.term,List(t))
  def unapply(t : Term) : Option[Term] = t match {
    case OMA(this.term,List(OMPMOD(mp,args))) => Some(OMPMOD(mp,args))
    case _ => None
  }
}

class RecSymbol(name:String) {
  val path = SHTML.meta_path ? name
  val term = OMS(path)
}

object Getfield extends RecSymbol("record field") {
  def apply(t:Term, f: LocalName) = OMA(this.term, List(t, OML(f)))
  def unapply(t: Term) : Option[(Term,LocalName)] = t match {
    case OMA(this.term, List(tm, OML(f,_,_,_,_))) => Some((tm, f))
    case _ => None
  }
}

object RecMerge extends RecSymbol("module type merge") {
  def apply(tm:Term*) = OMA(this.term,tm.toList)
  def unapply(tm : Term) =tm match {
    case OMA(this.term,args) => Some(args)
    case _ => None
  }
}

/** unifies record terms and types; the empty record is OMA(this.term,Nil), not this.term */
abstract class SimpleRecordLike(n: String) extends RecSymbol(n) {
  // there may not be an apply method that takes a context instead of a List[OML]
  def apply(v:OML*): Term = apply(v.toList)
  def apply(fields: List[OML]): Term = if (fields.isEmpty) term else OMA(this.term, fields)
  // case Some(l) => OMBINDC(this.term, Context(VarDecl(l)), fields)
  def unapply(t: Term) : Option[RecordBody]
}

object RecType extends SimpleRecordLike("anonymous record") {
  def unapply(t : Term) : Option[RecordTypeBody] = t match {
    case OMA(this.term, OMLList(fs)) => Some(RecordTypeBody(fs))
    // case OMBINDC(this.term, Context(VarDecl(n, None, None, None, _), rest@_*), OMLList(fs)) if rest.isEmpty => Some(RecordBody(Some(n), fs,isType))
    case _ => None
  }
  def make(fs : OML*) = OMA(this.term,fs.toList)
}

object RecExp extends SimpleRecordLike("anonymous record") {
  def unapply(t : Term) : Option[RecordExpBody] = t match {
    case OMA(this.term, OMLList(fs)) => Some(RecordExpBody(fs))
    // case OMBINDC(this.term, Context(VarDecl(n, None, None, None, _), rest@_*), OMLList(fs)) if rest.isEmpty => Some(RecordBody(Some(n), fs,isType))
    case _ => None
  }
}


class MathStructureFeature extends StructuralFeature("structure") with ParametricTheoryLike {
  //override def getHeaderNotation: List[Marker] = List(LabelArg(1, LabelInfo.none),SimpArg(1))
  override def check(dd: DerivedDeclaration)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  override def elaborate(parent: ModuleOrLink, dd: DerivedDeclaration)(implicit env: Option[ExtendedSimplificationEnvironment]): Elaboration = new Elaboration {
    assert(dd.name.toString.endsWith("-structure"))
    val rname = LocalName(dd.name.toString.dropRight(10))
    val c = Constant(parent.toTerm,rname,Nil,Some(OMS(ModelsOf.tp)),Some(ModelsOf(dd.modulePath)),None)
    override def getO(name: LocalName): Option[Declaration] = if (name == rname) Some(c) else None
    override def domain: List[LocalName] = List(rname)
  }
}

object MathStructureRule extends RuleSet {
  override def getAll: Iterable[Rule] = Seq(
    //StructuralFeatureRule(classOf[MathStructureFeature],"structure"),
    RecordTypeInhabitable,
    RecordTypeTerm,
    RecordExpTerm,
    RecordMergeInference,
    GetfieldTerm,
    GetFieldComp,
    MergeCheck,
    RecTypeCheck,
    RecEquality,
    RecTypeCongruence,
    RecExpCongruence,
    CanonicalSolution,
    RecSubtype,
    ModTypeInhabitable
  )
}

object ModTypeInhabitable extends InhabitableRule(ModelsOf.sym) {
  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = term match {
    case ModelsOf(OMPMOD(_,_)) => Some(true)
    case _ => None
  }
}


object RecordsGeneral {

  /** reverts the effect of omlsToContext by replacing every OMV(prefix/name) to OML(name)
   * returns translated term and flag whether self was used
   */
  def contextToOMLs(prefix: LocalName, t: Term): (Term, Boolean) = {
    var skips : List[LocalName] = Nil
    var prefixUsed = false
    val replacer = new Traverser[Boolean] {
      def traverse(t: Term)(implicit con: Context, toplevel: Boolean): Term = t match {
        case OMV(name) if !con.isDeclared(name) => name.dropPrefix(prefix) match {
          case None => t
          case Some(rest) => if (/*toplevel*/ !skips.contains(name)) OML(rest) else {
            prefixUsed = true
            Getfield(OMV(prefix), rest)
          }
        }
        case RecType(ls) =>
          val oldskips = skips
          skips :::= ls.fields.map(_.name)
          val ret = Traverser(this, t)(con, false)
          skips = oldskips
          ret
        case RecExp(ls) =>
          val oldskips = skips
          skips :::= ls.fields.map(_.name)
          val ret = Traverser(this, t)(con, false)
          skips = oldskips
          ret
        case _ => Traverser(this, t)
      }
    }
    (replacer(t, true), prefixUsed)
  }

  /** tries to turn a type tp into a record */
  def makeRecBody(checker: CheckingCallback, tm: Term)(implicit stack: Stack, history: History) = {
    implicit val lookup : Lookup = checker.lookup
    def unappl(t : Term) : Option[RecordBodyLike] = t match {
      case RecordBodyLike(bd) =>
        Some(bd)
      case RecMerge(stp) =>
        val nstp = stp map {it =>
          checker.safeSimplifyUntil(it)(unappl)._1
        }
        if (stp != nstp)
          unappl(RecMerge(nstp:_*)) else None
      case _ => None
    }
    val ret = checker.safeSimplifyUntil(tm)(unappl)
    ret
    /* tpS match {
      case RecordBodyLike(ret) => Some(ret)
      // we might try to handle a unknown here and introduce a fresh unknown for a list of other fields, compare makePi in LF
      case _ =>
        tpS// solver.simplify(tpS)
    } */
  }
}

// import RecordsGeneral._

trait RecordRule {
  def safe[A](f : => Option[A])(implicit checker : CheckingCallback, history : History) = try {
    f
  } catch {
    case RecordError(s) =>
      checker.error(s)
      None
  }
  def safeS(f : => Simplifiability)(implicit checker : CheckingCallback, history : History) = try {
    f
  } catch {
    case RecordError(s) =>
      checker.error(s)
      Simplifiability.NoRecurse
  }
  object RecLike {
    def unapply(tm : Term) = tm match {
      case RecType(l) => Some(l)
      case ModelsOf(t) => Some(t)
      case RecMerge(ls) => Some(ls)
      case _ => None
    }
  }
}

object RecordTypeInhabitable extends InhabitableRule(RecType.path) with RecordRule {
  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = safe { term match {
    case RecType(bd) =>
      history += "checking inhabitability of record type " + solver.presentObj(term)
      if (bd.hasDuplicates) {
        solver.error("no shadowing allowed in record type")
        return Some(false)
      }
      // turn into context and check the context
      val prefix = bd.getPrefix()(solver.constantContext:::stack.context)
      val cont = bd.asContext{msg =>
        solver.error(msg)
        return Some(false)
      }(solver.constantContext:::stack.context)
      solver.check(IsContext(stack++VarDecl(prefix), cont))(history + ("checking record body as a context using name prefix " + prefix))
      cont.mapVarDecls {case (sofar,vd) =>
        (vd.tp,vd.df) match {
          case (None,_) =>
            solver.error("record type has untyped field: " + vd.name)
            return Some(false)
          case (Some(tp),None) =>
            solver.check(info.kwarc.mmt.api.objects.Inhabitable(stack++sofar, tp))(history + ("checking that abstract field " + vd.name + " is a type"))
          case (Some(_),Some(_)) =>
          // concrete field can have any universe
        }
      }
      Some(true)
    case _ => Some(false)
  }}(solver,history)
}

/** Formation: the type inference rule |- A1:type , ... , |- An:type  --->  |- |{x1:A1,...,xn:An}| : type */
object RecordTypeTerm extends FormationRule(RecType.path, OfType.path) with RecordRule {

  override def applicable(t: Term): Boolean = t match {
    case ModelsOf(_) => true
    case RecType(_) => true
    case _ => false
  }
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = safe {
    // if (covered) return Some(Univ(1)) WTF?
    tm match {
      case ModelsOf(_) =>
        Some(OMS(ModelsOf.tp))
      case RecType(bd) =>
        /*
        history += "checking record type " + solver.presentObj(tm)
        if (bd.hasDuplicates) {
          solver.error("no shadowing allowed in record type")
          return None
        }
        // turn into context and check the context
        val prefix = bd.getPrefix()(solver.constantContext:::stack.context)
        val cont = bd.asContext{msg =>
          solver.error(msg)
          return None
        }(solver.constantContext:::stack.context)
        val ret = solver.check(IsContext(stack++VarDecl(prefix), cont))(history + ("checking record body as a context using name prefix " + prefix))
        history += "Context checked: " + ret
        // being a context without shadowing is a necessary requirement
        // additionally we check the universe of each abstract field
        // alternatively, we could allow for higher universes and return a correspondingly higher universe
        var maxLevel : BigInt = 1
        cont.mapVarDecls {case (sofar,vd) =>
          history += "Checking record field " + vd.name + ". Current universe: " + solver.presentObj(DefinedTypeLevel(maxLevel))
          (vd.tp,vd.df) match {
            case (None,_) =>
              solver.error("record type has untyped field: " + vd.name)
              return None
            case (Some(tp),None) =>
              //solver.check(Typing(stack++sofar, tp, Univ(1)))(history + ("checking that abstract field " + vd.name + " is a type"))
              val tpI = solver.inferType(tp, true)(stack++sofar,history)
              tpI match {
                case Some(DefinedTypeLevel(i)) =>
                  // println(i + ": " + tpI)
                  maxLevel = i max maxLevel
                  history += "Field belongs to universe " + solver.presentObj(DefinedTypeLevel(i))
                case Some(other) =>
                  val ret = solver.tryToCheckWithoutDelay(Subtyping(stack++sofar,other,DefinedTypeLevel(maxLevel)))
                  if (!ret.contains(true)) {
                    // println("here")
                    history += "No idea what to do with type " + solver.presentObj(other)
                    return None
                  }
                case _ =>
                  history += "Couldn't infer universe of " + vd.name + "'s type: " + solver.presentObj(tp)
                  return None
              }
            case (Some(_),Some(_)) =>
              history += "Field is defined, hence checks out"
            // concrete field can have any universe
          }
        }
        Some(DefinedTypeLevel(maxLevel))
         */
        Some(OMS(ModelsOf.tp))
      case _ => None // should be impossible
    }
  }(solver,history)
}

/** the type inference rule |-t1=d1:A1, ... |-tn=dn:An  --->  |[d1,...,dn]|:|{t1:A1,...,tn:An}| */
// note that this proceeds parallel to [[RecordTypeTerm]]
object RecordExpTerm extends IntroductionRule(RecExp.path, OfType.path) with RecordRule {
  // override def alternativeHeads: List[GlobalName] = List(RecMerge.path)
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = safe {
    tm match {
      case RecExp(bd) =>
        history += "inferring record type of " + solver.presentObj(tm)
        if (!covered) {
          if (bd.hasDuplicates) {
            history += "no shadowing allowed in record term"
            solver.error("no shadowing allowed in record term")
            return None
          }
        }
        // turn into context and check the context
        // prefix does not matter but must be fresh
        // val prefix = pickPrefix(solver, bd)
        var prefixUsed = false
        val prefix =bd.getPrefix()(solver.constantContext:::stack.context)
        val cont = bd.asContext{msg =>
          history += msg
          solver.error(msg)
          return None
        }(solver.constantContext:::stack.context)
        if (!covered) {
          // is this needed?
          solver.check(IsContext(stack++VarDecl(prefix), cont))(history + ("checking record body as a context using name prefix " + prefix))
        }
        // being a context without shadowing is a necessary requirement
        // additionally we check that all fields are concrete and collect the types to build the result
        val omls = cont.mapVarDecls {case (sofar, vd) =>
          val name = vd.name.dropPrefix(prefix).get // defined by invariant of omlsToContext
          val tp = (vd.tp,vd.df) match {
            case (_, None) =>
              history += "record term has undefined field"
              solver.error("record term has undefined field")
              return None
            case (None, Some(df)) =>
              // infer missing type
              solver.inferType(df, covered)(stack ++ VarDecl(prefix) ++ sofar, history + ("infering type of field: " + name)).getOrElse {
                history += "can't infer type of " + solver.presentObj(vd)
                return None
              }
            case (Some(itp), Some(_)) =>
              itp
          }
          // turn back into an OML and drop the definiens
          // alternatively, we could keep the definiens, thus inferring a much smaller type
          // it is unclear if that is desirable or how it interacts with other rules
          val (tpS, pU) = RecordsGeneral.contextToOMLs(prefix, tp)
          prefixUsed ||= pU
          OML(name, Some(tp), None)
        }
        Some(RecType(omls))
      case _ => None
    }
  }(solver,history)
}

object RecordMergeInference extends IntroductionRule(RecMerge.path,OfType.path) with RecordRule {
  override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = safe {
    implicit val lookup : Lookup = solver.lookup
    RecordsGeneral.makeRecBody(solver,tm)._2 match {
      case Some(bd : RecordTypeLike) =>
        RecordTypeTerm(solver)(RecType(bd.fields),covered)
      case Some(bd : RecordExpressionLike) =>
        RecordExpTerm(solver)(RecExp(bd.fields),covered)
      case _ => None
    }
  }(solver,history)
}

/** Elimination: the type inference rule t : |{a:A,...}|  --->  t.a:A */
object GetfieldTerm extends EliminationRule(Getfield.path, OfType.path) with RecordRule {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = safe { tm match {
    case Getfield(recexp, f) =>
      implicit val lookup : Lookup = solver.lookup
      val rec = RecordsGeneral.makeRecBody(solver,recexp)._1// solver.safeSimplifyUntil(recexp)(RecExp.unapply)._1
      // infer type of record expression and turn it into a record type
      val recI = solver.inferType(rec, covered)(stack, history + "infering type of record").getOrElse {
        return None
      }
      val recTp = RecordsGeneral.makeRecBody(solver, recI)
      recTp._2 match {
        case Some(bd :RecordTypeLike) =>
          bd.getTypeForTerm(rec,f)
        // bd.getType(solver,f,rec)
        case _ =>
          history += "does not look like a record type at this point"
          None
      }
    case _ => None // should be impossible
  }}(solver,history)
}

/** computation: the rule |[..., x=t,...]|.x = t */
object GetFieldComp extends ComputationRule(Getfield.path) with RecordRule {
  // override def priority: Int = super.priority + 1
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = safeS {
    // var s = "GetFieldComp: " + tm
    var (record, field) = tm match {
      case Getfield(r, f) => (r, f)
      case _ =>
        return Simplifiability.NoRecurse
    }
    implicit val lookup : Lookup = solver.lookup

    // since the definiens might be in the type, this rule cannot exploit Simplifiability properly

    def getInType: Simplifiability = {
      // try to find the definiens in the type
      val tp = solver.inferType(record, covered).getOrElse(return Simplifiability.NoRecurse)
      RecordsGeneral.makeRecBody(solver,tp)._2 match {
        case Some(ibd:RecordTypeLike) =>
          ibd.getDefiniensForTerm(record,field) match {
            // ibd.getDefinition(solver,field,record,true) match {
            case Some(dfP) =>
              Simplify(dfP)
            case None =>
              Simplifiability.NoRecurse
          }
        case _ =>
          Simplifiability.NoRecurse
      }
    }
    val r = RecordsGeneral.makeRecBody(solver,record)
    record = r._1
    // Is a record Expression that contains required definiens
    r._2 match {
      case Some(bd:RecordExpressionLike) =>
        bd.getDefiniens(field) match {
          // bd.getDefinition(solver,field,record,covered) match {
          case Some(dfP) =>
            if (!covered) solver.inferType(record,false)
            Simplify(dfP)
          case None =>
            getInType
        }
      case _ =>
        getInType
    }
  }(solver,history)
}

object MergeCheck extends TypingRule(RecMerge.path) with RecordRule {
  override def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = safe {
    implicit val lookup : Lookup = solver.lookup
    RecordsGeneral.makeRecBody(solver,tp)._2 match {
      case Some(tp:RecordTypeLike) =>
        RecTypeCheck(solver)(tm,RecType(tp.fields))
      case _ => None
    }
  }(solver,history)
}

/** type-checking: the type checking rule |-t.a1:A1 ... |-t.an:An  --->  t : |{a1:A1,...,an:An}|
 *
 *  t may have additional fields; those are ignored except that they have to be well-formed.
 */
object RecTypeCheck extends TypingRule(RecType.path) with RecordRule {
  // var debugstack : List[String] = Nil
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = safe {
    val RecType(bd) = tp
    implicit val lookup : Lookup = solver.lookup
    val rtm = RecordsGeneral.makeRecBody(solver,tm)// solver.safeSimplifyUntil(tm)(RecExp.unapply)._1
    val fieldsOK = bd.fields.filterNot(_.df.isDefined).forall {oml =>
      val name = oml.name
      history += "checking field " + name
      val tmName = rtm._2 match {
        case Some(itmbd:RecordExpressionLike) =>
          val tmbd = (bd + itmbd).asInstanceOf[RecordExpressionLike]
          tmbd.setPrefix(bd.getPrefix()(solver.constantContext:::stack.context))
          tmbd.getDefiniens(name).getOrElse(Getfield(rtm._1,name))
        // tmbd.getDefinition(solver,name,rtm,false,bd.self,Some(bd)).getOrElse {
        // Getfield(rtm, name)
        // }
        case _ => Getfield(rtm._1,name)
      }// getField(rtm, name, bd.self,Some(bd)) // tm.name
      // we now check tmName against the expected type and definiens
      bd.getTypeForTerm(rtm._1,name) match {
        // bd.getType(solver,name,rtm,Some(bd)) match {
        case None =>
          return Some(solver.error("untyped field in record type")) // impossible for well-formed type
        case Some(fieldTp) =>
          // check tmName against expected type
          // val fieldTp = projectTerm(rtm, bd.self, t,Some(bd)) // replace all OMLs in oml.tp with projections of tm
          // history += s"checking field: $name against expected type"
          // debugstack ::= (oml.name + " in " + solver.presentObj(tm) + " _=_ " + solver.presentObj(tmName) + " _:_ " + solver.presentObj(tp))
          // println("-----\n" + debugstack.reverse.mkString("\n") + "\n-----")

          val tpOK = if (!isUnknown(solver, rtm._1)) solver.check(Typing(stack, tmName, fieldTp))(history + s"checking field: $name against expected type")
          else {
            true
          }
          // check tmName against expected value (if any)
          // this is usually redundant as tmName is by construction identical to fieldDf unless tm explicitly lists a different definiens
          // this may happen when overriding or redefining a field with an equivalent value
          if (!tpOK) {
            history += "Field " + name + " failed"
            false
          } else {
            bd.getDefiniensForTerm(rtm._1,name) match {
              // bd.getDefinition(solver,name,rtm,false) match {
              case None =>
                history += "Field " + name + " okay"
                true
              case Some(fieldDf) =>
                // val fieldDf = projectTerm(rtm, bd.self, d,Some(bd)) // replace all OMLs in oml.tp with projections of tm
                if (solver.check(Equality(stack, tmName, fieldDf, Some(fieldTp)))(history + s"checking field: $name against expected value")) {
                  history += "Field " + name + " okay"
                  true
                } else {
                  history += "Field " + name + " failed"
                  false
                }
            }
          }
      }
    }
    // debugstack = debugstack.tail
    if (!fieldsOK) {
      Some(false)
    } else {
      // we're essentially done, but technically we have to make that all the fields of tm that the type ignores are well-formed
      // conceptually, this should happen first, but the available type information should be exploited to solve unknowns first
      val msg = "the term checks as far as the expected type is concerned; but we have to make it is well-formed at all"
      if (!isUnknown(solver,tm)) Some(solver.inferTypeAndThen(tm)(stack, history + msg) { _ => true }) else Some(true)
    }
  }(solver,history)
  def isUnknown(solver: Solver, t: Term) = solver.Unknown.unapply(t).isDefined
}

/** equality-checking:  |- t1.a1 = t2.a1 : A1 ... |- t1.an = t2.an : An  --->  t1 = t2 : |{ a1:A1,...,an:An }|
 *
 *  Only the fields of the type are compared. Thus, the equality of two records depends on the type at which they are compared.
 */
object RecEquality extends ExtensionalityRule(Nil, RecType.path) with RecordRule {
  val introForm = RecExp
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = safe {
    val RecType(bd) = tp
    val result = bd.fields.forall {oml =>
      val name = oml.name
      if (oml.df.isDefined)
        true // no need to compare fields whose equality is forced by the type (by precondition, we have tm1: tp and tm2: tp)
      else {
        implicit val lookup : Lookup = solver.lookup
        val omlTp = bd.getTypeForTerm(tm1,name) // bd.getType(solver,name,tm1)// oml.tp.map {t => projectTerm(tm1, bd.self, t,  None)} // replace all OMLs in oml.tp with projections of tm1 (or tm2)
        solver.check(Equality(stack, Getfield(tm1, name), Getfield(tm2, name), omlTp))(history + s"checking equality of field $name")
      }
    }
    Some(result)
  }(solver,history)
}

/** checks two records for equality of all fields up to reordering and definition expansion */
abstract class RecordCongruence(rec: SimpleRecordLike) extends TermHeadBasedEqualityRule(Nil, rec.path, rec.path) with RecordRule {
  def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History): Option[Continue[Boolean]] = safe {
    (tm1, tm2) match {
      case (rec(bd1), rec(bd2)) =>
        implicit val lookup : Lookup = checker.lookup
        val continue = Continue {
          if (bd1.names.toSet != bd2.names.toSet) {
            checker.error("records do not have the same fields")
          } else {
            // convert to contexts using the same prefix for both records
            val onError = (msg:String) => {
              checker.error(msg)
              return None
            }
            val prefix = bd1.getPrefix()(checker.outerContext:::stack.context)
            val cont1 = bd1.asContext(onError)(checker.outerContext:::stack.context)//omlsToContext(prefix, bd1)(onError)
            bd2.setPrefix(prefix)
            val cont2 = bd2.asContext(onError)(checker.outerContext:::stack.context)//omlsToContext(prefix, bd2)(onError)
            checker.check(EqualityContext(stack++VarDecl(prefix),cont1,cont2,false)) // context equality includes reordering
          }
        }
        Some(continue)
      case _ => None
    }}(checker,history)
}

/** two record types are equal if if they have the same field names and agree in all fields */
object RecTypeCongruence extends RecordCongruence(RecType)

/**
 * two record terms whose type is not known are equal if they have the same field names and agree in all fields
 *
 * This corresponds to using the strictest possible equality: it guarantees equality at all types
 *
 * If the type is known, we do not proceed in order to apply [[RecEquality]] which makes more terms equal.
 */
object RecExpCongruence extends RecordCongruence(RecExp) {
  override def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
    if (tp.isDefined) None else super.apply(checker)(tm1, tm2, tp)
  }
}

/**
 * This rule tries to pick a canonical solution for an unknown of record type, for which one field value is known.
 *
 * It covers the case ?.(f_i) = tm2.
 *
 * Such constraints occur in particularly when some fields of a records are types or have a type-like flavor.
 * This is particularly common in algebra, e.g., ?.universe = U with ?:Magma occurs frequently when using a constant
 *   comp: {M:Magma} M.universe -> M.universe -> M.universe # 2 * 3
 *
 * To be sound and complete, we should generate new unknowns ?i (one per abstract field in the type of ?) and solve ?=[..., f_i = ?i, ...] and ?i = tm2.
 * But in practice, there is typically not enough information to solve all ?i uniquely.
 * Therefore, heuristics have been introduced to pick a canonical solution for ? if some but not all of its fields are known.
 *
 * A typical heuristic is to prefer uniform solutions, i.e., a solution for ? that is not a record term.
 * In particular, one may choose solving ? with a variable or a constant if possible.
 * But even that often does not determine a unique solution.
 *
 * Specifying the desired solution (which is necessary to check whether this rule is implemented correctly) is an open problem.
 * Gonthier's canonical structures and unification hints are recent attempts.
 *
 * For now, this rule is experimental.
 */
object CanonicalSolution extends ValueSolutionRule(Getfield.path) with RecordRule {
  def applicable(t: Term): Option[Int] = t match {
    case Getfield(_,_) =>
      Some(0)
    case _ => None
  }
  def apply(j: Equality): Option[(Equality,String)] = {
    j.tm1 match {
      case Getfield(r, f) =>
        j.tm2 match {
          case Getfield(s, g) if f == g =>
            val solve = s match {
              case OMV(x) if j.context.isDeclared(x) =>
                true
              case _ =>
                true // this is too optimistic, but works for now
            }
            if (solve)
              Some((Equality(j.stack, r, s, None), "using bound variable as canonical record"))
            else
              None
          case _ => None
        }
      case _ => None
    }
  }
}

/**
 * subtyping for record types
 *
 * the rule  A#x <: B#x  for all fields x of B  ---->  A <: B
 * where A and B are record types and R#x projects out the type of a field (in analogy to r.x for terms)
 *
 * horizontal subtyping: A may have more fields or definitions than B
 * vertical   subtyping: A's fields must be subtypes of their B-counterparts
 */
import info.kwarc.mmt.api.objects.Conversions._

object RecSubtype extends SubtypingRule with RecordRule {
  val head = RecType.path

  def applicable(tp1: Term, tp2: Term) = (tp1, tp2) match {
    case (RecLike(l), RecLike(r)) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = safe {
    implicit val lookup : Lookup = solver.lookup
    (tp1, tp2) match {
      case (RecordTypeLike(bd1), RecordTypeLike(bd2)) =>
        // pair up omls in bd1 and bd2 in the correct checking order
        val paired = {
          var subs: List[OML] = bd1.fields
          bd2.fields.foldLeft(List[(Option[OML], Option[OML])]())((sofar, field) => {
            val i = subs.indexWhere(_.name == field.name) match {
              case -1 => None
              case j => Some(j)
            }
            val news: List[(Option[OML], Option[OML])] = i.map(subs.take).getOrElse(Nil).map(o => (Some(o), None))
            subs = i.map(subs.drop).getOrElse(subs)
            val old = i.map(_ => {
              val h = subs.head
              subs = subs.tail
              h
            })
            sofar ::: news ::: List((old, Some(field)))
          })
        }

        implicit val lookup : Lookup = solver.lookup

        paired.foldLeft(List[OML]())((istack, p) => {
          history += "Checking field " + p._1.getOrElse(p._2.get).name // at least one is defined by construction.
          val vd = Context.pickFresh(stack.context, LocalName("rec"))._1 % RecType(istack: _*)
          p match {
            case (None, Some(field)) if field.df.isEmpty =>
              solver.error("Missing field " + field.name + " in " + solver.presentObj(tp1))
              return Some(false)
            case (None, Some(field)) =>
              istack ::: List(field)
            case (Some(field), None) =>
              istack ::: List(field)
            case (Some(left), Some(right)) =>
              val tpn1 = bd1.getTypeForTerm(OMV(vd.name), left.name) /*bd1.getType(solver,left.name,OMV(vd.name)).*/ getOrElse {
                solver.error("presumed subtype does not have typed field " + left.name)
                return Some(false)
              }
              val tpn2 = bd2.getTypeForTerm(OMV(vd.name), left.name) /* bd2.getType(solver,left.name,OMV(vd.name)).*/ getOrElse {
                solver.error("presumed supertype has untyped field " + left.name)
                return Some(false)
              }
              // val tpb1 = projectTerm(OMV(vd.name),None,tp1,Some(RecordBody(None,istack)))
              // val tpb2 = projectTerm(OMV(vd.name),None,tp2,Some(RecordBody(None,istack)))
              if (!solver.check(Subtyping(stack ++ vd, tpn1, tpn2))) {
                solver.error(solver.presentObj(tpn1) + " is not subtype of " + solver.presentObj(tpn2) + "  ( " + tpn1 + " and " + tpn2 + " )")
                return Some(false)
              } else istack ::: List(left)
          }
        }
        )
        Some(true)
      case _ => None
    }
  }(solver, history)
}


/** matches for a list of OMLs */
object OMLList {
  def unapply(ts: List[Term]): Option[List[OML]] = {
    val omls = ts map {
      case o:OML => o
      case _ => return None
    }
    Some(omls)
  }
}

/** unifies all terms that behave like records (record expressions/types, Mod-types, RecordMerge-terms...) */
trait RecordBodyLike {
  private var _self: Option[LocalName] = None
  def asTerm : Term

  def setPrefix(ln : LocalName) = _self = Some(ln)
  /** the prefix used in omlsToContext must be a fresh variable
   *
   * this is a convenience method that picks such a prefix
   *
   * @param checker only used to obtain the current context
   * @param stack   only used to obtain the current context
   * @param bds     some other record bodies, whose self-variables (if any) are used as the preferred prefixes if possible
   */
  def getPrefix(bds : RecordBodyLike*)(context : Context) : LocalName = _self.getOrElse {
    val p = (this :: bds.toList).foldLeft[Option[LocalName]](None)((sofar, next) => sofar orElse next._self) getOrElse LocalName("self")
    val (prefix, _) = Context.pickFresh(/* checker.outerContext ++ stack.context */ context, p)
    setPrefix(prefix)
    prefix
  }

  /** names of all fields */
  def names : List[LocalName]
  def fields : List[OML] = prfields(tm => thisprojectioncase(tm))
  def prfields(projectioncase : Term => Option[LocalName]): List[OML]

  def thisprojectioncase(tm : Term) : Option[LocalName]

  def +(that : RecordBodyLike) : RecordBodyLike
}

object RecordBodyLike {
  def unapply(t : Term)(implicit lookup: Lookup) : Option[RecordBodyLike] = t match {
    case RecordTypeLike(rb) => Some(rb)
    case RecordExpressionLike(rb) => Some(rb)
    case _ => None
  }

  /** derived projection that replaces every OML in a term with the corresponding projection from a record
   *
   * If record types/expressions are seen as theories/morphisms, this is morphism application.
   */
  def projectTerm(tm : Term,getDf : LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup: Lookup) : Option[Term] = {
    var skips : List[LocalName] = Nil
    var loop : List[(Term,LocalName)] = Nil
    object Break extends Throwable
    object prcase {
      def unapply(tm : Term) = projectioncase(tm)
    }

    val replacer = new Traverser[Boolean] {
      def traverse(t: Term)(implicit con: Context, toplevel: Boolean): Term = t match {
        case prcase(name) =>
          if (/* toplevel */ !skips.contains(name)) {
            val df = getDf(name)
            val ret = df match {
              case Some(Getfield(r,f)) =>
                if (loop.contains((r,f))) return df.get
                loop ::= (r,f)
                Getfield(traverse(r),f)
              case _ =>
                traverse(df.getOrElse(throw Break))
            }
            // projectTerm(df.getOrElse(throw Break), getDf).getOrElse(throw Break) // getField(rec, name, self, tp)
            df match {
              case Some(Getfield(r,f)) if loop.head==(r,f) => loop=loop.tail
              case _ =>
            }
            ret
          }
          else
            t
        case Getfield(r,f) =>
          Getfield(traverse(r),f)
        // after traversing into a record, OMLs refer to fields of that record, not to this record anymore; the only way to refer to this record is via self.f
        case RecordBodyLike(ls) =>
          val oldskips = skips
          skips :::= ls.names
          val ret = Traverser(this, t)(con, false)
          skips = oldskips
          ret
        case _ => Traverser(this, t)
      }
    }
    try {
      Some(replacer(tm, true))
    } catch {
      case Break => None
    }
  }
}

object RecordTypeLike {
  def unapply(tm : Term)(implicit lookup:Lookup) : Option[RecordTypeLike] = tm match {
    case RecType(rb) => Some(rb)
    case ModelsOf(OMPMOD(mp,args)) => Some(ModuleType(mp,args,lookup))
    case RecMerge(tms) =>
      val bds = tms.map(RecordBodyLike.unapply)
      bds.tail.foldLeft(bds.head)((b,ob) => if (b.isEmpty) None else ob.map(b.get + _)) match {
        case Some(rt: RecordTypeLike) => Some(rt)
        case _ => None
      }
    case _ => None
  }
}

object RecordExpressionLike {
  def unapply(tm : Term)(implicit lookup:Lookup): Option[RecordExpressionLike] = tm match {
    case RecExp(rb) => Some(rb)
    case RecMerge(tms) =>
      val bds = tms.map(RecordBodyLike.unapply)
      bds.tail.foldLeft(bds.head)((b,ob) => if (b.isEmpty) None else ob.map(b.get + _)) match {
        case Some(rt: RecordExpressionLike) => Some(rt)
        case _ => None
      }
    case _ => None
  }
}

abstract class Merged(children_ : => List[RecordBodyLike]) extends RecordBodyLike {
  def children : List[RecordBodyLike] = children_.flatMap {
    case m:Merged => m.children
    case o => List(o)
  }

  override def prfields(projectioncase : Term => Option[LocalName]): List[OML] = children.flatMap(_.prfields(projectioncase)).distinct

  override def names: List[LocalName] = children.flatMap(_.names).distinct//left.names ::: right.names
  override def asTerm: Term = RecMerge(children.map(_.asTerm):_*)

  def thisprojectioncase(tm: Term): Option[LocalName] = children.collectFirst{
    case c if c.thisprojectioncase(tm).isDefined => c.thisprojectioncase(tm).get
  }

  override def setPrefix(ln: LocalName): Unit = {
    children.foreach(_.setPrefix(ln))
  }

  override def getPrefix(bds: RecordBodyLike*)(context : Context): LocalName = {
    val ret = children.head.getPrefix(children ::: bds.toList: _*)(context)
    children.tail.foreach(_.setPrefix(ret))
    ret
  }
}

trait RecordTypeLike extends RecordBodyLike {

  def getOrig(name:LocalName)(implicit lookup : Lookup,history: History) : Option[Declaration] = None

  private val _defs : mutable.HashMap[(Term,LocalName),Option[Term]] = mutable.HashMap.empty
  private val _tps : mutable.HashMap[(Term,LocalName),Option[Term]] = mutable.HashMap.empty
  private var tpcycle : Option[(Term,LocalName)] = None
  private var dfcycle : Option[(Term,LocalName)] = None

  def getTypeForTerm(tm: Term, name: LocalName)(implicit lookup : Lookup,history: History): Option[Term] =
    _tps.getOrElseUpdate((tm,name),{
      if (tpcycle contains ((tm,name))) {
        tpcycle = None
        None
      } else {
        tpcycle = Some((tm,name))
        getTypeForTerm(name,ln => Some(getDf(tm,ln)),tm => thisprojectioncase(tm))
      }
    })
  def getTypeForTerm(name: LocalName,cont : LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history:History):Option[Term]

  def getDefiniensForTerm(tm: Term, name: LocalName)(implicit lookup : Lookup,history: History): Option[Term] =
    _defs.getOrElseUpdate((tm,name),{
      if (dfcycle contains ((tm,name))) {
        dfcycle = None
        None
      } else {
        dfcycle = Some((tm, name))
        getDefiniensForTerm(name, ln => Some(getDf(tm, ln)), tm => thisprojectioncase(tm))
      }
    })
  def getDefiniensForTerm(name: LocalName,cont : LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history:History):Option[Term]

  protected def getDf(tm: Term,name:LocalName)(implicit lookup : Lookup,history:History) : Term = {
    def default = getDefiniensForTerm(tm,name).getOrElse(Getfield(tm,name))
    tm match {
      case RecordExpressionLike(exp) =>
        exp.getDefiniens(name).getOrElse(default)
      case _ => default
    }
  }

  def +(that : RecordBodyLike) = that match {
    case rt: RecordTypeLike => new MergedType({List(this, that)})
    case re: RecordExpressionLike => new MergedExpression({List(this, re)})
  }

}

case class RecordError(s : String) extends Throwable

class MergedType(children_ : => List[RecordBodyLike]) extends Merged(children_) with RecordTypeLike {
  override def getOrig(name: LocalName)(implicit lookup : Lookup,history: History): Option[Declaration] =
    MyList(children).mapFind {
      case rtl : RecordTypeLike => rtl.getOrig(name)
      case r => throw RecordError("Merge of non-type records in record type " + r.asTerm)
    }

  override def getDefiniensForTerm(name: LocalName, cont: LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history: History): Option[Term] = {
    MyList(children).mapFind {
      case rtl : RecordTypeLike =>
        rtl.getDefiniensForTerm(name,cont,projectioncase)
      case r => throw RecordError("Merge of non-type records in record type " + r.asTerm)
    }
  }

  override def getTypeForTerm(name: LocalName, cont: LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history: History): Option[Term] = {
    MyList(children).mapFind {
      case rtl : RecordTypeLike => rtl.getTypeForTerm(name,cont,projectioncase)
      case r => throw RecordError("Merge of non-type records in record type " + r.asTerm)
    }
  }

}

trait RecordExpressionLike extends RecordBodyLike {
  private val _defs: mutable.HashMap[LocalName, Option[Term]] = mutable.HashMap.empty
  private var dfcycle: Option[LocalName] = None

  def getDefiniens(name: LocalName)(implicit lookup : Lookup,history:History):Option[Term] =
    _defs.getOrElseUpdate(name,{
      if (dfcycle contains name) {
        dfcycle = None
        None
      } else {
        dfcycle = Some(name)
        getDefiniens(name, ln => getDefiniens(name), tm => thisprojectioncase(tm))
      }
    })
  def getDefiniens(name: LocalName,cont : LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history:History):Option[Term]

  def +(that : RecordBodyLike) : RecordBodyLike = that match {

    case re : RecordExpressionLike => new MergedExpression({List(this,re)})
    case _ => new MergedExpression({List(that,this)})
  }
}

class MergedExpression(children_ : => List[RecordBodyLike]) extends Merged(children_) with RecordExpressionLike {

  override def getDefiniens(name: LocalName, cont: LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history: History): Option[Term] = {
    MyList(children).mapFind {
      case tp : RecordTypeLike => tp.getDefiniensForTerm(name,cont,projectioncase)
      case exp : RecordExpressionLike => exp.getDefiniens(name,cont,projectioncase)
    }
  }
}

/** unifies the case of bodies of record expressions and types */
abstract class RecordBody(ifields: List[OML]) extends RecordBodyLike {
  /** names of all fields */
  def names = fields.map(_.name)
  /** checks for duplicate names */
  def hasDuplicates = utils.hasDuplicates(names)
  /** retrieve a field for a given name */
  def get(l: LocalName) = fields.find(_.name == l)

  def thisprojectioncase(tm : Term) : Option[LocalName] = tm match {
    case OML(name, None, None, None, None) =>
      Some(name)
    //case OMSemiFormal(Text(_,s) :: Nil) => Some(LocalName(s))
    case _ => None
  }
  def prfields(projectioncase : Term => Option[LocalName]): List[OML] = ifields

  def asContext(onError: String => Nothing)(context : Context): Context = omlsToContext(onError)(context)

  /** turns the OML fields into the corresponding context
   *
   * field name n becomes variable name prefix/n, prefix must be fresh to ensure invertibility of this function
   * if defined, body.self can be safely used as the prefix
   */
  private def omlsToContext(onError: String => Nothing)(context : Context)/*(implicit checker: CheckingCallback, stack: Stack)*/: Context = {
    val prefix = getPrefix()(context)
    var result = Context.empty
    var skips : List[LocalName] = Nil
    object project {
      def unapply(tm: Term) = thisprojectioncase(tm)
    }
    // replaces every OML in replacements with the corresponding OMV
    val replacer = new Traverser[Boolean] {
      def traverse(t: Term)(implicit con: Context, toplevel: Boolean): Term = t match {
        case project(name) =>
          if (!skips.contains(name) && result.isDeclared(prefix / name) && !con.isDeclared(prefix))
          // toplevel OML that has previously been declared in this record refers to a field of this record
          // TODO what to do if con.isDeclared(prefix)? introducing the OMV would capture but leaving the wrong too.
            OMV(prefix / name)
          else
          // other OMLs are unaffected
            t
        case Getfield(tm, f) =>
          tm match {
            case OMV(n) if n == prefix && !con.isDeclared(n) =>
              // self.f refers to a field of this record, becomes OMV(f)
              OMV(prefix / f)
            case _ =>
              // all other projections are unaffected
              Getfield(traverse(tm), f)
          }
        case OMV(n) if n==prefix && !con.isDeclared(n) =>
          onError("self reference may only occur in projections")
        // after traversing into a record, OMLs refer to fields of that record, not to this record anymore; the only way to refer to this record is via self.f
        case RecType(ls) =>
          val oldskips = skips
          skips :::= ls.fields.map(_.name)
          val ret = Traverser(this, t)(con, false)
          skips = oldskips
          ret
        case RecExp(ls) =>
          val oldskips = skips
          skips :::= ls.fields.map(_.name)
          val ret = Traverser(this, t)(con, false)
          skips = oldskips
          ret
        case _ => Traverser(this, t)
      }
    }
    fields.foreach { o =>
      val vdR = o.vd.copy(name = prefix / o.vd.name).map(t => replacer(t, true))
      result = result ++ vdR
    }
    result
  }

}

case class RecordTypeBody(ifields:List[OML]) extends RecordBody(ifields) with RecordTypeLike {
  def asTerm = RecType(fields)

  def getTypeForTerm(name: LocalName,cont : LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history:History):Option[Term] = {
    // find the right field, takes its type, and substitute references to the other fields
    fields.find(_.name == name) match {
      case Some(oml) if oml.tp.isDefined =>
        RecordBodyLike.projectTerm(oml.tp.get, ln => cont(ln),projectioncase /* Some(getDf(tm,name)) */)
      // Some(projectTerm(relativeTo, tpi, tp, None))
      case Some(oml) =>
        throw RecordError("untyped field in record type") //impossible if type is well-formed

      case _ =>
        throw RecordError("term has no field " + name)
    }
  }

  def getDefiniensForTerm(name: LocalName,cont : LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history: History): Option[Term] = fields.find(_.name==name) match {
    case Some(oml) if oml.df.isDefined =>
      RecordBodyLike.projectTerm(oml.df.get,ln => cont(ln),projectioncase/* Some(getDf(tm,ln)) */)
    case _ => None
  }
}

case class RecordExpBody(ifields : List[OML]) extends RecordBody(ifields) with RecordExpressionLike {
  def asTerm = RecExp(fields)
  override def getDefiniens(name: LocalName,cont : LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history: History): Option[Term] = {
    get(name).flatMap(_.df) match {
      case Some(df) =>
        val dfP = RecordBodyLike.projectTerm(df,ln => cont(ln),projectioncase)
        /*
        if (!covered) {
          val tpO = checker.inferType(relativeTo, false)(stack, history + "making sure redex is well-formed")
          if (tpO.isEmpty) return None
        }
        */
        dfP
      case _ =>
        None
    }
  }
}

object ModuleType {
  def apply(mp : MPath, args : List[Term], lookup : Lookup) = {

    def getD(ln : LocalName) = lookup.getO(OMPMOD(mp,args),ln) match {
      case Some(c : Constant) => c
      case Some(c) => throw RecordError("not a constant: " + c.path)
      case _ =>
        throw RecordError("Symbol doesn't exist: " + ln)
    }
    val dones : mutable.HashMap[(MPath,List[Term]),SimpleModule] = mutable.HashMap.empty
    def get(ip : MPath, iargs : List[Term]) : SimpleModule = dones.getOrElseUpdate((ip,iargs),{
      val th = try{lookup.getO(ip) match {
        case Some(t : Theory) => t
        case _ => throw RecordError("Not a theory: " + ip)
      }} catch {
        case nf:NotFound =>
          throw RecordError("Not found: " + ip)
      }
      def names = th.getConstants/*.filter(_.rl.contains("stexsymbol"))*/.map(_.path)
      new SimpleModule(th,iargs,{ names },ln => getD(ln))
    })
    def getAll(ip : MPath,ia : List[Term]): List[SimpleModule] = {
      val sm = get(ip,ia)
      val is : List[(MPath,List[Term])] = getIncludes(sm.theory,ia).map(id => (id.from, id.args.map(_ ^? sm.subs))) //TODO id may now inlcude a definition
      is.flatMap(p => getAll(p._1,p._2)) ::: List(sm)
    }
    val th = try {
      lookup.getO(mp) match {
        case Some(t: Theory) => t
        case _ => throw RecordError("Not a theory: " + mp)
      }
    } catch {
      case NotFound(_,_) => throw RecordError("Not found: " + mp)
    }
    if (getIncludes(th,args).isEmpty) get(mp,args) else
      new MergedType({ getAll(mp,args).distinct })
  }

  def getIncludes(th : Theory,args : List[Term]) = th.getAllIncludesWithoutMeta

  def makepath(gn : GlobalName,getD : LocalName => Constant) : GlobalName = {
    val c = getD(LocalName(gn.module)/gn.name)
    makepath(c)
  }
  def makepath(c : Constant) : GlobalName = if (c.name.steps.length == 1) c.path
  else {
    val opt = c.alias.find(_.steps.length == 1)
    opt match {
      case Some(n) => c.parent ? n
      case None =>
        c.parent ? c.name
    }
  }
  def makeName(ln : LocalName) = LocalName(ln.steps.filterNot(_.isInstanceOf[ComplexStep]))
}

class SimpleModule(val theory : Theory, args : List[Term], _names : => List[GlobalName], getD : LocalName => Constant) extends RecordTypeLike {
  private lazy val localNames = _names.map(ModuleType.makepath(_,getD))
  override def asTerm: Term = ModelsOf(OMPMOD(theory.path,args))
  override def names : List[LocalName] = localNames.map(gn => ModuleType.makeName(gn.name))

  override def getOrig(name: LocalName)(implicit lookup: Lookup, history: History): Option[Declaration] = {
    Try(lookup.getO(OMPMOD(theory.path,args),name)).toOption.flatten match {
      case Some(c : Constant) => Some(c)
      case _ => None
    }
  }

  override def prfields(projectioncase : Term => Option[LocalName]): List[OML] = localNames.map(gn => {
    val n = ModuleType.makepath(gn,getD)
    object traverser extends StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: this.State): Term = {
        projectioncase(t) match {
          case Some(ln) => OML(ln)
          case _ => Traverser(this, t)
        }
      }
    }
    val c = getD(LocalName(n.module)/n.name)
    OML(ModuleType.makeName(n.name),c.tp.map(traverser.apply(_,())),c.df.map(traverser.apply(_,())))
  })

  val subs = (theory.parameters / args).getOrElse {
    // error: wrong number of arguments
    // println("wrong number of arguments: " + th.parameters + " <~~ " + args)
    throw RecordError("wrong number of arguments for theory " + theory.path + ": " + theory.parameters + " <~~ " + args)
  }

  override def thisprojectioncase(tm: Term): Option[LocalName] = tm match {
    case OMS(gn) =>
      val p = Try(ModuleType.makepath(gn,getD)).getOrElse(return None)
      if (localNames contains p) Some(ModuleType.makeName(p.name)) else None
    case _ =>
      None
  }
  override def getTypeForTerm(name: LocalName, cont: LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history: History): Option[Term]
  = {
    val names = localNames
    val n = names.find(gn => ModuleType.makeName(gn.name) == name).getOrElse(return None)
    RecordBodyLike.projectTerm(getD(LocalName(n.module) / n.name).tp.getOrElse(return None) /* ^? subs */,cont,projectioncase)
  }

  override def getDefiniensForTerm(name: LocalName, cont: LocalName => Option[Term],projectioncase : Term => Option[LocalName])(implicit lookup : Lookup,history: History): Option[Term]
  = {
    val n = localNames.find(gn => ModuleType.makeName(gn.name) == name).getOrElse(return None)
    RecordBodyLike.projectTerm(getD(LocalName(n.module) / n.name).df.getOrElse(return None) ^? subs,cont,projectioncase)
  }

}