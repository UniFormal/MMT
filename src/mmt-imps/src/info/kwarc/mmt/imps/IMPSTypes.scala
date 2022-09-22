package info.kwarc.mmt.imps

/* IMPORTS */

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.{Logger, Report}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{JSONObject, JSONString, URI}
import info.kwarc.mmt.imps.FrmFnd.{handpick, removeWhitespace}
import info.kwarc.mmt.imps.ParseMethod.ParseMethod
import info.kwarc.mmt.imps.NumericalType.NumericalType
import info.kwarc.mmt.imps.OperationType.OperationType
import info.kwarc.mmt.imps.Usage.Usage
import info.kwarc.mmt.imps.impsMathParser.{SortParser, SymbolicExpressionParser}

//-----------

abstract class SEXP

case class SEXPAtom(s : String) extends SEXP

case class SEXPNested(args : List[SEXP]) extends SEXP

//-----------

case class Section(name : String, dependencies : List[Section], files : List[String], jsons : List[String])

/* IMPS SORTS ETC */

abstract class IMPSSort

case class IMPSIndividual() extends  IMPSMathExp
{
  override def toString: String = "an%individual"
}

/* The atomic sorts */
case class IMPSAtomSort(s : String) extends IMPSSort
{
  override def toString: String = s
}

/* Function sorts */
case class IMPSNaryFunSort(ss : List[IMPSSort]) extends IMPSSort
{
  /* Example: What would be "qq -> qq -> qq" in Haskell
              is "[qq,qq,qq]" in IMPS. */
  override def toString: String =
  {
    var str : String = "[" + ss.head.toString

    for (s <- ss.tail)
    { str = str + "," + s.toString }

    str = str + "]^" + ss.length.toString
    str
  }
}

case class IMPSBinaryFunSort(s1 : IMPSSort, s2 : IMPSSort) extends IMPSSort
{
  override def toString : String = "[" + s1.toString + "," + s2.toString + "]"
}

case class IMPSSetSort(s : IMPSSort) extends IMPSSort
{
  override def toString: String = "sets[" + s.toString + "]"
}

case class IMPSUnknownSort(hash : Int) extends IMPSSort
{
  override def toString: String = "?" + hash + "?"
}

/* IMPS MATH EXPRESSIONS */
/* See page 64 etc. of the IMPS manual */

abstract class IMPSMathExp

//case class IMPSSymbolRef(gn : GlobalName) extends IMPSMathExp {
//  override def toString: String = gn.toString
//}

case class IMPSWith(vars : List[(IMPSVar,IMPSSort)], expression : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "with("
    assert(vars.nonEmpty)
    str = str + vars.head._1.toString + ":" + vars.head._2.toString
    for (vr <- vars.tail){
      str = str + "," + vr._1.toString + ":" + vr._2.toString
    }
    str = str + "," + expression.toString + ")"
    str
  }
}

case class IMPSMathSymbol(s : String) extends IMPSMathExp {
  override def toString: String = "<" + s + ">"
}

case class IMPSVar(v : String) extends IMPSMathExp {
  override def toString: String = "{" + v + "}"
}

case class IMPSTruth() extends IMPSMathExp
{
  override def toString: String = "truth"
}

case class IMPSFalsehood() extends IMPSMathExp
{
  override def toString: String = "falsehood"
}

case class IMPSNegation(p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "not(" + p.toString + ")"
}

case class IMPSConjunction(ps : List[IMPSMathExp]) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = ps.head.toString
    for (p <- ps.tail)
    {
      str = str + " and " + p.toString
    }
    str
  }
}

case class IMPSDisjunction(ps : List[IMPSMathExp]) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = ps.head.toString
    for (p <- ps.tail)
    {
      str = str + " or " + p.toString
    }
    str
  }
}

case class IMPSImplication(p : IMPSMathExp, q : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = p.toString + " implies " + q.toString
}

case class IMPSIff(p : IMPSMathExp, q : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = p.toString + " iff " + q.toString
}

case class IMPSIfForm(p : IMPSMathExp, q : IMPSMathExp, r : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "if_form(" + p.toString + "," + q.toString + "," + r.toString + ")"
}

case class IMPSForAll(vs : List[(IMPSVar, IMPSSort)], p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "forall("
    for ((v, s) <- vs)
    {
      str = str + v.toString
      str = str + ":" + s.toString
      str = str + ","
    }
    str = str + p.toString + ")"
    str
  }
}

case class IMPSForSome(vs : List[(IMPSVar, IMPSSort)], p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "forsome("
    for ((v, s) <- vs)
    {
      str = str + v.toString
      str = str + ":" + s.toString
      str = str + ","
    }
    str = str + p.toString + ")"
    str
  }
}

case class IMPSEquals(t1 : IMPSMathExp, t2 : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = t1.toString + "=" + t2.toString
}

case class IMPSApply(f : IMPSMathExp, ts : List[IMPSMathExp]) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = f.toString
    str = str + "@(" + ts.head.toString
    for (t <- ts.tail)
    {
      str = str + "," + t.toString
    }
    str = str + ")"
    str
  }
}

case class IMPSLambda(vs : List[(IMPSVar, IMPSSort)], t : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "lambda("
    for ((v, s) <- vs)
    {
      str = str + v.toString
      str = str + ":" + s.toString
      str = str + ","
    }
    str = str + t.toString + ")"
    str
  }
}

case class IMPSIota(v1 : IMPSVar, s1 : IMPSSort, p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "iota(" + v1.toString + ":" + s1.toString + "," + p.toString + ")"
}

case class IMPSIotaP(v1 : IMPSVar, s1 : IMPSSort, p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "iota_p(" + v1.toString + ":" + s1.toString + "," + p.toString + ")"
}

case class IMPSIf(p : IMPSMathExp, t1 : IMPSMathExp, t2 : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "if(" + p.toString + "," + t1.toString + "," + t2.toString + ")"
}

case class IMPSIsDefined(t : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "#(" + t.toString + ")"
}

case class IMPSIsDefinedIn(t : IMPSMathExp, s : IMPSSort) extends IMPSMathExp
{
  override def toString: String = "#(" + t.toString + "," + s.toString + ")"
}

case class IMPSUndefined(s : IMPSSort) extends IMPSMathExp
{
  override def toString: String = "?" + s.toString
}

/* Quasi-Constructors */

abstract class IMPSQuasiConstructor extends IMPSMathExp

case class IMPSTotal(f : IMPSMathExp, beta : IMPSSort) extends IMPSQuasiConstructor
{
  override def toString: String = "total_q(" + f.toString + ", " + beta.toString + ")"
}

case class IMPSNonVacuous(p : IMPSMathExp) extends IMPSQuasiConstructor
{
  override def toString: String = "nonvacuous_q(" + p.toString + ")"
}

case class IMPSQuasiEquals(e1 : IMPSMathExp, e2 : IMPSMathExp) extends IMPSQuasiConstructor
{
  override def toString: String = e1.toString + " == " + e2.toString
}

case class IMPSDomain(f : IMPSMathExp) extends IMPSQuasiConstructor
{
  override def toString: String = "domain(" + f.toString + ")"
}

// Not used.
case class IMPSFalseLike(b : IMPSSort) extends IMPSQuasiConstructor

/* User-defined Quasi-Constructors */

abstract class IMPSUserDefinedQuasiConstructor extends IMPSQuasiConstructor

case class IMPSQCPred2Indicator(pred : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "pred_to_indic(" + pred.toString + ")"
}

case class IMPSQCSort2Indicator(sort : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = {
    assert(sort.isInstanceOf[IMPSUndefined])
    sort match {
      case IMPSUndefined(s) => "sort_to_indic(" + s.toString + ")"
    }
  }
}

case class IMPSQCIn(e1 : IMPSMathExp, e2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = e1.toString + " in " + e2.toString
}

case class IMPSQCSubsetEQ(e1 : IMPSMathExp, e2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = e1.toString + " subseteq " + e2.toString
}

case class IMPSQCSubset(e1 : IMPSMathExp, e2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = e1.toString + " subset " + e2.toString
}

case class IMPSQCEmptyIndicator(srt : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "empty_indic{" + srt.toString + "}"
}

case class IMPSQCNonemptyIndicator(srt : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "nonempty_inidc_q{" + srt.toString + "}"
}

case class IMPSQCEmptyIndicatorQ(srt : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "empty_inidc_q{" + srt.toString + "}"
}

case class IMPSQCComplement(m : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(" + m.toString + ")^C"
}

case class IMPSQCUnion(u1 : IMPSMathExp, u2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(" + u1.toString + " union " + u2.toString + ")"
}

case class IMPSQCIntersection(i1 : IMPSMathExp, i2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(" + i1.toString + " inters " + i2.toString + ")"
}

case class IMPSQCDifference(d1 : IMPSMathExp, d2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(" + d1.toString + " diff " + d2.toString + ")"
}

case class IMPSQCSymDifference(sd1 : IMPSMathExp, sd2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(" + sd1.toString + " sym_diff " + sd2.toString + ")"
}

case class IMPSQCDisjoint(dj1 : IMPSMathExp, dj2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(" + dj1.toString + " disj " + dj2.toString + ")"
}

case class IMPSQCPartitionQ(p : IMPSMathExp, s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "partition_q{" + p.toString + "," + s.toString + "}"
}

case class IMPSQCSingleton(n : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(singleton " + n.toString + ")"
}

case class IMPSQCBigUnion(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(big_u " + f.toString + ")"
}

case class IMPSQCBigIntersection(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(big_i " + f.toString + ")"
}

case class IMPSQCMDomain(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "dom{" + f.toString + "}"
}

case class IMPSQCMComposition(g : IMPSMathExp, f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "(" + g.toString + " oo " + f.toString + ")"
}

case class IMPSQCMRange(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "ran{" + f.toString + "}"
}

case class IMPSQCMImage(f : IMPSMathExp, s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "imgage{" + f.toString + "," + s.toString + "}"
}

case class IMPSQCMInverseImage(f : IMPSMathExp, v : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "inv_image{" + f.toString + "," + v.toString + "}"
}

case class IMPSQCMInverse(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "inverse{" + f.toString + "}"
}

case class IMPSQCMId(s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "id{" + s.toString + "}"
}

case class IMPSQCMRestrict(f : IMPSMathExp, a : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "restrict{" + f.toString + "," + a.toString + "}"
}

case class IMPSQCMRestrict2(f : IMPSMathExp, a : IMPSMathExp, b : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "restrict2{" + f.toString + "," + a.toString + "," + b.toString + "}"
}

case class IMPSQCMSurjective(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "surjective_q{" + f.toString + "}"
}

case class IMPSQCMInjective(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "injective_q{" + f.toString + "}"
}

case class IMPSQCMBijective(f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "bijective_q{" + f.toString + "}"
}

case class IMPSQCMSurjectiveOn(f : IMPSMathExp, a : IMPSMathExp, b : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "surjective_on_q{" + f.toString + "," + a.toString + "," + b.toString + "}"
}

case class IMPSQCMInjectiveOn(f : IMPSMathExp, a : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "injective_on_q{" + f.toString + "," + a.toString + "}"
}

case class IMPSQCMBijectiveOn(f : IMPSMathExp, a : IMPSMathExp, b : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "bijective_on_q{" + f.toString + "," + a.toString + "," + b.toString + "}"
}

case class IMPSQCEquinumerous(a : IMPSMathExp, b : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = a.toString + " equinumerous " + b.toString
}

case class IMPSQCEmbeds(a : IMPSMathExp, b : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = a.toString + " embeds " + b.toString
}

case class IMPSQCCountableCover(f : IMPSMathExp, a : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "countable%cover{" + f.toString + "," + a.toString + "}"
}

case class IMPSQCFiniteCover(f : IMPSMathExp, a : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "finite%cover{" + f.toString + "," + a.toString + "}"
}

case class IMPSQCGroups(m : IMPSMathExp, mul : IMPSMathExp, e : IMPSMathExp, inv : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "group{" + m + "," + mul + "," + e + "," + inv + "}"
}

case class IMPSQCNil(o : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "nil{" + o.toString + "}"
}

case class IMPSQCLength(o : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "length{" + o.toString + "}"
}

case class IMPSQCFseqQ(s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "f_seq_q{" + s.toString + "}"
}

case class IMPSQCCons(e : IMPSMathExp, s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "cons{" + e.toString + "," + s.toString + "}"
}

case class IMPSQCDrop(e : IMPSMathExp, s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "drop{" + e.toString + "," + s.toString + "}"
}

case class IMPSQCTakeFirst(l : IMPSMathExp, n : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "takefirst{" + l.toString + "," + n.toString + "}"
}

case class IMPSQCAppend(s1 : IMPSMathExp, s2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "append{" + s1.toString + "," + s2.toString + "}"
}

case class IMPSQCInSeq(s1 : IMPSMathExp, s2 : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "in_seq{" + s1.toString + "," + s2.toString + "}"
}

case class IMPSQCFinCard(a : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "f_card{" + a.toString + "}"
}

case class IMPSQCFinIndic(i : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "f_indic_q{" + i.toString + "}"
}

case class IMPSQCFinSort(s : IMPSSort) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "f_sort_q{" + s.toString + "}"
}

case class IMPSQCInvariant(a : IMPSMathExp, f : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "invariant{" + a.toString + "," + f.toString + "}"
}

case class IMPSQCCollapse(s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString : String = "collapse{" + s.toString + "}"
}

case class IMPSQCConstrict(s : IMPSMathExp, a : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "constrict{" + s.toString + "," + a.toString + "}"
}

case class IMPSQCPair(p : IMPSMathExp, q : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString: String = "pair{" + p.toString + "," + q.toString + "}"
}

case class IMPSQCPairQ(s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString : String = "pair_q{" + s.toString + "}"
}

case class IMPSQCFirst(s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString : String = "first{" + s.toString + "}"
}

case class IMPSQCSecond(s : IMPSMathExp) extends IMPSUserDefinedQuasiConstructor
{
  override def toString : String = "second{" + s.toString + "}"
}

//-------------

trait DefForm
{
  var src : SourceInfo
  var cmt : CommentInfo

  def addSource(start : (Int, Int, Int), end : (Int, Int, Int)) : Unit = {
    if (this.src.isEmpty) { this.src = Some(scala.util.Left((start,end))) }
  }

  /* This is kept separate so that it doesn't need to be overwritten */
  def updateURI(uri : URI) : SourceInfo => SourceInfo =
  {
    case None => None
    case Some(scala.util.Left(((a,b,c),(x,y,z)))) => Some(scala.util.Right(SourceRef(uri,SourceRegion(SourcePosition(a,b,c),SourcePosition(x,y,z)))))
    case Some(scala.util.Right(SourceRef(_,sr))) => Some(scala.util.Right(SourceRef(uri,sr)))
  }

  def updateSource(uri : URI) : Unit = {
    val foo = updateURI(uri)
    this.src = foo(this.src)
    if (this.cmt.isDefined) { this.cmt.get.updateSource(uri) }
  }

  def addComment(c : CommentInfo) : Unit = {
    if (this.cmt.isEmpty) { this.cmt = c }
  }
}

abstract class Comp[T <: DefForm](js : List[JSONObject] = Nil) extends Logger {
  override def report    : Report = new Report()
  override def logPrefix : String = "imps-scalatypes"
  def build[S <: DefForm](args : List[Any]) : S
}

case class LineComment(s : String, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

// Arguments and whatnot

case class Name(s : String, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = s
}

case class Number(n : Int, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = n.toString
}

case class Script(s : String, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = s
}

case class DefString(s : String, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = s
}

case class ODefString(o : Either[(DefString,Option[IMPSMathExp]),Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = o.toString
}

case class ArgTheory(thy : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = { "(theory " + thy.toString + ")"}
}

case class ArgTranslation(t : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = { "(translation " + t.toString + ")"}
}

case class ArgLanguage(lang : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = { "(theory " + lang.toString + ")"}
}

case class ArgWitness(w : DefString, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = { "(witness " + w.toString + ")"}
}

/* These are all seven usages of (for example) theorems.
 * See page 77 of IMPS manual. */
object Usage extends Enumeration
{
  type Usage = Value

  val ELEMENTARYMACETE       : Usage = Value("elementary-macete")
  val TRANSPORTABLE          : Usage = Value("transportable")
  val TRANSPORTABLEMACETE    : Usage = Value("transportable-macete")
  val REWRITE                : Usage = Value("rewrite")
  val TRANSPORTABLEREWRITE   : Usage = Value("transportable-rewrite")
  val SIMPLIFYLOGICALLYFIRST : Usage = Value("simplify-logically-first")
  val DRCONVERGENCE          : Usage = Value("d-r-convergence")
  val DRVALUE                : Usage = Value("d-r-value")
}

case class ArgUsages(usgs : List[Usage], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = { "(usages " + usgs.mkString(" ") + ")"}
}

case class ArgSort(srt : IMPSSort, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = { "(sort " + srt.toString + ")"}
}

case class ArgFixedTheories(ts : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = { "(fixed-theories " + ts.mkString(" ") + ")"}
}

case class ModTransportable(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "transportable"
}

case class ModNull(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "null"
}

abstract class MaceteSpec extends DefForm

case class MSpecName(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends MaceteSpec {
  override def toString: String = nm.s
}

case class MSpecSeries(specs : List[MaceteSpec], var src : SourceInfo, var cmt : CommentInfo) extends MaceteSpec {
  override def toString: String = "(series " + specs.mkString(" ") + ")"
}

case class MSpecRepeat(specs : List[MaceteSpec], var src : SourceInfo, var cmt : CommentInfo) extends MaceteSpec {
  override def toString: String = "(repeat " + specs.mkString(" ") + ")"
}

case class MSpecSequential(specs : List[MaceteSpec], var src : SourceInfo, var cmt : CommentInfo) extends MaceteSpec {
  override def toString: String = "(sequential " + specs.mkString(" ") + ")"
}

case class MSpecParallel(specs : List[MaceteSpec], var src : SourceInfo, var cmt : CommentInfo) extends MaceteSpec {
  override def toString: String = "(parallel " + specs.mkString(" ") + ")"
}

case class MSpecSound(spec1 : MaceteSpec, spec2 : MaceteSpec, spec3 : MaceteSpec,
                      var src : SourceInfo, var cmt : CommentInfo) extends MaceteSpec
{
  override def toString: String = "(sound " + spec1 + " " + spec2 + " " + spec3 + ")"
}

case class MSpecWithoutMinorPremises(spec : MaceteSpec, var src : SourceInfo, var cmt : CommentInfo) extends MaceteSpec
{
  override def toString: String = "(without-minor-premises " + spec.toString + ")"
}

case class ArgSourceTheory(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(source-theory " + nm.toString + ")"
}

case class ArgSourceTheories(nms : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(source-theories " + nms.mkString(" ") + ")"
}

case class ArgBaseCaseHook(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(base-case-hook " + nm.toString + ")"
}

case class ArgInductionStepHook(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(induction-step-hook " + nm.toString + ")"
}

case class ArgDontUnfold(nms : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(dont-unfold " + nms.mkString(" ") + ")"
}

case class ArgInductionPrinciple(p : Either[Name,DefString], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = p match {
    case Left(Name(n,_,_)) => n
    case Right(d)          => d.toString
  }
}

case class ArgEmbeddedLang(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(embedded-language " + nm.toString + ")"
}

case class ArgEmbeddedLangs(nms : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(embedded-languages " + nms.mkString(" ") + ")"
}

case class ArgBaseTypes(nms : List[IMPSSort], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(base-types " + nms.mkString(" ") + ")"
}

case class ArgSortSpec(sub : IMPSSort, enc : IMPSSort, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + sub.toString + " " + enc.toString + ")"
}

case class ArgSorts(specs : List[ArgSortSpec], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(sorts " + specs.mkString(" ") + ")"
}

case class ArgConstantSpec(nm : Name, enc : IMPSSort, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + nm.toString + " " + enc.toString + ")"
}

case class ArgConstants(specs : List[ArgConstantSpec], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(constants " + specs.mkString(" ") + ")"
}

object NumericalType extends Enumeration
{
  type NumericalType = Value

  val INTEGERTYPE  : NumericalType = Value("*integer-type*")
  val RATIONALTYPE : NumericalType = Value("*rational-type*")
  val OCTETTYPE    : NumericalType = Value("*octet-type*")
}

case class ArgTypeSortAList(tp : NumericalType, srt : IMPSSort, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + tp.toString + " " + srt.toString + ")"
}

case class ArgExtensible(specs : List[ArgTypeSortAList], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(extensible " + specs.mkString(" ") + ")"
}

case class ArgRenamerPair(old : Name, nu : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + old.toString + " " + nu.toString + ")"
}

case class ArgPairs(ps : List[ArgRenamerPair], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(pairs " + ps.mkString(" ") + ")"
}

case class ArgToken(t : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(token " + t.toString + ")"
}

case class ArgBinding(n : Int, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(binding " + n.toString + ")"
}

case class ArgTable(tn : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(table " + tn.toString + ")"
}

/* Maybe make a better representation for this method (like below) if it ever becomes relevant.

jbetzend@turing :: 13:34:20 :: ~ > grep -r -o --no-filename "(method .*)" Development/KWARC/content/MathHub/MMT/imps/source/ | sort | uniq
(method  present-binary-infix-operator)
(method present-binary-infix-operator)
(method present-indicator-constructor-operator)
(method  present-loglike-operator)
(method present-loglike-operator)
(method present-non-associative-infix-operator)
(method present-postfix-operator)
(method present-prefix-operator)
(method present-sort-dependent-prefix-operator)
(method present-subscripted-sort-arg)
(method  present-tex-binary-infix-operator)
(method present-tex-binary-infix-operator)
(method present-tex-delimited-expression)
(method present-tex-delimited-expression-with-dots)
(method PRESENT-TEX-differentiation)
(method present-tex-direct-image-operator)
(method present-tex-id-operator)
(method present-tex-indicator-constructor-operator)
(method present-tex-interval-iteration-operator)
(method present-tex-inverse-image-operator)
(method present-tex-inverse-operator)
(method present-tex-prefix-operator)
(method PRESENT-TEX-RAISE)
(method present-tex-sort-dependent-prefix-operator)
(method present-tex-stack-label)
(method present-tex-symbol)

 */

case class ArgMethod(mn : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(method " + mn.toString + ")"
}

object ParseMethod extends Enumeration
{
  type ParseMethod = Value

  val PREFIXMETHOD   : ParseMethod = Value("prefix-operator-method")
  val INFIXMETHOD    : ParseMethod = Value("infix-operator-method")
  val POSTFIXMETHOD  : ParseMethod = Value("postfix-operator-method")
  val NEGATIONMETHOD : ParseMethod = Value("negation-operator-method")
  val TABLEMETHOD    : ParseMethod = Value("table-operator-method")
  val INDBOTHSYN     : ParseMethod = Value("parse-indicator-constructor-both-syntaxes")
  val PREFSORTDEPOM  : ParseMethod = Value("prefix-sort-dependent-operator-method")
  val NULLCALL       : ParseMethod = Value("null-call-method-terminator")
}

case class ArgLeftMethod(mn : ParseMethod, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(left-method " + mn.toString + ")"
}

case class ArgNullMethod(mn : ParseMethod, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(null-method " + mn.toString + ")"
}

case class ModTex(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "tex"
}

case class ModReverse(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "reverse"
}

case class ModLemma(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "lemma"
}

case class ArgMacete(mn : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(macete " + mn.toString + ")"
}

case class ArgHomeTheory(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(home-theory " + nm.toString + ")"
}

case class ArgProof(prf : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(proof " + prf.toString + ")"
}

case class ArgComponentTheories(cps : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(component-theories " + cps.mkString(" ") + ")"
}

case class ArgDistinctConstants(ds : List[List[Name]], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = {
    var str = "(distinct-constants"
    for (d <- ds) {
      str = str + " (" + d.mkString(" ") +  ")"
    }
    str = str + ")"
    str
  }
}

case class AxiomSpec(name : Option[String], defstr : DefString, frm : Option[IMPSMathExp], usgs : Option[List[Usage]],
                     var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = {
    "(" + (if (name.isDefined) {name.get + " "} else {""}) + defstr.toString + (if (usgs.isDefined) {" " + usgs.get.mkString(" ")} else {""}) + ")"
  }
}

case class ArgAxioms(cps : List[AxiomSpec], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(axioms " + cps.mkString(" ") + ")"
}

case class ArgOverloadingPair(tname : Name, sname : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + tname.toString + " " + sname.toString + ")"
}

case class ModForce(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "force"
}

case class ModForceUnderQuickLoad(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "force-under-quick-load"
}

case class ModDontEnrich(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "dont-enrich"
}

case class ArgSource(var thy : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(source " + thy.toString + ")"
}

case class ArgTarget(var thy : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(target " + thy.toString + ")"
}

case class ArgAssumptions(defs : List[DefString], frms : List[IMPSMathExp], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(assumptions " + defs.mkString(" ") + ")"
}


case class ArgSortPairSpec(name : Name, srt : Either[Either[Name,DefString],Either[DefString,DefString]], mth : Option[IMPSMathExp], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {

  def mapPrint[A,B,C,D](e : Either[Either[A,B],Either[C,D]]) : String = {
    e match {
      case Left(Left(value))   => value.toString
      case Left(Right(value))  => value.toString
      case Right(Left(value))  => value.toString
      case Right(Right(value)) => value.toString
    }
  }

  override def toString: String = {
    "(" + name.toString + " " + mapPrint(srt) + ")"
  }
}

case class ArgSortPairs(defs : List[ArgSortPairSpec], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(sort-pairs " + defs.mkString(" ") + ")"
}

case class ArgConstPairSpec(name : Name, const : ODefString, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + name.toString + " " + const.toString + ")"
}

case class ArgConstPairs(defs : List[ArgConstPairSpec], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(constant-pairs " + defs.mkString(" ") + ")"
}

case class ArgCoreTranslation(var tr : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(core-translation " + tr.toString + ")"
}

case class ArgTheoryInterpretationCheck(var cm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(theory-interpretation-check " + cm.toString + ")"
}

case class ArgDefinitionName(var dn : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(definition-name " + dn.toString + ")"
}

case class ArgNameList(var nms : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = if (nms.length == 1) { nms.head.toString } else { "(" + nms.mkString(" ") + ")"}
}

case class ArgDefStringList(var dfs : List[DefString], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = if (dfs.length == 1) { dfs.head.toString } else { "(" + dfs.mkString(" ") + ")"}
}

case class ArgRenamer(var rn : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(renamer " + rn.toString + ")"
}

case class ModCancellative(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "cancellative"
}

object OperationType extends Enumeration
{
  type OperationType = Value

  val PLUS  : OperationType = Value("+")
  val TIMES : OperationType = Value("*")
  val MINUS : OperationType = Value("-")
  val DIV   : OperationType = Value("/")
  val EXP   : OperationType = Value("^")
  val SUB   : OperationType = Value("sub")
  val ZERO  : OperationType = Value("zero")
  val UNIT  : OperationType = Value("unit")
}

case class ArgOperationAlist(optype : OperationType, op : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(" + optype.toString + " " + op.toString + ")"
}

case class ArgOperations(defs : List[ArgOperationAlist], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(operations " + defs.mkString(" ") + ")"
}

case class ArgScalars(ntype : NumericalType, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(scalars " + ntype.toString + ")"
}

case class ModCommutes(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "commutes"
}

case class ModUseNumerals(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "use-numerals-for-ground-terms"
}

case class ArgSpecForms(specform : Either[Name,QDFSpecForm], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = specform match {
    case Left(n)  => n.toString
    case Right(s) => s.toString
  }
}

case class ArgBase(sf : ArgSpecForms, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(base " + sf.toString + ")"
}

case class ArgExponent(sf : ArgSpecForms, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(exponent " + sf.toString + ")"
}

case class ArgCoefficient(sf : ArgSpecForms, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(coefficient " + sf.toString + ")"
}

case class ArgRetrievalProtocol(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(retrieval-protocol " + nm.toString + ")"
}

case class ArgApplicabilityRecognizer(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(applicability-recognizer " + nm.toString + ")"
}

case class ArgComponentSections(nms : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(component-sections " + nms.mkString(" ") + ")"
}

case class ArgFileSpec(nm1 : Name, nm2 : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(" + nm1.toString + " " + nm2.toString + ")"
}

case class ArgFiles(nms : List[ArgFileSpec], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(files " + nms.mkString(" ") + ")"
}

case class ArgBaseTheory(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(base-theory " + nm.toString + ")"
}

case class ArgReplicaRenamer(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(replica-renamer " + nm.toString + ")"
}

case class ArgNumbers(ns : List[Number], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(" + ns.mkString(" ") + ")"
}

case class ArgPermutations(nns : List[List[Number]], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = {
    var str = "(permutations"
    for (p <- nns) {
      str = str + " (" + p.mkString(" ") + ")"
    }
    str = str + ")"
    str
  }
}

case class ArgMultiples(ns : List[Number], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(multiples " + ns.mkString(" ") + ")"
}

case class ArgSpecialRenamings(ns : List[ArgRenamerPair], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(special-renamings " + ns.mkString(" ") + ")"
}

case class ArgTargetTheories(ns : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(target-theories " + ns.mkString(" ") + ")"
}

case class ArgTargetMultiple(n : Number, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(target-multiple " + n.toString + ")"
}

case class ArgSortAssoc(nm : Name, sorts : List[ODefString], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "( " + nm.toString + sorts.map(k => k.o match {
    case Left(d)  => " " + d.toString
    case Right(n) => " " + n.toString
  }) + ")"
}

case class ArgEnsembleSorts(sorts : List[ArgSortAssoc], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(sorts " + sorts.mkString(" ") + ")"
}

case class ArgConstAssoc(nm : Name, consts : List[ODefString], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "( " + nm.toString + consts.map(k => k.o match {
    case Left(d)  => " " + d.toString
    case Right(n) => " " + n.toString
  }) + ")"
}

case class ArgEnsembleConsts(consts : List[ArgConstAssoc], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(constants " + consts.mkString(" ") + ")"
}

case class ArgNewTranslationName(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString : String = "(new-translation-name " + nm.toString + ")"
}

case class ArgAlgebraicSimplifierSpec(names : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + names.mkString(" ") + ")"
}

case class ArgAlgebraicSimplifier(specs : List[ArgAlgebraicSimplifierSpec],
                                  var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(algebraic-simplifier" + specs.mkString(" ") + ")"
}


case class ArgAlgebraicOrderSimplifierSpec(names : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + names.mkString(" ") + ")"
}

case class ArgAlgebraicOrderSimplifier(specs : List[ArgAlgebraicOrderSimplifierSpec],
                                       var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(algebraic-order-simplifier" + specs.mkString(" ") + ")"
}

case class ArgAlgebraicTermComparator(specs : List[Name], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(algebraic-term-comparator" + specs.mkString(" ") + ")"
}

case class ArgAlgebraicProcessor(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(algebraic-processor" + nm.toString + ")"
}

case class ArgDiscreteSorts(srts : List[IMPSSort], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(discrete-sorts" + srts.mkString(" ") + ")"
}

case class ArgOperationsAlist(optype : Name, op : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + optype.toString + " " + op.toString + ")"
}

case class ArgProcOperations(alists : List[ArgOperationsAlist], var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(operations " + alists.mkString(" ") + ")"
}

case class ArgSet(cont : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(set " + cont.toString + ")"
}

case class Define(cont : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(define " + cont.toString + ")"
}

case class ModReload(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "reload"
}

case class ModQuickLoad(var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "quick-load"
}

case class TeXCorrespondence(scrpt : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(make-tex-correspondence " + scrpt.toString + ")"
}

case class QCConstantlike(nm : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(make-quasi-constructor-constantlike " + nm.toString + ")"
}

case class EnsembleDontTranslateConst(nm : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(ensemble-dont-translate-constant " + nm.toString + ")"
}

case class ArbitraryScript(opener : String, scrpt : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(" + opener + " " + scrpt.toString + ")"
}

case class Simplog1stWrapper(d : DefForm, var src : SourceInfo, var cmt : CommentInfo) extends DefForm {
  override def toString: String = "(transportable-rewrite-usage-simplog1st " + d.toString + ")"
}

// Full DefForms

case class Heralding(name : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object Heralding extends Comp[Heralding] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (nm : Name @unchecked) :: Nil => Heralding(nm, None, None).asInstanceOf[T]
    case _ => ??!(args)
  }
}


case class DFAtomicSort(name : Name, dfs : DefString, frm : IMPSMathExp, sort : IMPSSort,
                        thy : ArgTheory, usgs : Option[ArgUsages], wtn : Option[ArgWitness],
                        var src : SourceInfo, var cmt : CommentInfo) extends DefForm

class DFAtomicSortC(js : List[JSONObject]) extends Comp[DFAtomicSort] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (d : DefString @unchecked) :: (t : Option[ArgTheory] @unchecked)
      :: (u : Option[ArgUsages] @unchecked) :: (w : Option[ArgWitness] @unchecked) :: Nil =>

      val frm : IMPSMathExp = FrmFnd.findFormula(t.get.thy.s,Some(d),"defsorts","",Some(n.s),js)
      var srt : IMPSSort    = FrmFnd.findSort(t.get.thy.s,d,"defsorts",Some(n.s),js)
      assert(srt.isInstanceOf[IMPSBinaryFunSort] | srt.isInstanceOf[IMPSNaryFunSort])
      srt match {
        case IMPSBinaryFunSort(s1,prop) => srt = s1
        case IMPSNaryFunSort(srts) => assert(srts.length == 2) ; srt = srts.head
        case _ => ??!(srt)
      }

      DFAtomicSort(n,d,frm,srt,t.get,u,w,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFConstant(name : Name, dfs : DefString, frm : IMPSMathExp, sort : IMPSSort, thy : ArgTheory,
                      srt : Option[ArgSort], usgs : Option[ArgUsages],
                      var src : SourceInfo, var cmt : CommentInfo) extends DefForm

class DFConstantC(js : List[JSONObject]) extends Comp[DFConstant] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (d : DefString @unchecked) :: (thy : Option[ArgTheory] @unchecked)
      :: (s : Option[ArgSort] @unchecked) :: (u : Option[ArgUsages] @unchecked) :: Nil =>

      val frm : IMPSMathExp = FrmFnd.findFormula(thy.get.thy.s,Some(d),"defconsts","",Some(n.s),js)
      val srt : IMPSSort    = FrmFnd.findSort(thy.get.thy.s,d,"defconsts",Some(n.s),js)

      DFConstant(n,d,frm,srt,thy.get,s,u,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFQuasiConstructor(name : Name, dfs : DefString,
                              lan  : ArgLanguage, fx : Option[ArgFixedTheories],
                              var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFQuasiConstructor extends Comp[DFQuasiConstructor] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (d : DefString @unchecked) :: (t : Option[ArgLanguage] @unchecked)
      :: (f : Option[ArgFixedTheories] @unchecked) :: Nil => DFQuasiConstructor(n,d,t.get,f,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFSchematicMacete(name : Name, dfs : DefString, nil : Option[ModNull],
                             trans : Option[ModTransportable], t : ArgTheory,
                             var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFSchematicMacete extends Comp[DFSchematicMacete] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (d : DefString @unchecked) :: (nl : Option[ModNull] @unchecked)
      :: (tr : Option[ModTransportable] @unchecked) :: (t : Option[ArgTheory] @unchecked) :: Nil =>
      DFSchematicMacete(n,d,nl,tr,t.get,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFCompoundMacete(nm : Name, spec : MaceteSpec, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFCompoundMacete extends Comp[DFCompoundMacete] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (s : MaceteSpec @unchecked) :: Nil => DFCompoundMacete(n,s,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFImportedRewriteRules(nm : Name, thy : Option[ArgSourceTheory], thies : Option[ArgSourceTheories],
                                  var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFImportedRewriteRules extends Comp[DFImportedRewriteRules] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (thy : Option[ArgSourceTheory] @unchecked)
      :: (thies : Option[ArgSourceTheories] @unchecked) :: Nil =>
      DFImportedRewriteRules(n,thy,thies,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFInductor(nm : Name, princ : ArgInductionPrinciple, thy : ArgTheory,
                      trans : Option[ArgTranslation], bh : Option[ArgBaseCaseHook],
                      ih : Option[ArgInductionStepHook], du : Option[ArgDontUnfold],
                      var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFInductor extends Comp[DFInductor] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (p : ArgInductionPrinciple @unchecked) :: (thy : Option[ArgTheory] @unchecked)
      :: (tr : Option[ArgTranslation] @unchecked) :: (bh : Option[ArgBaseCaseHook] @unchecked)
      :: (ih : Option[ArgInductionStepHook] @unchecked) :: (du : Option[ArgDontUnfold] @unchecked) :: Nil =>
      DFInductor(n,p,thy.get,tr,bh,ih,du,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFLanguage(name : Name, els : Option[ArgEmbeddedLangs], el : Option[ArgEmbeddedLang],
                      bt : Option[ArgBaseTypes], srts : Option[ArgSorts],
                      ex : Option[ArgExtensible], cnsts : Option[ArgConstants],
                      var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFLanguage extends Comp[DFLanguage] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (els : Option[ArgEmbeddedLangs] @unchecked)
      :: (el : Option[ArgEmbeddedLang] @unchecked) :: (bt : Option[ArgBaseTypes] @unchecked)
      :: (srts : Option[ArgSorts] @unchecked) :: (ex : Option[ArgExtensible] @unchecked)
      :: (cnsts : Option[ArgConstants] @unchecked) :: Nil =>
      DFLanguage(n,els,el,bt,srts,ex,cnsts,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFRenamer(nm : Name, ps : Option[ArgPairs], var src : SourceInfo, var cmt : CommentInfo) extends DefForm
{
  def toFunction : String => String = {
    if (ps.isDefined) { (x : String) => applyRenaming(x,ps.get.ps) } else { identity }
  }

  def applyRenaming(in : String, res : List[ArgRenamerPair]) : String = {
    if (res.isEmpty) {in} else {if (res.head.old.s == in) {res.head.nu.s} else {applyRenaming(in,res.tail)}}
  }
}

object DFRenamer extends Comp[DFRenamer] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (ps : Option[ArgPairs] @unchecked) :: Nil => DFRenamer(n,ps,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFParseSyntax(n : Name, t : Option[ArgToken], lm : Option[ArgLeftMethod],
                         nm : Option[ArgNullMethod], tbl : Option[ArgTable],
                         bnd : ArgBinding, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFParseSyntax extends Comp[DFParseSyntax] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (t : Option[ArgToken] @unchecked) :: (lm : Option[ArgLeftMethod] @unchecked)
      :: (nm : Option[ArgNullMethod] @unchecked) :: (tbl : Option[ArgTable] @unchecked)
      :: (bnd : Option[ArgBinding] @unchecked) :: Nil => DFParseSyntax(n,t,lm,nm,tbl,bnd.get,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFPrintSyntax(n : Name, tex : Option[ModTex], t : Option[ArgToken],
                         m : Option[ArgMethod], tbl : Option[ArgTable], bnd : ArgBinding,
                         var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFPrintSyntax extends Comp[DFPrintSyntax] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (tex : Option[ModTex] @unchecked) :: (t : Option[ArgToken] @unchecked)
      :: (m : Option[ArgMethod] @unchecked) :: (tbl : Option[ArgTable] @unchecked)
      :: (bnd : Option[ArgBinding] @unchecked) :: Nil => DFPrintSyntax(n,tex,t,m,tbl,bnd.get,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFTheorem(n : Name, defn : ODefString, frm : Option[IMPSMathExp], modr : Option[ModReverse],
                     modl : Option[ModLemma], t : ArgTheory, us : Option[ArgUsages], tr : Option[ArgTranslation],
                     mac : Option[ArgMacete], ht : Option[ArgHomeTheory], prf : Option[ArgProof],
                     var src : SourceInfo, var cmt : CommentInfo) extends DefForm

class DFTheoremC(js : List[JSONObject]) extends Comp[DFTheorem] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (df : ODefString @unchecked) :: (modr : Option[ModReverse] @unchecked)
      :: (modl : Option[ModLemma] @unchecked) :: (thy : Option[ArgTheory] @unchecked)
      :: (us : Option[ArgUsages] @unchecked) :: (tr : Option[ArgTranslation] @unchecked)
      :: (mac : Option[ArgMacete] @unchecked) :: (ht : Option[ArgHomeTheory] @unchecked)
      :: (prf : Option[ArgProof] @unchecked) :: Nil
    =>
      // Construct full theorem!
      val formula : IMPSMathExp = df.o match {
        case Left((defstring,None))                         => FrmFnd.findFormula(thy.get.thy.s,Some(defstring),"theorems","axioms",Some(n.s),js)
        case Left((_,Some(thef)))                           => thef
        case Right(Name(k@"right-act-inv",_,_))             => FrmFnd.findFormula("group-actions",None,"theorems","axioms",Some(k),js)
        case Right(Name(k@"left-act-inv",_,_))              => FrmFnd.findFormula("group-actions",None,"theorems","axioms",Some(k),js)
        case Right(Name(k@"action-macete",_,_))             => FrmFnd.findFormula("group-actions",None,"theorems","axioms",Some(k),js)
        case Right(Name(k@"reverse-act-associativity",_,_)) => FrmFnd.findFormula("group-actions",None,"theorems","axioms",Some(k),js)
        case Right(Name(k@"finiteness-of-zeta-orbit",_,_))  => FrmFnd.findFormula("counting-theorem-theory",None,"theorems","axioms",Some(k),js)
        case Right(name)                                    => FrmFnd.findFormula(thy.get.thy.s,None,"theorems","axioms",Some(name.s),js)
      }
      DFTheorem(n,df,Some(formula),modr,modl,thy.get,us,tr,mac,ht,prf,None,None).asInstanceOf[T]

    case _ => ??!(args)
  }
}

case class DFTheory(name : Name, lang : Option[ArgLanguage], comp : Option[ArgComponentTheories],
                    axioms : Option[ArgAxioms], dstnct : Option[ArgDistinctConstants],
                    var src : SourceInfo, var cmt : CommentInfo) extends DefForm

class DFTheoryC(js : List[JSONObject]) extends Comp[DFTheory] {

  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (lang : Option[ArgLanguage] @unchecked)
      :: (comp : Option[ArgComponentTheories] @unchecked) :: (axs : Option[ArgAxioms] @unchecked)
      :: (dscs : Option[ArgDistinctConstants] @unchecked) :: Nil =>

      val nuaxs : Option[ArgAxioms] = if (axs.isDefined) {
        var nu : List[AxiomSpec] = Nil
        for (ax <- axs.get.cps) {
          val found = FrmFnd.findAxiom(n.s, ax, js)
          nu = nu ::: List(AxiomSpec(ax.name,ax.defstr,Some(found),ax.usgs,ax.src,ax.cmt))
        }
        Some(ArgAxioms(nu,axs.get.src,axs.get.cmt))
      } else { None }

      DFTheory(n,lang,comp,nuaxs,dscs,None,None).asInstanceOf[T]

    case _ => ??!(args)
  }
}

case class DFOverloading(n : Name, ps : List[ArgOverloadingPair], var src : SourceInfo, var cmt : CommentInfo)
  extends DefForm

object DFOverloading extends Comp[DFOverloading] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (ps : List[ArgOverloadingPair] @unchecked) :: Nil =>
      DFOverloading(n,ps,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFTranslation(n : Name, f : Option[ModForce], fu : Option[ModForceUnderQuickLoad], de : Option[ModDontEnrich],
                         sour : ArgSource, tar : ArgTarget, asms : Option[ArgAssumptions], fxd : Option[ArgFixedTheories],
                         sps : Option[ArgSortPairs], cps : Option[ArgConstPairs], ct : Option[ArgCoreTranslation],
                         tic : Option[ArgTheoryInterpretationCheck], var src : SourceInfo, var cmt : CommentInfo)
  extends DefForm

class DFTranslationC(js : List[JSONObject]) extends Comp[DFTranslation] {

  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (n : Name @unchecked) :: (f : Option[ModForce] @unchecked) :: (fu : Option[ModForceUnderQuickLoad] @unchecked)
      :: (de  : Option[ModDontEnrich] @unchecked) :: (sour : Option[ArgSource] @unchecked)
      :: (tar : Option[ArgTarget] @unchecked) :: (asms : Option[ArgAssumptions] @unchecked)
      :: (fxd : Option[ArgFixedTheories] @unchecked) :: (sps : Option[ArgSortPairs] @unchecked)
      :: (cps : Option[ArgConstPairs] @unchecked) :: (ct : Option[ArgCoreTranslation] @unchecked)
      :: (tic : Option[ArgTheoryInterpretationCheck] @unchecked) :: Nil =>

      var formulas : List[IMPSMathExp] = Nil
      var nu_cps : Option[ArgConstPairs] = None
      var nu_sps : Option[ArgSortPairs]  = None

      if (sps.isDefined)
      {
        var nu_sps_defs : List[ArgSortPairSpec] = Nil

        for (sp <- sps.get.defs.indices)
        {
          var needle : String = ""

          sps.get.defs(sp).srt match {
            case scala.util.Left(scala.util.Left(_))           => nu_sps_defs = nu_sps_defs ::: List(sps.get.defs(sp))
            case scala.util.Left(scala.util.Right(the_string)) => needle = removeWhitespace(the_string.s.tail.init)
            case scala.util.Right(scala.util.Left(the_pred))   => needle = "(pred " + removeWhitespace(the_pred.s.tail.init) + ")"
            case scala.util.Right(scala.util.Right(the_indic)) => needle = "(indic " + removeWhitespace(the_indic.s.tail.init) + ")"
          }

          if (needle.nonEmpty)
          {
            val json_theory2 : Option[JSONObject] = js.find(j => j.getAsString("name") == tar.get.thy.s.toLowerCase)
            assert(json_theory2.isDefined)
            assert(json_theory2.get.getAsString("type") == "imps-theory")

            val group2   : List[JSONObject] = json_theory2.get.getAsList(classOf[JSONObject],"translations")
            val haystack : List[JSONObject] = group2.distinct
            assert(haystack.nonEmpty)

            var theThing : Option[JSONObject] = None

            // Search by string
            if (theThing.isEmpty)
            {
              // At this point, we need the actual def-string
              assert(needle.nonEmpty)

              // unmodified string
              needle = removeWhitespace(needle)
              var tempt = haystack.find(j => j.getAsList(classOf[JSONString],"sort-pairs").map(_.value.toLowerCase).map(a => removeWhitespace(a.dropWhile(c => !c.isWhitespace).trim.init)).contains(needle))

              // if that didn't work, handpicked string
              if (tempt.isEmpty) {
                val handpicked = handpick(needle)
                tempt = haystack.find(j => j.getAsList(classOf[JSONString],"sort-pairs").map(_.value.toLowerCase).map(a => removeWhitespace(a.dropWhile(c => !c.isWhitespace).trim.init)).contains(handpicked))
              }
              theThing = tempt
            }

            if (theThing.isEmpty)
            {
              assert(needle != "")
              logError("> needle     = " + needle)
              logError("> handpicked = " + handpick(needle))

              logError("Candidates:")
              for (t <- haystack) {
                for (a <- t.getAsList(classOf[JSONString],"sort-pairs").map(_.value.toLowerCase)) {
                  logError(removeWhitespace(a.dropWhile(c => !c.isWhitespace).trim.init))
                }
              }
            }

            assert(theThing.isDefined)
            val thesexp : List[String] = theThing.get.getAsList(classOf[String],"sort-pairs-sexp")
            assert(thesexp.nonEmpty)

            val sep : SymbolicExpressionParser = new SymbolicExpressionParser
            val lsp = sep.parseAll(sep.parseSEXP,thesexp(sp).toLowerCase.dropWhile(c => !c.isWhitespace).init.trim)
            assert(lsp.successful)

            val foo = Some(impsMathParser.makeSEXPFormula(lsp.get))

            nu_sps_defs = nu_sps_defs ::: List(ArgSortPairSpec(sps.get.defs(sp).name,sps.get.defs(sp).srt,foo,sps.get.defs(sp).src,sps.get.defs(sp).cmt))
          }
        }
        nu_sps = Some(ArgSortPairs(nu_sps_defs,sps.get.src,sps.get.cmt))
      }

      if (cps.isDefined)
      {
        var nu_cps_defs : List[ArgConstPairSpec] = Nil

        for (cp <- cps.get.defs.indices)
        {
          cps.get.defs(cp).const.o match {
            case scala.util.Right(_)
               | scala.util.Left((_,Some(_))) => nu_cps_defs = nu_cps_defs ::: List(cps.get.defs(cp))
            case scala.util.Left((dfs,None))  =>

              val json_theory2 : Option[JSONObject] = js.find(j => j.getAsString("name") == tar.get.thy.s.toLowerCase)
              assert(json_theory2.isDefined)
              assert(json_theory2.get.getAsString("type") == "imps-theory")

              val group2   : List[JSONObject] = json_theory2.get.getAsList(classOf[JSONObject],"translations")
              val haystack : List[JSONObject] = group2.distinct
              assert(haystack.nonEmpty)

              var needle : String = dfs.s.tail.init
              var theThing : Option[JSONObject] = None

              // Search by string
              if (theThing.isEmpty)
              {
                // At this point, we need the actual def-string
                assert(needle.nonEmpty)

                // unmodified string
                needle = removeWhitespace(needle)
                var tempt = haystack.find(j => j.getAsList(classOf[JSONString],"constant-pairs").map(_.value.toLowerCase).map(a => removeWhitespace(a.dropWhile(c => !c.isWhitespace).trim.init)).contains(needle))

                // if that didn't work, handpicked string
                if (tempt.isEmpty) {
                  val handpicked = handpick(needle)
                  tempt = haystack.find(j => j.getAsList(classOf[JSONString],"constant-pairs").map(_.value.toLowerCase).map(a => removeWhitespace(a.dropWhile(c => !c.isWhitespace).trim.init)).contains(handpicked))
                }
                theThing = tempt
              }

              if (theThing.isEmpty)
              {
                assert(needle != "")
                logError("> needle     = " + needle)
                logError("> handpicked = " + handpick(needle))

                logError("Candidates:")
                for (t <- haystack)
                {
                  for (a <- t.getAsList(classOf[JSONString],"constant-pairs").map(_.value.toLowerCase)) {
                    println(removeWhitespace(a.dropWhile(c => !c.isWhitespace).trim.init))
                  }
                }
              }

              assert(theThing.isDefined)
              val thesexp : List[String] = theThing.get.getAsList(classOf[String],"constant-pairs-sexp")
              assert(thesexp.nonEmpty)

              val sp : SymbolicExpressionParser = new SymbolicExpressionParser
              val lsp = sp.parseAll(sp.parseSEXP,thesexp(cp).toLowerCase.dropWhile(c => !c.isWhitespace).init.trim)
              assert(lsp.successful)

              nu_cps_defs = nu_cps_defs ::: List(ArgConstPairSpec(cps.get.defs(cp).name,ODefString(scala.util.Left((dfs,Some(impsMathParser.makeSEXPFormula(lsp.get)))),None,None),None,None))
          }
        }

        nu_cps = Some(ArgConstPairs(nu_cps_defs,cps.get.src,cps.get.cmt))
      }

      if (asms.isDefined)
      {
        val json_theory1 : Option[JSONObject] = js.find(j => j.getAsString("name") == sour.get.thy.s.toLowerCase)
        assert(json_theory1.isDefined)
        assert(json_theory1.get.getAsString("type") == "imps-theory")

        val json_theory2 : Option[JSONObject] = js.find(j => j.getAsString("name") == tar.get.thy.s.toLowerCase)
        assert(json_theory2.isDefined)
        assert(json_theory2.get.getAsString("type") == "imps-theory")

        val group1   : List[JSONObject] = json_theory1.get.getAsList(classOf[JSONObject],"translations")
        val group2   : List[JSONObject] = json_theory2.get.getAsList(classOf[JSONObject],"translations")
        val haystack : List[JSONObject] = (group1 ::: group2).distinct
        assert(haystack.nonEmpty)

        for (as <- asms.get.defs.indices)
        {
          var needle : String = asms.get.defs(as).s.tail.init
          var theThing : Option[JSONObject] = None

          // Search by string
          if (theThing.isEmpty)
          {
            // At this point, we need the actual def-string
            assert(needle.nonEmpty)

            // unmodified string
            needle = removeWhitespace(needle)
            var tempt = haystack.find(j => j.getAsList(classOf[JSONString],"assumptions").map(_.value.toLowerCase).map(a => removeWhitespace(a)).contains(needle))

            // if that didn't work, handpicked string
            if (tempt.isEmpty) {
              val handpicked = handpick(needle)
              tempt = haystack.find(j => j.getAsList(classOf[JSONString],"assumptions").map(_.value.toLowerCase).map(a => removeWhitespace(a)).contains(handpicked))
            }
            theThing = tempt
          }


          if (theThing.isEmpty)
          {
            assert(needle != "")
            logError("> needle     = " + needle)
            logError("> handpicked = " + handpick(needle))

            for (t <- haystack)
            {
              logError("Candidates:")
              for (a <- t.getAsList(classOf[JSONString],"assumptions").map(_.value.toLowerCase)) {
                logError(removeWhitespace(a))
              }
            }
          }

          assert(theThing.isDefined)
          val thesexp : List[String] = theThing.get.getAsList(classOf[String],"assumptions-sexp")
          assert(thesexp.nonEmpty)

          val sp : SymbolicExpressionParser = new SymbolicExpressionParser
          val lsp = sp.parseAll(sp.parseSEXP,thesexp(as).toLowerCase)
          assert(lsp.successful)

          formulas = formulas ::: List(impsMathParser.makeSEXPFormula(lsp.get))
        }
      }

      val nuAsms : Option[ArgAssumptions] = if (asms.isDefined) { Some(asms.get.copy(frms = formulas)) } else { None }

      DFTranslation(n, f, fu, de, sour.get, tar.get, nuAsms, fxd, nu_sps, nu_cps, ct, tic, None, None).asInstanceOf[T]

    case _ => ??!(args)
  }
}

case class DFRecursiveConstant(ns : ArgNameList, defs : ArgDefStringList, frms : List[IMPSMathExp], srts : List[IMPSSort],
                               thy : ArgTheory, usgs : Option[ArgUsages], defname : Option[ArgDefinitionName],
                               var src : SourceInfo, var cmt : CommentInfo) extends DefForm

class DFRecursiveConstantC(js : List[JSONObject]) extends Comp[DFRecursiveConstant] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (ns : ArgNameList @unchecked) :: (defs : ArgDefStringList @unchecked) :: (thy : Option[ArgTheory] @unchecked)
      :: (usgs : Option[ArgUsages] @unchecked) :: (defname : Option[ArgDefinitionName] @unchecked) :: Nil =>

      var frms : List[IMPSMathExp] = Nil
      var srts : List[IMPSSort] = Nil

      val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == thy.get.thy.s)
      assert(json_theory.isDefined)
      assert(json_theory.get.getAsString("type") == "imps-theory")

      val recconsts : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"def-recursive-consts")
      assert(recconsts.nonEmpty)

      val theconst : Option[JSONObject] = recconsts.find(j => j.getAsList(classOf[JSONString], "names").map(_.value.toLowerCase).contains(ns.nms.map(_.s).head.toLowerCase))
      assert(theconst.isDefined)
      assert(theconst.get.getAsString("type") == "imps-theory-recursive-constant")

      val thesexps = theconst.get.getAsList(classOf[JSONString],"defining-sexps")
      assert(thesexps.nonEmpty)
      var realsexps : List[String] = thesexps.map(_.value)
      if (thesexps.lengthCompare(ns.nms.length) != 0)
      {
        val nusexps = thesexps.head.value.split(", ")
        assert(nusexps.lengthCompare(ns.nms.length) == 0)
        realsexps = nusexps.toList
      }

      val sp : SymbolicExpressionParser = new SymbolicExpressionParser

      for (k <- ns.nms.indices)
      {
        val thesexp = realsexps(k)
        val lsp = sp.parseAll(sp.parseSEXP,thesexp)
        assert(lsp.successful)

        val defexp = impsMathParser.makeSEXPFormula(lsp.get)
        frms = frms ::: List(defexp)

        var thesorts  : List[String] = theconst.get.getAsList(classOf[JSONString],"sortings").map(g => g.value)

        if (thesorts.length < ns.nms.length) {
          assert(thesorts.length == 1)
          thesorts = thesorts.head.split(", ").toList
        }

        assert(thesorts.nonEmpty)
        assert(thesorts.lengthCompare(ns.nms.length) == 0)

        val sop = new SortParser()
        val losp = sop.parseAll(sop.parseSort,thesorts(k))
        assert(losp.successful)

        srts = srts ::: List(losp.get)
      }

      assert(frms.nonEmpty)
      assert(srts.nonEmpty)
      assert(ns.nms.length == frms.length)

      DFRecursiveConstant(ns,defs,frms,srts,thy.get,usgs,defname,None,None).asInstanceOf[T]

    case _ => ??!(args)
  }
}

case class DFTransportedSymbols(ns : ArgNameList, tr : ArgTranslation, rn : Option[ArgRenamer],
                                var src : SourceInfo, var cmt : CommentInfo) extends DefForm


object DFTransportedSymbols extends Comp[DFTransportedSymbols] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (ns : ArgNameList @unchecked) :: (tr : Option[ArgTranslation] @unchecked)
      :: (rn : Option[ArgRenamer] @unchecked) :: Nil => DFTransportedSymbols(ns,tr.get,rn,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class QDFSpecForm(sclrs : Option[ArgScalars], ops : Option[ArgOperations],
                       usenum : Option[ModUseNumerals], comm : Option[ModCommutes],
                       var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object QDFSpecForm extends Comp[QDFSpecForm] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (sclrs : Option[ArgScalars] @unchecked) :: (ops : Option[ArgOperations] @unchecked)
      :: (usenum : Option[ModUseNumerals] @unchecked) :: (comm : Option[ModCommutes] @unchecked) :: Nil =>
      QDFSpecForm(sclrs,ops,usenum,comm,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFAlgebraicProcessor(nm : Name, canc : Option[ModCancellative], lang : ArgLanguage,
                                bs : ArgBase, ex : Option[ArgExponent], co : Option[ArgCoefficient],
                                var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFAlgebraicProcessor extends Comp[DFAlgebraicProcessor] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (nm : Name @unchecked) :: (canc : Option[ModCancellative] @unchecked)
      :: (lang : Option[ArgLanguage] @unchecked) :: (bs : Option[ArgBase] @unchecked)
      :: (ex : Option[ArgExponent] @unchecked) :: (co : Option[ArgCoefficient] @unchecked) :: Nil =>
      DFAlgebraicProcessor(nm,canc,lang.get,bs.get,ex,co,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFScript(nm : Name, argc : Number, scrpt : Script, ret : Option[ArgRetrievalProtocol],
                    rec : Option[ArgApplicabilityRecognizer], var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFScript extends Comp[DFScript] {
  override def build[T <: DefForm](args : List[Any]) : T = args match {
    case (nm : Name @unchecked) :: (argc : Number @unchecked) :: (scrpt : Script @unchecked)
      :: (ret : Option[ArgRetrievalProtocol] @unchecked) :: (rec : Option[ArgApplicabilityRecognizer] @unchecked)
      :: Nil => DFScript(nm,argc,scrpt,ret,rec,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFSection(nm : Name, com : Option[ArgComponentSections], fls : Option[ArgFiles],
                     var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFSection extends Comp[DFSection] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (nm : Name @unchecked) :: (com : Option[ArgComponentSections] @unchecked)
      :: (fls : Option[ArgFiles] @unchecked) :: Nil => DFSection(nm,com,fls,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFTheoryEnsemble(nm : Name, bt : Option[ArgBaseTheory], fts : Option[ArgFixedTheories],
                            rnm : Option[ArgReplicaRenamer], var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFTheoryEnsemble extends Comp[DFTheoryEnsemble] {
  override def build[T <: DefForm](args : List[Any]): T = {
    args match {
      case (nm : Name @unchecked) :: (bt : Option[ArgBaseTheory] @unchecked)
        :: (fts : Option[ArgFixedTheories] @unchecked) :: (rnm : Option[ArgReplicaRenamer] @unchecked) :: Nil =>
        DFTheoryEnsemble(nm,bt,fts,rnm,None,None).asInstanceOf[T]
      case _ => ??!(args)
    }
  }
}

case class DFTheoryEnsembleMultiple(nm : Name, n : Number, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFTheoryEnsembleMultiple extends Comp[DFTheoryEnsembleMultiple] {
  override def build[T <: DefForm](args : List[Any]): T = {
    args match {
      case (nm : Name @unchecked) :: (n : Number @unchecked) :: Nil =>
        DFTheoryEnsembleMultiple(nm,n,None,None).asInstanceOf[T]
      case _ => ??!(args)
    }
  }
}

case class DFTheoryEnsembleOverloadings(nm : Name, ns : ArgNumbers, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFTheoryEnsembleOverloadings extends Comp[DFTheoryEnsembleOverloadings] {
  override def build[T <: DefForm](args : List[Any]): T = {
    args match {
      case (nm : Name @unchecked) :: (ns : ArgNumbers @unchecked) :: Nil =>
        DFTheoryEnsembleOverloadings(nm,ns,None,None).asInstanceOf[T]
      case _ => ??!(args)
    }
  }
}

case class DFTheoryEnsembleInstances(nm : Name, fuq : Option[ModForceUnderQuickLoad], tt : Option[ArgTargetTheories],
                                     tm : Option[ArgTargetMultiple], ss : Option[ArgEnsembleSorts], cs : Option[ArgEnsembleConsts],
                                     ms : Option[ArgMultiples], tic : Option[ArgTheoryInterpretationCheck],
                                     ps : Option[ArgPermutations], srs : Option[ArgSpecialRenamings],
                                     var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object  DFTheoryEnsembleInstances extends Comp[DFTheoryEnsembleInstances] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (nm : Name @unchecked) :: (fuq : Option[ModForceUnderQuickLoad] @unchecked)
      :: (tt : Option[ArgTargetTheories] @unchecked) :: (tm : Option[ArgTargetMultiple] @unchecked)
      :: (ss : Option[ArgEnsembleSorts]  @unchecked) :: (cs : Option[ArgEnsembleConsts] @unchecked)
      :: (ms : Option[ArgMultiples] @unchecked) :: (tic : Option[ArgTheoryInterpretationCheck] @unchecked)
      :: (ps : Option[ArgPermutations] @unchecked) :: (srs : Option[ArgSpecialRenamings] @unchecked) :: Nil =>
      val nucs : Option[ArgEnsembleConsts] = if (cs.isDefined)
      {
        var nuCAs : List[ArgConstAssoc] = List.empty

        for (c : ArgConstAssoc <- cs.get.consts) {
          var nu_t : List[ODefString] = List.empty
          for (t : ODefString <- c.consts) {
            t.o match
            {
              // In this case, we need to search closer.
              case scala.util.Left(tuple) => tuple match {
                case (df,Some(ime)) => nu_t = nu_t ::: List(t)
                case (df,None)      =>
                  nu_t = nu_t ::: List(t)
              }
              case scala.util.Right(_)  => nu_t = nu_t ::: List(t)
            }
          }
          nuCAs = nuCAs ::: List(ArgConstAssoc(c.nm,nu_t,c.src,c.cmt))
        }

        Some(ArgEnsembleConsts(nuCAs,cs.get.src,cs.get.cmt))

      } else None

      DFTheoryEnsembleInstances(nm,fuq,tt,tm,ss,nucs,ms,tic,ps,srs,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFTheoryInstance(nm : Name, sr : ArgSource, tar : ArgTarget, trans : ArgTranslation,
                            fts : Option[ArgFixedTheories], rnm : Option[ArgRenamer], ntn : Option[ArgNewTranslationName],
                            var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFTheoryInstance extends Comp[DFTheoryInstance] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (nm : Name @unchecked) :: (sr : Option[ArgSource] @unchecked) :: (tar : Option[ArgTarget] @unchecked)
      :: (trans : Option[ArgTranslation] @unchecked) :: (fts : Option[ArgFixedTheories] @unchecked)
      :: (rnm : Option[ArgRenamer] @unchecked) :: (ntn : Option[ArgNewTranslationName] @unchecked) :: Nil =>
      DFTheoryInstance(nm,sr.get,tar.get,trans.get,fts,rnm,ntn,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFTheoryProcessors(nm : Name, as : Option[ArgAlgebraicSimplifier], aos : Option[ArgAlgebraicOrderSimplifier],
                              atc : Option[ArgAlgebraicTermComparator], var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFTheoryProcessors extends Comp[DFTheoryProcessors] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (nm : Name @unchecked) :: (as : Option[ArgAlgebraicSimplifier] @unchecked)
      :: (aos : Option[ArgAlgebraicOrderSimplifier] @unchecked)
      :: (atc : Option[ArgAlgebraicTermComparator]  @unchecked) :: Nil =>
      DFTheoryProcessors(nm,as,aos,atc,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFOrderProcessor(nm : Name, ap : Option[ArgAlgebraicProcessor], ops : Option[ArgProcOperations],
                            ds : Option[ArgDiscreteSorts], var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFOrderProcessor extends Comp[DFOrderProcessor] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (nm : Name @unchecked) :: (ap : Option[ArgAlgebraicProcessor] @unchecked)
      :: (ops : Option[ArgProcOperations] @unchecked) :: (ds : Option[ArgDiscreteSorts] @unchecked) :: Nil =>
      DFOrderProcessor(nm,ap,ops,ds,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFIncludeFiles(rl : Option[ModReload], ql : Option[ModQuickLoad], fs : Option[ArgFiles],
                          var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFIncludeFiles extends Comp[DFIncludeFiles] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (rl : Option[ModReload] @unchecked) :: (ql : Option[ModQuickLoad] @unchecked)
      :: (fs : Option[ArgFiles]  @unchecked) :: Nil => DFIncludeFiles(rl,ql,fs,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFLoadSection(sc : Name, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFLoadSection extends Comp[DFLoadSection] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (sc : Name) :: Nil => DFLoadSection(sc,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

case class DFComment(c : Script, var src : SourceInfo, var cmt : CommentInfo) extends DefForm

object DFComment extends Comp[DFComment] {
  override def build[T <: DefForm](args : List[Any]): T = args match {
    case (c : Script) :: Nil => DFComment(c,None,None).asInstanceOf[T]
    case _ => ??!(args)
  }
}

object FrmFnd extends Logger
{
  override protected def report    : Report = new Report()
  override           def logPrefix : String = "imps-formula-finder"

  def removeWhitespace(str : String) : String = str.replaceAll(" ", "").replaceAll("\t","").replaceAll("\n","").toLowerCase

  def findAxiom(thyname : String, spec : AxiomSpec, js : List[JSONObject]) : IMPSMathExp =
    findFormula(thyname, Some(spec.defstr), "theorems", "axioms", spec.name, js)

  def findFormula(thyname : String, dfstr : Option[DefString], g1 : String, g2 : String = "", n : Option[String], js : List[JSONObject]) : IMPSMathExp =
  {
    val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == thyname.toLowerCase)
    hAssert(json_theory.isDefined, thyname.toLowerCase)
    assert(json_theory.get.getAsString("type") == "imps-theory")

    val group1 : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],g1)
    val group2 : List[JSONObject] = if (g2 == "") { Nil } else { json_theory.get.getAsList(classOf[JSONObject],g2) }

    val haystack : List[JSONObject] = (group1 ::: group2).distinct
    assert(haystack.nonEmpty)

    var needle : String = ""
    var theThing : Option[JSONObject] = None

    // Search by name
    if (n.isDefined)
    {
      assert(n.get != "")
      val temps = haystack.find(j => j.getAsString("name") == n.get.toLowerCase)
      theThing = temps
    }

    // Search by string
    if (theThing.isEmpty)
    {
      // At this point, we need the actual def-string
      if (dfstr.isEmpty) { logError(" > No defstr for " + n.get + " but nothing found via name either.") }
      assert(dfstr.isDefined)

      // unmodified string
      needle = removeWhitespace(dfstr.get.s.tail.init)
      var tempt = haystack.find(j => removeWhitespace(j.getAsString("formula-string")) == needle)

      // if that didn't work, handpicked string
      if (tempt.isEmpty) {
        val handpicked = handpick(removeWhitespace(dfstr.get.s.tail.init))
        tempt = haystack.find(j => removeWhitespace(j.getAsString("formula-string")) == handpicked)
      }
      theThing = tempt
    }

    if (theThing.isEmpty)
    {
      if (n.isDefined) { println("looking for " + n.get.toLowerCase) } else { println("looking for nameless formula") }
      assert(needle != "")
      val bar = removeWhitespace(needle)
      println("> s = " + needle)

      for (t <- haystack)
      {
        val foo = removeWhitespace(t.getAsString("formula-string"))
        val numb = math.min(foo.length,9)
        if (foo.take(numb) == bar.take(numb)) {
          println("> json candidate: " + (if (!(t.getAsString("name") == "")) { t.getAsString("name") + ": " } else {""}) + foo)
        }
      }
    }

    assert(theThing.isDefined)
    val thesexp : String = theThing.get.getAsString("formula-sexp")
    assert(thesexp.nonEmpty)

    val sp : SymbolicExpressionParser = new SymbolicExpressionParser
    val cheatedsexp : String = cheat(thesexp)
    val lsp = sp.parseAll(sp.parseSEXP, cheatedsexp)
    assert(lsp.successful)

    impsMathParser.makeSEXPFormula(lsp.get)
  }

  def findSort(thyname : String, dfstr : DefString, g1 : String, n : Option[String], js : List[JSONObject]) : IMPSSort =
  {
    val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == thyname.toLowerCase)
    assert(json_theory.isDefined)
    assert(json_theory.get.getAsString("type") == "imps-theory")

    val group1 : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],g1)
    assert(group1.nonEmpty)

    var s : String = ""
    val theThing : Option[JSONObject] = if (n.isDefined)
    {
      group1.find(j => j.getAsString("name") == n.get.toLowerCase)
    } else {
      s = dfstr.s.tail.init
      var tempt = group1.find(j => removeWhitespace(j.getAsString("formula-string")) == removeWhitespace(dfstr.s.tail.init))

      if (tempt.isEmpty) {
        val handpicked = handpick(removeWhitespace(dfstr.s.tail.init))
        tempt = group1.find(j => removeWhitespace(j.getAsString("formula-string")) == handpicked)
      }
      tempt
    }

    if (theThing.isEmpty)
    {
      assert(s != "")
      val bar = removeWhitespace(s)
      logError("> s = " + s)

      for (t <- group1)
      {
        val foo = removeWhitespace(t.getAsString("formula-string"))
        val n = 5
        if (foo.take(n) == bar.take(n)) {
          logError("> json theorem: " + foo)
        }
      }
    }

    if (theThing.isEmpty) {
      logError("ERROR: Didn't find thing for " + g1 + " or " + " / " + dfstr)
    }

    assert(theThing.isDefined)
    val thesort : String = theThing.get.getAsString("sort")
    assert(thesort.nonEmpty)

    val sp = new SortParser()
    val lsp = sp.parseAll(sp.parseSort,thesort)
    assert(lsp.successful)
    lsp.get
  }

  /* Welcome to the most shameful part of the codebase...
   * Some strings are subtly different from JSON to T, so we correct by hand.
   */
  def handpick(str: String) : String = {
    str match {
      case "forall(x:rr,#(abs(x)))" => "total_q{abs,[rr,rr]}"
      case "forall(x,y:rr,#(max(x,y)))" => "total_q{max,[rr,rr,rr]}"
      case "forall(x,y:pp,#(dist(x,y)))" => "total_q{dist,[pp,pp,rr]}"
      case "total_q(+_kk,[kk,kk,kk])" => "total_q{+_kk,[kk,kk,kk]}"
      case "total_q(*_kk,[kk,kk,kk])" => "total_q{*_kk,[kk,kk,kk]}"
      case "total_q(-_kk,[kk,kk])" => "total_q{-_kk,[kk,kk]}"
      case "total_q(**,[uu,uu,uu])" => "total_q{**,[uu,uu,uu]}"
      case "forall(x:rr,x+(-x)=0)" => "forall(x:rr,x+-x=0)"
      case "forall(y,x:rr,x-y=x+(-y))" => "forall(y,x:rr,x-y=x+-y)"
      case "forall(n,m:zz,x:rr,((#(x^m,rr)and#(x^n,rr))iff#((x^m)^n,rr)))" => "forall(n,m:zz,x:rr,(#(x^m,rr)and#(x^n,rr))iff#((x^m)^n,rr))"
      case "total_q(+,[rr,rr,rr])" => "total_q{+,[rr,rr,rr]}"
      case "total_q(*,[rr,rr,rr])" => "total_q{*,[rr,rr,rr]}"
      case "total_q(-,[rr,rr])" => "total_q{-,[rr,rr]}"
      case "total_q(sub,[rr,rr,rr])" => "total_q{sub,[rr,rr,rr]}"
      case "forall(x,y,z:kk,not(x=o_kk)andnot(y=o_kk)andnot(z=o_kk)impliesnot(x*_kky=o_kk)and(not(y*_kkz=o_kk)andx*_kky*_kkz=x*_kk(y*_kkz)))" => "forall(x,y,z:kk,not(x=o_kk)andnot(y=o_kk)andnot(z=o_kk)impliesnot(x*y=o_kk)and(not(y*z=o_kk)andx*y*z=x*(y*z)))"
      case "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andinv(x)*_kkx=i_kk)" => "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andinv(x)*x=i_kk)"
      case "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andx*_kkinv(x)=i_kk)" => "forall(x:kk,not(x=o_kk)impliesnot(inv(x)=o_kk)andx*inv(x)=i_kk)"
      case "forall(n,m:zz,x:kk,((#(x^m,kk)and#(x^n,kk))iff#((x^m)^n,kk)))" => "forall(n,m:zz,x:kk,(#(x^m,kk)and#(x^n,kk))iff#((x^m)^n,kk))"
      case "total_q{iterate,[[pp,pp],pp,[zz,pp]]}" => "total_q{ms%iterate,[[pp,pp],pp,[zz,pp]]}"
      case "forsome(a:sets[uu],equiv%class_q(a))" => "nonvacuous_q{equiv%class_q}"
      case "with(a:sets[gg],forall(g,h:gg,(gina)and(hina)implies(gmulh)ina))" => "with(a:sets[gg],forall(g,h:gg,ginaandhinaimplies(gmulh)ina))"
      case "with(a:sets[gg],forall(g:gg,(gina)implies(inv(g)ina)))" => "with(a:sets[gg],forall(g:gg,ginaimpliesinv(g)ina))"
      case "with(u:[ind_1,ind_2],a:sets[ind_1],dom{u}=a)" => "with(a:sets[ind_1],u:[ind_1,ind_2],dom{u}=a)"
      case "with(v:[ind_2,ind_1],b:sets[ind_2],dom{v}=b)" => "with(b:sets[ind_2],v:[ind_2,ind_1],dom{v}=b)"
      case "with(u:[ind_1,ind_2],b:sets[ind_2],ran{u}subseteqb)" => "with(b:sets[ind_2],u:[ind_1,ind_2],ran{u}subseteqb)"
      case "with(v:[ind_2,ind_1],a:sets[ind_1],ran{v}subseteqa)" => "with(a:sets[ind_1],v:[ind_2,ind_1],ran{v}subseteqa)"
      case "lambda(x,y:kk,if(x=o_kkory=o_kk,?kk,x*y))" => "lambda(x,y:kk,if(x=o_kkory=o_kk,?kk,x*_kky))"
      case "with(a:sets[gg],lambda(x,y:gg,if((xina)and(yina),xmuly,?gg)))" => "with(a:sets[gg],restrict2{mul,a,a})"
      case "with(a:sets[gg],lambda(x:gg,if(xina,inv(x),?gg)))" => "with(a:sets[gg],restrict{inv,a})"
      case "with(a:sets[gg],a)" => "with(a:sets[gg],lambda(x_0:gg,x_0ina))"
      case "(indica)" => "lambda(x_0:ind_1,x_0ina)"
      case "(indicb)" => "lambda(x_0:ind_2,x_0inb)"
      case "(indicwith(a:sets[gg],a))" => "with(a:sets[gg],lambda(x_0:gg,x_0ina))"
      case "(predlambda(x:kk,not(x=o_kk)))" => "lambda(x:kk,not(x=o_kk))"
      case "(predlambda(z:kk,not(z=o_kk)))" => "lambda(z:kk,not(z=o_kk))"
      case "lambda(x,y:kk,if(not(x=o_kk)andnot(y=o_kk),x*_kky,?kk))" => "(mullambda(x,y:kk,if(not(x=o_kk)andnot(y=o_kk),x*_kky,?kk))"
      case "(indicwith(a:sets[ind_1],a))" => "with(a:sets[ind_1],lambda(x_0:ind_1,x_0ina))"
      case "(indicwith(b:sets[ind_2],b))" => "with(b:sets[ind_2],lambda(x_0:ind_2,x_0inb))"
      case "lambda(x,y:uu,norm(x-y))" => "lambda(x,y:uu,norm(sub_vv(x,y)))"
      case "forall(x,y,z:bfun,bfun%dist(x,z)<=bfun%dist(x,y)+bfun%dist(y,z))" => "forall(x,y,z:ms%bfun,dist(x,z)<=dist(x,y)+dist(y,z))"
      case "forall(x,y:bfun,bfun%dist(x,y)=bfun%dist(y,x))" => "forall(x,y:ms%bfun,dist(x,y)=dist(y,x))"
      case "forall(x,y:bfun,x=yiffbfun%dist(x,y)=0)" => "forall(x,y:ms%bfun,x=yiffdist(x,y)=0)"
      case _ => str
    }
  }

  def cheat(ime : String) : String = {
    ime.toString match {
      case "(lambda (((pp_0 pp_1) f)) (forall (((pp_1 unit%sort) v)) (implies (apply-operator open v) (apply-operator open (m-inverse-image f v)))))"
        => "(lambda (((pp_0 pp_1) f)) (forall (((pp_1 unit%sort) v)) (implies (apply-operator open_1 v) (apply-operator open_0 (m-inverse-image f v)))))"
      case _ => ime // all cases not named above are identity
    }
  }
}
