package info.kwarc.mmt.imps

/* IMPORTS */

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.imps.NumericalType.NumericalType
import info.kwarc.mmt.imps.Usage.Usage

/* Parser abstract class and case classes. */

abstract class LispExp {
  override def toString: String = "<~ tokenized but unparsed expression ~>"
}

case class Exp(children : List[LispExp], src : Option[SourceRef]) extends LispExp {
  override def toString : String =
  {
    "Exp(" + children.toString + ")"
  }
}

case class Comment(content : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String =
  {
    "; " + content
  }
}

case class Str(str : String) extends LispExp {
  override def toString : String = { "Str(" + str + ")" }
}

/* TEMPORARY */
case class Dummy(str : String) extends LispExp
{
  override def toString : String =
  {
    "PLACEHOLDER: A " + str + " will appear here in the future!"
  }
}

case class ParseFailure(str : String) extends LispExp {
  override def toString: String = {
    "Could not parse structure: " + str
  }
}

/* IMPS SPECIAL FORM ARGUMENTS */

case class ArgumentTheory(thy : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(theory " + thy + ")"}
}

case class HomeTheory(hmthy : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(home-theory " + hmthy + ")"}
}

case class TranslationSource(thy : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(source " + thy + ")"}
}

case class TranslationTarget(thy : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(target " + thy + ")"}
}

case class ArgumentLanguage(lang : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(language " + lang + ")"}
}

case class Constructor(const : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(constructor " + const + ")" }
}

case class Macete(macete : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(macete " + macete + ")" }
}

case class Sort(sort : IMPSSort, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(sort " + sort + ")" }
}

case class Witness(witness : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(witness " + witness + ")"}
}

case class SourceTheory(srcthy : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(source-theory " + srcthy + ")"}
}

case class ArgumentTranslation(trans : String, src : Option[SourceRef]) extends LispExp {
  override def toString: String = { "(translation " + trans + ")" }
}

case class EmbeddedLanguage(name : String, src : Option[SourceRef]) extends LispExp {
  override def toString: String = { "(embedded-language " + name + ")" }
}

case class EmbeddedLanguages(names : List[String], src : Option[SourceRef]) extends LispExp {
  override def toString: String = {
    var str : String = "(embedded-languages " + names.head
    for (n <- names.tail)
    {
      str = str + " " + n
    }
    str = str + ")"
    str
  }
}

case class DistinctConstants(lst : List[List[String]], src : Option[SourceRef]) extends LispExp
{
  override def toString: String =
  {
    var str : String = "(distinct-constants "
    for (dst <- lst)
    {
      str = str + "("
      for (nm <- dst)
      {
        str = str + nm + " "
      }
      str = str.trim
      str = str + ")"
    }
    str = str + ")"
    str
  }
}

case class LangBaseTypes(tps : List[String], src : Option[SourceRef]) extends LispExp {
  override def toString: String = {
    var str = "(base-types " + tps.head
    for (t <- tps.tail)
    {
      str = str + " " + t
    }
    str = str + ")"
    str
  }
}

case class ComponentTheories(lst : List[String], src : Option[SourceRef]) extends LispExp
{
  override def toString: String =
  {
    assert(!lst.isEmpty)
    var str : String = "(component-theories"
    for (t <- lst)
    {
      str = str + " " + t
    }
    str = str + ")"
    str
  }
}

object NumericalType extends Enumeration
{
  type NumericalType = Value
  val INTEGERTYPE = Value("*integer-type*")
  val RATIONALTYPE = Value("*rational-type*")
  val OCTETTYPE = Value("*octet-type*")
}

case class TypeSortAList(numericType : NumericalType, sort : String) extends LispExp
{
  override def toString: String = "(" + numericType.toString + " " + sort + ")"
}

case class Extensible(lst : List[TypeSortAList], src : Option[SourceRef]) extends LispExp
{
  override def toString: String = {
    var str : String = ""
    for (p <- lst)
    {
      str = str + p.toString + " "
    }
    str = str.trim
    str
  }
}

case class SortSpecifications(lst : List[(IMPSSort, IMPSSort)], src : Option[SourceRef]) extends LispExp
{
  override def toString: String = {
    var str : String = ""
    for (p <- lst)
    {
      str = str + "(" + p._1.toString + " " + p._2.toString + ") "
    }
    str = str.trim
    str
  }
}

case class ConstantSpecifications(lst : List[(String, IMPSSort)], src : Option[SourceRef]) extends LispExp
{
  override def toString: String = {
    var str : String = ""
    for (p <- lst)
    {
      str = str + "(" + p._1 + " " + p._2.toString + ") "
    }
    str = str.trim
    str
  }
}

case class AxiomSpecification(formula : IMPSMathExp,
                              name    : Option[String],
                              usgs    : Option[List[Usage]],
                              src     : Option[SourceRef])
  extends LispExp
{
  override def toString: String =
  {
    var str : String = "("
    if (name.isDefined) { str = str + name + " " }
    str = str + "\"" + formula.toString + "\""
    if (usgs.isDefined)
    {
      assert(!usgs.get.isEmpty)
      for (u <- usgs.get)
      {
        str = str + " " + u
      }
    }
    str = str + ")"
    str
  }
}

case class TheoryAxioms(axs : List[AxiomSpecification], src : Option[SourceRef]) extends LispExp
{
  override def toString: String =
  {
    var str : String = "(axioms "
    assert(!axs.isEmpty)
    for (a <- axs)
    {
      str = str + " " + a.toString
    }
    str = str + ")"
    str
  }
}

// Proof scripts ATM only saved as strings
case class Proof(prf : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { prf }
}

case class Accessors(accs : List[String], src : Option[SourceRef]) extends LispExp {
  override def toString : String =
  {
    var str : String = "(accessors "
    str = str + accs.head
    for (a <- accs.tail)
    {
      str = str + " " + a
    }
    str = str + ")"
    str
  }
}

/* These are all seven usages of (for example) theorems.
 * See page 77 of IMPS manual. */
object Usage extends Enumeration
{
  type Usage = Value

  val ELEMENTARYMACETE       = Value("elementary-macete")
  val TRANSPORTABLEMACETE    = Value("transportable-macete")
  val REWRITE                = Value("rewrite")
  val TRANSPORTABLEREWRITE   = Value("transportable-rewrite")
  val SIMPLIFYLOGICALLYFIRST = Value("simplify-logically-first")
  val DRCONVERGENCE          = Value("d-r-convergence")
  val DRVALUE                = Value("d-r-value")
}

case class ArgumentUsages(usgs : List[Usage], src : Option[SourceRef]) extends LispExp {
  override def toString : String =
  {
    var str : String = "(usages "
    str = str + usgs.head.toString
    for (u <- usgs.tail)
    {
      str = str + " " + u.toString
    }
    str = str + ")"
    str
  }
}

case class FixedTheories(thrs : List[String], src : Option[SourceRef]) extends LispExp {
  override def toString : String =
  {
    var str : String = "(fixed-theories "
    str = str + thrs.head
    for (t <- thrs.tail)
    {
      str = str + " " + t
    }
    str = str + ")"
    str
  }
}

case class SourceTheories(thrs : List[String], src : Option[SourceRef]) extends LispExp {
  override def toString : String =
  {
    var str : String = "(source-theories "
    str = str + thrs.head
    for (t <- thrs.tail)
    {
      str = str + " " + t
    }
    str = str + ")"
    str
  }
}

case class Heralding(module : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(herald " + module + ")"}
}

case class LoadSection(section : String, src : Option[SourceRef]) extends LispExp {
  override def toString : String = { "(load-section " + section + ")"}
}

/* IMPS SPECIAL FORMS */

/* def-atomic-sort
 * Documentation: IMPS manual pgs. 158, 159 */
case class AtomicSort(sortName        : String, /* Positional Argument, Required */
                      quasiSortString : IMPSMathExp, /* Positional Argument, Required */
                      theory          : ArgumentTheory, /* Keyword Argument, Required */
                      usages          : Option[ArgumentUsages], /* Keyword Argument, Optional */
                      witness         : Option[Witness], /* Keyword Argument, Optional */
                      src             : Option[SourceRef],       /* SourceRef for MMT */
                      encSort         : IMPSSort)
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-atomic-sort " + sortName
    str = str + "\n  " + quasiSortString.toString
    str = str + "\n  " + theory.toString
    if (usages.isDefined) str = str + "\n  " + usages.get.toString
    if (witness.isDefined) str = str + "\n  " + witness.get.toString
    str = str + ")"
    str
  }
}

/* def-constant
 * Ducomentation: IMPS manual pgs. 168,169 */
case class Constant(constantName : String, /* Positional Argument, Required */
                    math         : IMPSMathExp, /* Positional Argument, Required */
                    theory       : ArgumentTheory, /* Keyword Argument, Required */
                    sort         : Option[Sort], /* Keyword Argument, Optional */
                    usages       : Option[ArgumentUsages], /* Keyword Argument, Optional */
                    src          : Option[SourceRef])      /* SourceRef for MMT */
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-constant " + constantName
    str = str + "\n  " + math.toString
    str = str + "\n  " + theory.toString
    if (usages.isDefined) { str = str + "\n  " + usages.get.toString}
    if (sort.isDefined) { str = str + "\n  " + sort.get.toString}
    str = str + ")"
    str
  }
}

/* def-imported-rewrite-rules
 * Documentation: IMPS manual pg. 169*/
case class ImportedRewriteRules(theoryName  : String,                 /* Positional Argument, Required */
                                srcTheory   : Option[SourceTheory],   /* Keyword Argument, Optional */
                                srcTheories : Option[SourceTheories], /* Keyword Argument, Optional */
                                src         : Option[SourceRef])              /* SourceRef for MMT */
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-imported-rewrite-rules " + theoryName
    if (srcTheory.isDefined) { str = str + "\n  " + srcTheory.get.toString}
    if (srcTheories.isDefined) { str = str + "\n  " + srcTheories.get.toString}
    str = str + ")"
    str
  }
}

/* def-quasi-constructor
 * Documentation: IMPS manual pgs. 176, 177 */
case class QuasiConstructor(name            : String,                 /* Positional Argument, Required */
                            lambdaExprString : String,                /* Positional Argument, Required */
                            language         : ArgumentLanguage,      /* Keyword Argument, Required */
                            fixedTheories    : Option[FixedTheories], /* Keyword Argument, Optional */
                            src              : Option[SourceRef])             /* SourceRef for MMT */
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-quasi-contructor " + name
    str = str + "\n  " + lambdaExprString
    str = str + "\n  " + language.toString
    if (fixedTheories.isDefined) { str = str + "\n  " + fixedTheories.get.toString}
    str = str + ")"
    str
  }
}

/* def-schematic-macete
 * Decomentation: IMPS manual pgs. 180, 181 */
case class SchematicMacete(name                 : String, /* Positional Argument, Required */
                           formula              : String, /* Positional Argument, Required */
                           thy                  : ArgumentTheory, /* Keyword Argument, Required */
                           nullPresent          : Boolean, /* Keyword Argument, Optional */
                           transportablePresent : Boolean, /* Keyword Argument, Optional */
                           src                  : Option[SourceRef]) /* SourceRef for MMT */
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-schematic-macete " + name
    str = str + "\n  " + formula
    if (nullPresent) { str = str + "\n  null" }
    if (transportablePresent) { str = str + "\n  transportable"}
    str = str + "\n  " + thy.toString
    str = str + ")"
    str
  }
}

/* def-theorem
 * Documentation: IMPS manual pgs. 184 ff. */
case class Theorem(name    : String, /* Positional argument. Required. */
                   formula : IMPSMathExp, /* Positional argument. Required. */
                   lemma   : Boolean, /* Modifier argument. Optional. */
                   reverse : Boolean, /* Modifier argument. Optional. */
                   thy     : ArgumentTheory, /* Keyword Argument, Required */
                   usages  : Option[ArgumentUsages], /* Keyword Argument, Optional */
                   trans   : Option[ArgumentTranslation], /* Keyword Argument, Optional */
                   macete  : Option[Macete], /* Keyword Argument, Optional */
                   hmthy   : Option[HomeTheory], /* Keyword Argument, Optional */
                   prf     : Option[Proof], /* Keyword Argument, Optional */
                   src     : Option[SourceRef])                   /* SourceRef for MMT */
  extends LispExp
{
  override def toString: String =
  {
    var str : String = "(def-theorem " + name + "\n  " + formula.toString
    if (lemma) { str = str + "\n  lemma"}
    if (reverse) { str = str + "\n  reverse"}
    str = str + "\n  " + thy.toString
    if (usages.isDefined) { str = str + "\n  " + usages.get.toString }
    if (macete.isDefined) { str = str + "\n  " + macete.get.toString }
    if (hmthy.isDefined) { str = str + "\n  " + hmthy.get.toString }
    if (prf.isDefined) { str = str + "\n  " + prf.get.toString }
    str = str + ")"
    str
  }
}

/* def-language
 * Documentation: IMPS manual pgs. 172 ff. */
case class Language(name       : String,
                    embedlang  : Option[EmbeddedLanguage],
                    embedlangs : Option[EmbeddedLanguages],
                    bstps      : Option[LangBaseTypes],
                    extens     : Option[Extensible],
                    srts       : Option[SortSpecifications],
                    cnstnts    : Option[ConstantSpecifications],
                    src        : Option[SourceRef])
  extends LispExp
{
  override def toString: String =
  {
    var str : String = "(def-language " + name
    if (embedlang.isDefined)  { str = str + "\n  " + embedlang.get.toString }
    if (embedlangs.isDefined) { str = str + "\n  " + embedlangs.get.toString }
    if (bstps.isDefined) { str = str + "\n  " + bstps.get.toString }
    if (extens.isDefined) { str = str + "\n  " + extens.get.toString }
    if (srts.isDefined) { str = str + "\n  " + srts.get.toString }
    if (cnstnts.isDefined) { str = str + "\n  " + cnstnts.get.toString }
    str = str + ")"
    str
  }

  def union(l : Language) : Language =
  {
    val nu_name : String = name + "_union_" + l.name

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    var lsHere  : List[String] = Nil
    var lsThere : List[String] = Nil

    if (embedlang.isDefined)  { lsHere = lsHere :+  embedlang.get.name }
    if (embedlangs.isDefined) { lsHere = lsHere ::: embedlangs.get.names }

    if (l.embedlang.isDefined)  { lsThere = lsThere :+  l.embedlang.get.name }
    if (l.embedlangs.isDefined) { lsThere = lsThere ::: l.embedlangs.get.names }

    val union_embedlangs : List[String] = (lsHere ::: lsThere).distinct

    var nu_embedlang  : Option[EmbeddedLanguage]  = None
    var nu_embedlangs : Option[EmbeddedLanguages] = None

    if (union_embedlangs.nonEmpty)
    {
      val src_embedlang = if (embedlang.isDefined)    { embedlang.get.src }
                     else if (embedlangs.isDefined)   { embedlangs.get.src }
                     else if (l.embedlang.isDefined)  { l.embedlang.get.src }
                     else                             { l.embedlangs.get.src }

      if (union_embedlangs.length == 1) { nu_embedlang = Some(EmbeddedLanguage(union_embedlangs.head, src_embedlang)) }
      else { nu_embedlangs = Some(EmbeddedLanguages(union_embedlangs, src_embedlang)) }
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    var nu_basetypes : Option[LangBaseTypes] = None

    var bsHere  : List[String] = Nil
    var bsThere : List[String] = Nil

    if (bstps.isDefined)   { bsHere  = bstps.get.tps }
    if (l.bstps.isDefined) { bsThere = l.bstps.get.tps }

    val union_basetypes : List[String] = (bsHere ::: bsThere).distinct

    if (union_basetypes.nonEmpty)
    {
      val src_basetypes = if (bstps.isDefined) { bstps.get.src } else { l.bstps.get.src }
      nu_basetypes = Some(LangBaseTypes(union_basetypes, src_basetypes))
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    var nu_extensible : Option[Extensible] = None

    var exHere  : List[TypeSortAList] = Nil
    var exThere : List[TypeSortAList] = Nil

    if (extens.isDefined)   { exHere = extens.get.lst }
    if (l.extens.isDefined) { exThere = l.extens.get.lst }

    val union_extens : List[TypeSortAList] = (exHere ::: exThere).distinct

    if (union_extens.nonEmpty)
    {
      val src_extens = if (extens.isDefined) { extens.get.src } else { l.extens.get.src }
      nu_extensible = Some(Extensible(union_extens, src_extens))
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    var nu_sorts : Option[SortSpecifications] = None

    var srtHere  : List[(IMPSSort, IMPSSort)] = Nil
    var srtThere : List[(IMPSSort, IMPSSort)] = Nil

    if (srts.isDefined)   { srtHere = srts.get.lst }
    if (l.srts.isDefined) { srtThere = l.srts.get.lst }

    val union_srts : List[(IMPSSort, IMPSSort)] = (srtHere ::: srtThere).distinct

    if (union_srts.nonEmpty)
    {
      val src_srts = if (srts.isDefined) { srts.get.src } else { l.srts.get.src }
      nu_sorts = Some(SortSpecifications(union_srts, src_srts))
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    var nu_consts : Option[ConstantSpecifications] = None

    var conHere  : List[(String, IMPSSort)] = Nil
    var conThere : List[(String, IMPSSort)] = Nil

    if (cnstnts.isDefined)   { conHere = cnstnts.get.lst }
    if (l.cnstnts.isDefined) { conThere = l.cnstnts.get.lst }

    val union_constants : List[(String, IMPSSort)] = (conHere ::: conThere).distinct

    if (union_constants.nonEmpty)
    {
      val src_constants = if (cnstnts.isDefined) { cnstnts.get.src } else { l.cnstnts.get.src }
      nu_consts = Some(ConstantSpecifications(union_constants, src_constants))
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Language(nu_name, nu_embedlang, nu_embedlangs, nu_basetypes, nu_extensible, nu_sorts, nu_consts, src)
  }
}

/* def-theory
 * Documentation: IMPS manual pgs. 186 ff. */
case class Theory(name      : String,
                  lang      : Option[ArgumentLanguage],
                  cmpntthrs : Option[ComponentTheories],
                  axioms    : Option[TheoryAxioms],
                  dstnct    : Option[DistinctConstants],
                  src       : Option[SourceRef])
  extends LispExp
{
  override def toString: String =
  {
    var str : String = "(def-theory " + name
    if (lang.isDefined) { str = str + "\n  " + lang.get.toString }
    if (cmpntthrs.isDefined) { str = str + "\n  " + cmpntthrs.get.toString }
    if (axioms.isDefined) { str = str + "\n  " + axioms.get.toString }
    if (dstnct.isDefined) { str = str + "\n  " + dstnct.get.toString }
    str = str + ")"
    str
  }
}

case class Translation(name: String,
                       force : Boolean,
                       forceQL : Boolean,
                       dontEnrich : Boolean,
                       source : TranslationSource,
                       target : TranslationTarget,
                       fixed : Option[List[String]],
                       assumptions : Option[List[IMPSMathExp]],
                       sortpairs : Option[List[(IMPSSort,String)]],
                       constpairs : Option[List[(IMPSMathExp,IMPSMathExp)]],
                       coretrans : Option[String],
                       theintcheck : Option[String],
                       src : Option[SourceRef])
extends LispExp
{

}

/* IMPS SORTS ETC */

abstract class IMPSSort

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

    str = str + "]"
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

/* IMPS MATH EXPRESSIONS */
/* See page 64 etc. of the IMPS manual */

abstract class IMPSMathExp
{
  override def toString: String = "<~ unparsed IMPS math expression ~>"
}

case class IMPSSymbolRef(gn : GlobalName) extends IMPSMathExp {
  override def toString: String = gn.toString
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

case class IMPSForAll(vs : List[(IMPSVar, Option[IMPSSort])], p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "forall("
    for ((v, s) <- vs)
    {
      str = str + v.toString
      if (s.isDefined) { str = str + ":" + s.get.toString }
      str = str + ","
    }
    str = str + p.toString + ")"
    str
  }
}

case class IMPSForSome(vs : List[(IMPSVar, Option[IMPSSort])], p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "forsome("
    for ((v, s) <- vs)
    {
      str = str + v.toString
      if (s.isDefined) str = str + ":" + s.get.toString
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

/* TODO: Is this even correct? */
case class IMPSApply(f : IMPSMathExp, ts : List[IMPSMathExp]) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = f.toString
    str = str + "(" + ts.head.toString
    for (t <- ts.tail)
    {
      str = str + "," + t.toString
    }
    str = str + ")"
    str
  }
}

case class IMPSLambda(vs : List[(IMPSVar, Option[IMPSSort])], t : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "lambda("
    for ((v, s) <- vs)
    {
      str = str + v.toString
      if (s.isDefined) str = str + ":" + s.get.toString
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

case class IMPSTotal(f : IMPSMathExp, betas : List[IMPSSort]) extends IMPSMathExp
{
  override def toString: String = "total?(" + f.toString + ", " + betas.toString() + ")"
}

case class IMPSNonVacuous(p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "nonvacuous?(" + p.toString + ")"
}

case class IMPSQuasiEquals(e1 : IMPSMathExp, e2 : IMPSMathExp) extends  IMPSMathExp
{
  override def toString: String = e1.toString + " == " + e2.toString
}

case class IMPSIndividual() extends  IMPSMathExp
{
  override def toString: String = "an%individual"
}

//-----------

abstract class SEXP

case class SEXPAtom(s : String) extends SEXP

case class SEXPNested(args : List[SEXP]) extends SEXP