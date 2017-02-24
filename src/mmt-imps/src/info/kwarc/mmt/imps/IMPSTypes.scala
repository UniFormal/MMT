package info.kwarc.mmt.imps

/* IMPORTS */

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.parser.SourceRef

/* Parser abstract class and case classes. */

abstract class LispExp {
  override def toString: String = "<~ tokenized but unparsed expression ~>"
}

case class Exp(children : List[LispExp], src : SourceRef) extends LispExp {
  override def toString : String =
  {
    "Exp(" + children.toString + ")"
  }
}

case class Comment(content : String, src : SourceRef) extends LispExp {
  override def toString : String =
  {
    ";" + content
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

case class ArgumentTheory(thy : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(theory " + thy + ")"}
}

case class HomeTheory(hmthy : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(home-theory " + hmthy + ")"}
}

case class Language(lang : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(language " + lang + ")"}
}

case class Constructor(const : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(constructor " + const + ")" }
}

case class Macete(macete : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(macete " + macete + ")" }
}

case class Sort(sort : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(sort " + sort + ")" }
}

case class Witness(witness : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(witness " + witness + ")"}
}

case class SourceTheory(srcthy : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(source-theory " + srcthy + ")"}
}

case class ArgumentTranslation(trans : String, src : SourceRef) extends LispExp {
  override def toString: String = { "(translation " + trans + ")" }
}

// Proof scripts ATM only saved as strings
case class Proof(prf : String, src : SourceRef) extends LispExp {
  override def toString : String = { prf }
}

case class Accessors(accs : List[String], src : SourceRef) extends LispExp {
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

case class Usages(usgs : List[String], src : SourceRef) extends LispExp {
  override def toString : String =
  {
    var str : String = "(usages "
    str = str + usgs.head
    for (u <- usgs.tail)
    {
      str = str + " " + u
    }
    str = str + ")"
    str
  }
}

case class FixedTheories(thrs : List[String], src : SourceRef) extends LispExp {
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

case class SourceTheories(thrs : List[String], src : SourceRef) extends LispExp {
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

case class Heralding(module : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(herald " + module + ")"}
}

case class LoadSection(section : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(load-section " + section + ")"}
}

/* IMPS SPECIAL FORMS */

/* def-atomic-sort
 * Documentation: IMPS manual pgs. 158, 159 */
case class AtomicSort(sortName        : String,          /* Positional Argument, Required */
                      quasiSortString : IMPSMathExp,     /* Positional Argument, Required */
                      theory          : ArgumentTheory,  /* Keyword Argument, Required */
                      usages          : Option[Usages],  /* Keyword Argument, Optional */
                      witness         : Option[Witness], /* Keyword Argument, Optional */
                      src             : SourceRef)       /* SourceRef for MMT */
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

/* def-cartesian-product
 * Documentation: IMPS manual pg. 166 */
case class CartesianProduct(name      : String, /* Keyword Argument, Required */
                            sortNames : List[String], /* Keyword Argument, Required */
                            thy       : ArgumentTheory, /* Keyword Argument, Required */
                            const     : Option[Constructor], /* Keyword Argument, Optional */
                            accs      : Option[Accessors], /* Keyword Argument, Optional */
                            src       : SourceRef)            /* SourceRef for MMT */
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-cartesian-product " + name
    str = str + "\n  (" + sortNames.head
    for (sn <- sortNames.tail)
    {
      str = str + " " + sn
    }
    str = str + ")\n  " + thy.toString

    if (const.isDefined) { str = str + "\n  " + const.get.toString}
    if (accs.isDefined)  { str = str + "\n  " + accs.get.toString}
    str = str + ")"
    str
  }
}

/* def-constant
 * Ducomentation: IMPS manual pgs. 168,169 */
case class Constant(constantName : String, /* Positional Argument, Required */
                    math         : IMPSMathExp, /* Positional Argument, Required */
                    theory       : ArgumentTheory, /* Keyword Argument, Required */
                    sort         : Option[IMPSSortRef], /* Keyword Argument, Optional */
                    usages       : Option[Usages], /* Keyword Argument, Optional */
                    src          : SourceRef)      /* SourceRef for MMT */
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
                                src         : SourceRef)              /* SourceRef for MMT */
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
case class QuasiConstructor(name            : String,                /* Positional Argument, Required */
                            lambdaExprString : String,                /* Positional Argument, Required */
                            language         : Language,              /* Keyword Argument, Required */
                            fixedTheories    : Option[FixedTheories], /* Keyword Argument, Optional */
                            src              : SourceRef)             /* SourceRef for MMT */
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
                           src                  : SourceRef) /* SourceRef for MMT */
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
case class Theorem(name    : String,                      /* Positional argument. Required. */
                   formula : IMPSMathExp,                 /* Positional argument. Required. */
                   lemma   : Boolean,                     /* Modifier argument. Optional. */
                   reverse : Boolean,                     /* Modifier argument. Optional. */
                   thy     : ArgumentTheory,              /* Keyword Argument, Required */
                   usages  : Option[Usages],              /* Keyword Argument, Optional */
                   trans   : Option[ArgumentTranslation], /* Keyword Argument, Optional */
                   macete  : Option[Macete],              /* Keyword Argument, Optional */
                   hmthy   : Option[HomeTheory],          /* Keyword Argument, Optional */
                   prf     : Option[Proof],               /* Keyword Argument, Optional */
                   src     : SourceRef)                   /* SourceRef for MMT */
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

/* IMPS MATH EXPRESSIONS */
/* See page 64 etc. of the IMPS manual */

abstract class IMPSMathExp
{
  override def toString: String = "<~ unparsed IMPS math expression ~>"
}

case class IMPSSymbolRef(gn : GlobalName) extends IMPSMathExp
case class IMPSMathSymbol(s : String) extends IMPSMathExp

case class IMPSVar(v : String) extends IMPSMathExp
case class IMPSSortRef(s : String) extends IMPSMathExp

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

case class IMPSForAll(vs : List[(IMPSVar, Option[IMPSSortRef])], p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String =
  {
    var str : String = "forall("
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

case class IMPSForSome(vs : List[(IMPSVar, Option[IMPSSortRef])], p : IMPSMathExp) extends IMPSMathExp
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

case class IMPSLambda(vs : List[(IMPSVar, Option[IMPSSortRef])], t : IMPSMathExp) extends IMPSMathExp
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

case class IMPSIota(v1 : IMPSVar, s1 : IMPSSortRef, p : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "iota(" + v1.toString + ":" + s1.toString + "," + p.toString + ")"
}

case class IMPSIotaP(v1 : IMPSVar, s1 : IMPSSortRef, p : IMPSMathExp) extends IMPSMathExp
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

case class IMPSIsDefinedIn(t : IMPSMathExp, s : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "#(" + t.toString + "," + s.toString + ")"
}

case class IMPSUndefined(s : IMPSMathExp) extends IMPSMathExp
{
  override def toString: String = "?" + s.toString
}