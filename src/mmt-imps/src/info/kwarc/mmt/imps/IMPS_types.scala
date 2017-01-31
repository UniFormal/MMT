package info.kwarc.mmt.imps

/* IMPORTS */

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
  override def toString : String = { "Str(" + str + ")"}
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

case class Theory(thy : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(theory " + thy + ")"}
}

case class Language(lang : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(language " + lang + ")"}
}

case class Constructor(const : String, src : SourceRef) extends LispExp {
  override def toString : String = { "(constructor " + const + ")" }
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
                      quasiSortString : String,          /* Positional Argument, Required */
                      theory          : Theory,          /* Keyword Argument, Required */
                      usages          : Option[Usages],  /* Keyword Argument, Optional */
                      witness         : Option[Witness], /* Keyword Argument, Optional */
                      src             : SourceRef)       /* SourceRef for MMT */
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-atomic-sort " + sortName
    str = str + "\n  " + quasiSortString
    str = str + "\n  " + theory.toString
    if (usages.isDefined) str = str + "\n  " + usages.get.toString
    if (witness.isDefined) str = str + "\n  " + witness.get.toString
    str = str + ")"
    str
  }
}

/* def-cartesian-product
 * Documentation: IMPS manual pg. 166 */
case class CartesianProduct(name      : String,               /* Keyword Argument, Required */
                            sortNames : List[String],         /* Keyword Argument, Required */
                            thy       : Theory,               /* Keyword Argument, Required */
                            const     : Option[Constructor],  /* Keyword Argument, Optional */
                            accs      : Option[Accessors],    /* Keyword Argument, Optional */
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
case class Constant(constantName : String,         /* Positional Argument, Required */
                    defExpString : String,         /* Positional Argument, Required */
                    math         : IMPSMathExpr,   /* inferred */
                    theory       : Theory,         /* Keyword Argument, Required */
                    sort         : Option[Sort],   /* Keyword Argument, Optional */
                    usages       : Option[Usages], /* Keyword Argument, Optional */
                    src          : SourceRef)      /* SourceRef for MMT */
  extends LispExp
{
  override def toString : String =
  {
    var str : String = "(def-constant " + constantName
    str = str + "\n  " + defExpString
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
case class SchematicMacete(name                 : String,    /* Positional Argument, Required */
                           formula              : String,    /* Positional Argument, Required */
                           thy                  : Theory,    /* Keyword Argument, Required */
                           nullPresent          : Boolean,   /* Keyword Argument, Optional */
                           transportablePresent : Boolean,   /* Keyword Argument, Optional */
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

/* IMPS MATH EXPRESSIONS */
/* See page 64 etc. of the IMPS manual */

abstract class IMPSMathExpr
{
  override def toString: String = "<~ unparsed IMPS math expression ~>"
}

case class IMPSTruth() extends IMPSMathExpr
{
  override def toString: String = "truth"
}

case class IMPSFalsehood() extends IMPSMathExpr
{
  override def toString: String = "falsehood"
}

case class IMPSNegation(p : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "not(" + p.toString + ")"
}

case class IMPSConjunction(ps : List[IMPSMathExpr]) extends IMPSMathExpr
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

case class IMPSDisjunction(ps : List[IMPSMathExpr]) extends IMPSMathExpr
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

case class IMPSImplication(p : IMPSMathExpr, q : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = p.toString + " implies " + q.toString
}

case class IMPSIff(p : IMPSMathExpr, q : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = p.toString + " iff " + q.toString
}

case class IMPSIfForm(p : IMPSMathExpr, q : IMPSMathExpr, r : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "if_form(" + p.toString + "," + q.toString + "," + r.toString + ")"
}

case class IMPSForAll(vs : List[(IMPSMathExpr, Option[IMPSMathExpr])], p : IMPSMathExpr) extends IMPSMathExpr
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

case class IMPSForSome(vs : List[(IMPSMathExpr, Option[IMPSMathExpr])], p : IMPSMathExpr) extends IMPSMathExpr
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

case class IMPSEquals(t1 : IMPSMathExpr, t2 : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = t1.toString + "=" + t2.toString
}

/* TODO: Is this even correct? */
case class IMPSApply(f : IMPSMathExpr, ts : List[IMPSMathExpr]) extends IMPSMathExpr
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

case class IMPSLambda(vs : List[(IMPSMathExpr, Option[IMPSMathExpr])], t : IMPSMathExpr) extends IMPSMathExpr
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

case class IMPSIota(v1 : IMPSMathExpr, s1 : IMPSMathExpr, p : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "iota(" + v1.toString + ":" + s1.toString + "," + p.toString + ")"
}

case class IMPSIotaP(v1 : IMPSMathExpr, s1 : IMPSMathExpr, p : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "iota_p(" + v1.toString + ":" + s1.toString + "," + p.toString + ")"
}

case class IMPSIf(p : IMPSMathExpr, t1 : IMPSMathExpr, t2 : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "if(" + p.toString + "," + t1.toString + "," + t2.toString + ")"
}

case class IMPSIsDefined(t : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "#(" + t.toString + ")"
}

case class IMPSIsDefinedIn(t : IMPSMathExpr, s : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "#(" + t.toString + "," + s.toString + ")"
}

case class IMPSUndefined(s : IMPSMathExpr) extends IMPSMathExpr
{
  override def toString: String = "?" + s.toString
}