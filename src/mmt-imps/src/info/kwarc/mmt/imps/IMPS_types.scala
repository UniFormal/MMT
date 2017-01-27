package info.kwarc.mmt.imps

/* IMPORTS */

import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.parser.SourceRegion
import info.kwarc.mmt.api.parser.SourcePosition

/* Parser abstract class and case classes. */

abstract class LispExp {
    override def toString() : String = "<~ tokenized but unparsed expression ~>"
}

case class Exp(children : List[LispExp], src : SourceRef) extends LispExp {
    override def toString() : String =
    {
        var str : String = "Exp(" + children.toString + ")"
        return str
    }
}

/* TEMPORARY */
case class Dummy(str : String) extends LispExp
{
	override def toString() : String =
	{
		return ("PLACEHOLDER: A " + str + " will appear here in the future!")
	}
}

case class Str(str : String) extends LispExp {
    override def toString() : String = { return "Str(" + str + ")"}
}

/* IMPS SPECIAL FORM ARGUMENTS */

case class Theory(thy : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(theory " + thy + ")"}
}

case class Language(lang : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(language " + lang + ")"}
}

case class Constructor(const : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(constructor " + const + ")" }
}

case class Sort(sort : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(sort " + sort + ")" }
}

case class Witness(witness : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(witness " + witness + ")"}
}

case class SourceTheory(srcthy : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(source-theory " + srcthy + ")"}
}

case class Accessors(accs : List[String], src : SourceRef) extends LispExp {
    override def toString() : String =
    {
        var str : String = "(accessors "
        str = str + accs.head
        for (a <- accs.tail)
        {
            str = str + " " + a
        }
        str = str + ")"
        return str
    }
}

case class Usages(usgs : List[String], src : SourceRef) extends LispExp {
    override def toString() : String =
    {
        var str : String = "(usages "
        str = str + usgs.head
        for (u <- usgs.tail)
        {
            str = str + " " + u
        }
        str = str + ")"
        return str
    }
}

case class FixedTheories(thrs : List[String], src : SourceRef) extends LispExp {
	override def toString() : String =
    {
        var str : String = "(fixed-theories "
        str = str + thrs.head
        for (t <- thrs.tail)
        {
            str = str + " " + t
        }
        str = str + ")"
        return str
    }
}

case class SourceTheories(thrs : List[String], src : SourceRef) extends LispExp {
	override def toString() : String =
    {
        var str : String = "(source-theories "
        str = str + thrs.head
        for (t <- thrs.tail)
        {
            str = str + " " + t
        }
        str = str + ")"
        return str
    }
}

case class Heralding(module : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(herald " + module + ")"}
}

case class LoadSection(section : String, src : SourceRef) extends LispExp {
    override def toString() : String = { return "(load-section " + section + ")"}
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
    override def toString() : String =
    {
        var str : String = "(def-atomic-sort " + sortName
        str = str + "\n  " + quasiSortString
        str = str + "\n  " + theory.toString
        if (!(usages.isEmpty)) { str = str + "\n  " + usages.get.toString}
        if (!(witness.isEmpty)) { str = str + "\n  " + witness.get.toString}
        str = str + ")"
        return str
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
	override def toString() : String =
    {
        var str : String = "(def-cartesian-product " + name
        str = str + "\n  (" + sortNames.head 
        for (sn <- sortNames.tail)
        {
            str = str + " " + sn
        }
        str = str + ")\n  " + thy.toString
        
        if (!(const.isEmpty)) { str = str + "\n  " + const.get.toString}
        if (!(accs.isEmpty))  { str = str + "\n  " + accs.get.toString}
        str = str + ")"
        return str
    }
}

/* def-constant
 * Ducomentation: IMPS manual pgs. 168,169 */
case class Constant(constantName : String,         /* Positional Argument, Required */
                    defExpString : String,         /* Positional Argument, Required */
                    theory       : Theory,         /* Keyword Argument, Required */
                    sort         : Option[Sort],   /* Keyword Argument, Optional */
                    usages       : Option[Usages], /* Keyword Argument, Optional */
                    src          : SourceRef)      /* SourceRef for MMT */
                    extends LispExp
{
    override def toString() : String =
    {
        var str : String = "(def-constant " + constantName
        str = str + "\n  " + defExpString
        str = str + "\n  " + theory.toString
        if (!(usages.isEmpty)) { str = str + "\n  " + usages.get.toString}
        if (!(sort.isEmpty)) { str = str + "\n  " + sort.get.toString}
        str = str + ")"
        return str
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
	override def toString() : String = 
	{
		var str : String = "(def-imported-rewrite-rules " + theoryName
		if (!(srcTheory.isEmpty)) { str = str + "\n  " + srcTheory.get.toString}
		if (!(srcTheories.isEmpty)) { str = str + "\n  " + srcTheories.get.toString}
		str = str + ")"
		return str
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
    override def toString() : String =
    {
        var str : String = "(def-quasi-contructor " + name
        str = str + "\n  " + lambdaExprString
        str = str + "\n  " + language.toString
        if (!(fixedTheories.isEmpty)) { str = str + "\n  " + fixedTheories.get.toString}
        str = str + ")"
        return str
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
	override def toString() : String = 
	{
		var str : String = "(def-schematic-macete " + name
		str = str + "\n  " + formula
		if (nullPresent) { str = str + "\n  null" }
		if (transportablePresent) { str = str + "\n  transportable"}
		str = str + "\n  " + thy.toString
		str = str + ")"
		return str
	}
}
