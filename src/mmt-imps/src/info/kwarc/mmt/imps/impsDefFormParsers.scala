package info.kwarc.mmt.imps

package object impsDefFormParsers
{

  def parseLanguage ( e : Exp ) : Option[]

  /* Parser for IMPS special form def-atomic sort
   * Documentation: IMPS manual pgs. 158, 159 */
  def parseAtomicSort (e : Exp) : Option[LispExp] =
  {
    // Required arguments
    var name : Option[String]         = None
    var qss  : Option[IMPSMathExp]    = None
    var thy  : Option[ArgumentTheory] = None

    // Optional arguments
    var usages  : Option[Usages]  = None
    var witness : Option[Witness] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => qss  = impsMathParser.parseIMPSMath(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_)  => thy     = impsArgumentParsers.parseTheory(Exp(ds,src))
            case Exp(List(Str("usages")),_)  => usages  = impsArgumentParsers.parseUsages(Exp(ds,src))
            case Exp(List(Str("witness")),_) => witness = impsArgumentParsers.parseWitness(Exp(ds,src))
            case _                           => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || qss.isEmpty || thy.isEmpty) None
      else { Some(AtomicSort(name.get, qss.get, thy.get, usages, witness, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-constants
   * Documentation: IMPS manual pgs. 168, 169 */
  def parseConstant (e : Exp) : Option[LispExp] =
  {
    // Required arguments
    var name   : Option[String]         = None
    var defexp : Option[IMPSMathExp]    = None
    var thy    : Option[ArgumentTheory] = None

    // Optional arguments
    var usages : Option[Usages] = None
    var sort   : Option[Sort]   = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => defexp = impsMathParser.parseIMPSMath(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_) => thy    = impsArgumentParsers.parseTheory(Exp(ds,src))
            case Exp(List(Str("usages")),_) => usages = impsArgumentParsers.parseUsages(Exp(ds,src))
            case Exp(List(Str("sort")),_)   => sort   = impsArgumentParsers.parseSort(Exp(ds,src))
            case _                          => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || defexp.isEmpty || thy.isEmpty) None
      else { Some(Constant(name.get, defexp.get, thy.get, ???, usages, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-quasi-constructor
   * Documentation: IMPS manual pgs. 177, 178 */
  def parseQuasiConstructor (e : Exp) : Option[LispExp] =
  {
    // Required arguments
    var name   : Option[String]   = None
    var expstr : Option[String]   = None
    var lang   : Option[Language] = None

    // Optional arguments
    var fixed  : Option[FixedTheories] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => expstr = Some(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("language")),_)       => lang  = impsArgumentParsers.parseLanguage(Exp(ds,src))
            case Exp(List(Str("fixed-theories")),_) => fixed = impsArgumentParsers.parseFixedTheories(Exp(ds,src))
            case _                           => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || expstr.isEmpty || lang.isEmpty) None
      else { Some(QuasiConstructor(name.get, expstr.get, lang.get, fixed, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-imported-rewrite-rules
   * Documentation: IMPS manual pg. 169 */
  def parseImportedRewriteRules (e : Exp) : Option[LispExp] =
  {
    // Required arguments
    var name   : Option[String]   = None

    // Optional arguments
    var srcTheory   : Option[SourceTheory]   = None
    var srcTheories : Option[SourceTheories] = None

    val cs : Int = e.children.length

    if (cs >= 2)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 2
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("src-theory")),_)   => srcTheory   = impsArgumentParsers.parseSourceTheory(Exp(ds,src))
            case Exp(List(Str("src-theories")),_) => srcTheories = impsArgumentParsers.parseSourceTheories(Exp(ds,src))
            case _                                => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || (srcTheory.isEmpty && srcTheories.isEmpty)) None
      else { Some(ImportedRewriteRules(name.get, srcTheory, srcTheories, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-cartesian-product
   * Documentation: IMPS manual pg. 166 */
  def parseCartesianProduct (e : Exp) : Option[LispExp] =
  {
    // Required arguments
    var name      : Option[String]         = None
    var sortNames : Option[List[String]]   = None
    var thy       : Option[ArgumentTheory] = None

    // Optional arguments
    var accs  : Option[Accessors]   = None
    var const : Option[Constructor] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

      var tmplst : List[String] = List.empty

      e.children(2) match {
        case Exp(czs, _) => for (c <- czs) {
          c match {
            case Exp(List(Str(k)),_) => tmplst = tmplst ::: List(k)
            case _                   => ()
          }
        }
        case _ => None
      }
      if (tmplst != List.empty) { sortNames = Some(tmplst) } else { None }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("constructor")),_) => const = impsArgumentParsers.parseConstructor(Exp(ds,src))
            case Exp(List(Str("accessors")),_)   => accs  = impsArgumentParsers.parseAccessors(Exp(ds,src))
            case Exp(List(Str("theory")),_)      => thy   = impsArgumentParsers.parseTheory(Exp(ds,src))
            case _                               => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || sortNames.isEmpty || thy.isEmpty) None
      else { Some(CartesianProduct(name.get, sortNames.get, thy.get, const, accs, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-schematic-macete
   * Documentation: IMPS manual pgs. 180, 181 */
  def parseSchematicMacete (e : Exp) : Option[LispExp] =
  {
    // Required arguments
    var name    : Option[String] = None
    var formula : Option[String] = None
    var thy     : Option[ArgumentTheory] = None

    // Optional arguments
    var nullarg  : Boolean = false
    var transarg : Boolean = false

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name    = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => formula = Some(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match
        {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_) => thy = impsArgumentParsers.parseTheory(Exp(ds,src))
            case (Str("null"))              => nullarg  = true
            case (Str("transportable"))     => transarg = true
            case _                          => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || formula.isEmpty || thy.isEmpty) None
      else { Some(SchematicMacete(name.get, formula.get, thy.get, nullarg, transarg, e.src)) }

    } else { None }
  }

  /* Parser for IMPS form def-theorem
   * Documentation: IMPS manual pgs. 184 ff. */
  def parseTheorem (e : Exp) : Option[LispExp] =
  {
    /* Required arguments */
    var name    : Option[String]         = None
    var formula : Option[IMPSMathExp]    = None
    var theory  : Option[ArgumentTheory] = None

    /* Optional Arguments */
    var lemma   : Boolean = false
    var reverse : Boolean = false

    var usages : Option[Usages]              = None
    var hmthy  : Option[HomeTheory]          = None
    var trans  : Option[ArgumentTranslation] = None
    var macete : Option[Macete]              = None
    var prf    : Option[Proof]               = None

    /* Three arguments minimum */
    val csl : Int = e.children.length
    if (csl >= 3)
    {
      /* Parse positional arguments, these must be in this order */
      e.children(1) match
      {
        case Exp(List(Str(x)), _) => name = Some(x)
        case Exp(List(),_)        => name = Some("()") // Theorems can have this name, according to documentation.
                                                       // Turns out parsers get easily confused here.
      }
      e.children(2) match {
        case Exp(List(Str(y)), _) => formula = impsMathParser.parseIMPSMath(y)
      }

      /* Parse modifier and keyword arguments, these can come in any order */
      var i : Int = 3
      while (csl - i > 0)
      {
        e.children(i) match
        {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_)      => theory  = impsArgumentParsers.parseTheory(Exp(ds,src))
            case Exp(List(Str("home-theory")),_) => hmthy   = impsArgumentParsers.parseHomeTheory(Exp(ds,src))
            case Exp(List(Str("usages")),_)      => usages  = impsArgumentParsers.parseUsages(Exp(ds,src))
            case Exp(List(Str("macete")),_)      => macete  = impsArgumentParsers.parseMacete(Exp(ds,src))
            case Exp(List(Str("translation")),_) => trans   = impsArgumentParsers.parseTranslation(Exp(ds,src))
            case Exp(List(Str("proof")),_)       => prf     = impsArgumentParsers.parseProofScript(Exp(ds,src))
            case (Str("lemma"))                  => reverse = true
            case (Str("reverse"))                => lemma   = true
            case _                               => () // other arguments get silently discarded. TODO: Maybe fail instead?
          }
          case _ => ()
        }
        i += 1
      }

      if (name.isEmpty || formula.isEmpty || theory.isEmpty) { None }
      else { Some(Theorem(name.get, formula.get, lemma, reverse, theory.get, usages, trans, macete, hmthy, prf, e.src))}
    } else { None }


  }

  /* Parser for IMPS special form def-language
   * Documentation: IMPS manual pgs. 172 - 174 */
  def parseLanguage (e : Exp) : Option[LispExp] =
  {
    /* Required Arguments */
    var name : Option[String] = None

    /* Optional Arguments */
    var embedlang  : Option[EmbeddedLanguage]       = None
    var embedlangs : Option[EmbeddedLanguages]      = None
    var bstps      : Option[LangBaseTypes]          = None
    var extens     : Option[Extensible]             = None
    var srts       : Option[SortSpecifications]     = None
    var cnstnts    : Option[ConstantSpecifications] = None

    if (e.children.nonEmpty)
    {
      /* Parse positional arguments, these must be in this order */
      e.children(1) match {
        case Exp(List(Str(x)), _) => name = Some(x)
      }

      for (c <- e.children.tail)
      {
        c match
        {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("embedded-languages")),_) => embedlangs = impsArgumentParsers.parseEmbeddedLangs(Exp(ds,src))
            case Exp(List(Str("embedded-language")),_)  => embedlang  = impsArgumentParsers.parseEmbeddedLang(Exp(ds,src))
            case Exp(List(Str("base-types")),_)         => bstps      = impsArgumentParsers.parseLanguageBaseTypes(Exp(ds,src))
            case Exp(List(Str("sorts")),_)              => srts       = impsArgumentParsers.parseSortsArgument(Exp(ds,src))
            case Exp(List(Str("extensible")),_)         => extens     = impsArgumentParsers.parseExtensible(Exp(ds,src))
            case Exp(List(Str("constants")),_)          => cnstnts    = impsArgumentParsers.parseConstants(Exp(ds,src))
            case _                                      => ()
          }
          case _ => ()
        }

      }

      if (name.isEmpty) { None }
      else { Some(Language(name.get, embedlang, embedlangs, bstps, extens, srts, cnstnts, e.src)) }
    } else { None }
  }
}
