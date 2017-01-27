package info.kwarc.mmt.imps

package object defFormParsers
{
	/* Parser for IMPS special form def-constants
     * Documentation: IMPS manual pgs. 168, 169 */
    def parseConstant (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name       : Option[String] = None
        var defstring  : Option[String] = None
        var thy        : Option[Theory] = None

        // Optional arguments
        var usages : Option[Usages] = None
        var sort   : Option[Sort]   = None

        val cs : Int = e.children.length

        /* Three arguments minimum because three req. arguments */
        if (cs >= 3)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name      = Some(x) }
            e.children(2) match { case Exp(List(Str(y)), _) => defstring = Some(y) }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("theory")),_) => thy    = argParsers.parseTheory(Exp(ds,src))
                        case Exp(List(Str("usages")),_) => usages = argParsers.parseUsages(Exp(ds,src))
                        case Exp(List(Str("sort")),_)   => sort   = argParsers.parseSort(Exp(ds,src))
                        case _                          => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || defstring.isEmpty || thy.isEmpty) { return None }
            else { return Some(Constant(name.get, defstring.get, thy.get, sort, usages, e.src)) }

        } else { return None }
    }

    /* Parser for IMPS special form def-atomic sort
     * Documentation: IMPS manual pgs. 158, 159 */
    def parseAtomicSort (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name : Option[String] = None
        var qss  : Option[String] = None
        var thy  : Option[Theory] = None

        // Optional arguments
        var usages  : Option[Usages]  = None
        var witness : Option[Witness] = None

        val cs : Int = e.children.length

        /* Three arguments minimum because three req. arguments */
        if (cs >= 3)
        {
            /* Parse positional arguments */
            e.children(1) match { case Exp(List(Str(x)), _) => name = Some(x) }
            e.children(2) match { case Exp(List(Str(y)), _) => qss  = Some(y) }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("theory")),_)  => thy     = argParsers.parseTheory(Exp(ds,src))
                        case Exp(List(Str("usages")),_)  => usages  = argParsers.parseUsages(Exp(ds,src))
                        case Exp(List(Str("witness")),_) => witness = argParsers.parseWitness(Exp(ds,src))
                        case _                           => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || qss.isEmpty || thy.isEmpty) { return None }
            else { return Some(AtomicSort(name.get, qss.get, thy.get, usages, witness, e.src)) }

        } else { return None }
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
                        case Exp(List(Str("language")),_)       => lang  = argParsers.parseLanguage(Exp(ds,src))
                        case Exp(List(Str("fixed-theories")),_) => fixed = argParsers.parseFixedTheories(Exp(ds,src))
                        case _                           => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || expstr.isEmpty || lang.isEmpty) { return None }
            else { return Some(QuasiConstructor(name.get, expstr.get, lang.get, fixed, e.src)) }

        } else { return None }
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
                        case Exp(List(Str("src-theory")),_)   => srcTheory   = argParsers.parseSourceTheory(Exp(ds,src))
                        case Exp(List(Str("src-theories")),_) => srcTheories = argParsers.parseSourceTheories(Exp(ds,src))
                        case _                                => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || (srcTheory.isEmpty && srcTheories.isEmpty)) { return None }
            else { return Some(ImportedRewriteRules(name.get, srcTheory, srcTheories, e.src)) }

        } else { return None }
    }
    
    /* Parser for IMPS special form def-cartesian-product
     * Documentation: IMPS manual pg. 166 */
    def parseCartesianProduct (e : Exp) : Option[LispExp] =
    {
        // Required arguments
        var name      : Option[String]       = None
        var sortNames : Option[List[String]] = None
        var thy       : Option[Theory]       = None

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
				case Exp(cs, sc) => for (c <- cs) {
					c match {
						case Exp(List(Str(s)),_) => tmplst = tmplst ::: List(s)
						case _                   => ()
					}
				}
				case _ => return None
			}
			if (tmplst != List.empty) { sortNames = Some(tmplst) } else { return None }

            /* Parse keyword arguments, these can come in any order */
            var i : Int = 3
            while (cs - i > 0)
            {
                e.children(i) match {
                    case Exp(ds,src) => ds.head match
                    {
                        case Exp(List(Str("constructor")),_) => const = argParsers.parseConstructor(Exp(ds,src))
                        case Exp(List(Str("accessors")),_)   => accs  = argParsers.parseAccessors(Exp(ds,src))
                        case Exp(List(Str("theory")),_)      => thy   = argParsers.parseTheory(Exp(ds,src))
                        case _                               => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || sortNames.isEmpty || thy.isEmpty) { return None }
            else { return Some(CartesianProduct(name.get, sortNames.get, thy.get, const, accs, e.src)) }

        } else { return None }
    }
    
    /* Parser for IMPS special form def-schematic-macete
     * Documentation: IMPS manual pgs. 180, 181 */
    def parseSchematicMacete (e : Exp) : Option[LispExp] =
    {	
        // Required arguments
        var name    : Option[String] = None
        var formula : Option[String] = None
        var thy     : Option[Theory] = None

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
                        case Exp(List(Str("theory")),_) => thy = argParsers.parseTheory(Exp(ds,src))
                        case (Str("null"))              => nullarg  = true
                        case (Str("transportable"))     => transarg = true
                        case _                          => ()
                    }
                    case _ => ()
                }
                i += 1
            }

            /* check for required arguments */
            if (name.isEmpty || formula.isEmpty || thy.isEmpty) { return None }
            else { return Some(SchematicMacete(name.get, formula.get, thy.get, nullarg, transarg, e.src)) }

        } else { return None }
    }
}
