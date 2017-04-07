package info.kwarc.mmt.imps

import info.kwarc.mmt.api.parser.SourceRef

package object impsArgumentParsers
{
  /* Parser for IMPS usages objects
   * used in: def-atomic-sort */
  def parseUsages (e : Exp) : Option[Usages] =
  {
    /* Can contain one or multiple usages */
    var usgs : List[String] = List.empty

    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str(x)),_) => usgs = usgs ::: List(x)
          case _                   => None
        }
        i += 1
      }
      if (usgs != List.empty)
      { Some(Usages(usgs, e.src)) } else { None }

    } else { None }
  }

  /* Parser for IMPS constructor argument objects
   * used in: def-cartesian-product */
  def parseConstructor(e : Exp) : Option[Constructor] =
  {
    if (e.children.length == 2)
    {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Constructor(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS theory argument objects
   * used in: def-atomic-sort... */
  def parseTheory (e : Exp) : Option[ArgumentTheory] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(ArgumentTheory(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS source-theory argument objects
   * used in: def-imported-rewrite-rules... */
  def parseSourceTheory (e : Exp) : Option[SourceTheory] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(SourceTheory(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS home-theory argument objects
   * used in: def-theorem... */
  def parseHomeTheory (e : Exp) : Option[HomeTheory] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(HomeTheory(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS macete argument (!) objects
  * used in: def-theorem... */
  def parseMacete (e : Exp) : Option[Macete] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Macete(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS translation argument (!) objects
  * used in: def-theorems... */
  def parseTranslation (e : Exp) : Option[ArgumentTranslation] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(ArgumentTranslation(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS witness argument objects
   * used in: def-atomic-sort, ... */
  def parseWitness (e : Exp) : Option[Witness] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Witness(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS language argument objects
   * used in: def-quasi-constructor, ... */
  def parseLanguage (e : Exp) : Option[ArgumentLanguage] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(ArgumentLanguage(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS sort argument objects
   * used in: def-constant, ... */
  def parseSort (e : Exp) : Option[Sort] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Sort(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS fixed theories objects
   * used in: def-quasi-constructor */
  def parseFixedTheories (e : Exp) : Option[FixedTheories] =
  {
    /* Can contain one or multiple usages */
    var fixed : List[String] = List.empty

    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str(x)),_) => fixed = fixed ::: List(x)
          case _                   => None
        }
        i += 1
      }
      if (fixed != List.empty)
      { Some(FixedTheories(fixed, e.src)) } else { None }

    } else { None }
  }

  /* Parser for IMPS source theories objects
   * used in: def-imported-rewrite-rules */
  def parseSourceTheories (e : Exp) : Option[SourceTheories] =
  {
    /* Can contain one or multiple usages */
    var source : List[String] = List.empty

    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str(x)),_) => source = source ::: List(x)
          case _                   => None
        }
        i += 1
      }
      if (source.nonEmpty)
      { Some(SourceTheories(source, e.src)) } else { None }

    } else { None }
  }

  /* Parser for IMPS embedded-lang arguments
   * used in: def-language */
  def parseEmbeddedLang(e : Exp) : Option[EmbeddedLanguage] =
  {
    assert(e.children.length == 2)

    e.children(2) match {
      case Exp(List(Str(x)),_) => Some(EmbeddedLanguage(x, e.src))
      case _                   => None
    }
  }

  /* Parser for IMPS embedded-langs arguments
   * used in: def-language */
  def parseEmbeddedLangs(e : Exp) : Option[EmbeddedLanguages] =
  {
    var lst : List[String] = Nil

    for (k <- e.children.tail)
    {
      k match {
        case Exp(List(Str(x)),_) => lst = lst ::: List(x)
        case _                   => ()
      }
    }

    if (lst.isEmpty) { None } else { Some(EmbeddedLanguages(lst, e.src)) }
  }


  /* Parser for IMPS base-types argument
   * used in: def-language */
  def parseLanguageBaseTypes(e : Exp) : Option[LangBaseTypes] =
  {
    var lst : List[String] = Nil

    for (k <- e.children.tail)
    {
      k match {
        case Exp(List(Str(x)),_) => lst = lst ::: List(x)
        case _                   => ()
      }
    }

    if (lst.isEmpty) { None } else { Some(LangBaseTypes(lst, e.src))}
  }

  /* Parser for IMPS extensible argument
   * used in: def-language */
  def parseExtensible(e : Exp) : Option[Extensible] =
  {
    var list : List[TypeSortAList] = Nil

    for (l <- e.children)
    {
      val prs : Option[TypeSortAList] = l match {
        case Exp(x,y) => parseTypeSortAList(Exp(x,y))
        case _        => None
      }

      if (prs.isDefined) { list = list ::: List(prs.get) }
    }

    if (list.isEmpty) { None } else { Some(Extensible(list, e.src))}

  }

  /* Parser for IMPS typeSortALists
   * used in: def-language (extensible, see parseExtensible) */
  def parseTypeSortAList(e : Exp) : Option[TypeSortAList] =
  {
    e.children(1) match {
      case Exp(List(Str(x)),_) => e.children(2) match {
        case Exp(List(Str(y)),_) => Some(TypeSortAList(x,y))
        case _                   => None
      }
      case _                   => None
    }

  }

  /* Parser for IMPS constantsList argument
   * used in: def-language */
  def parseConstants(e : Exp) : Option[ConstantSpecifications] =
  {
    var lst : List[(String, String)] = Nil

    // One constant notification minimum
    assert(e.children.tail.nonEmpty)

    // each c is one constant specification
    for (c <- e.children.tail)
    {
      c match
      {
        case Exp(ss,_) =>
        {
          // Constant specification has two elements
          assert(ss.length == 2)

          ss(1) match {
            case Exp(List(Str(name)),_) => ss(2) match {
              case Exp(js,_) =>
              {
                var str : String = ""
                if (js.length >= 2) {str = "("}
                for (j <- js)
                {
                  j match {
                    case Str(srt) => str = str + srt + " "
                    case _ => ()
                  }
                }
                str = str.trim
                if (js.length >= 2) {str = str + ")"}
                lst = lst ::: List((name, str))
              }
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    }

    if (lst.isEmpty) { None } else { Some(ConstantSpecifications(lst, e.src)) }
  }

  /* Parser for IMPS sorts argument
   * used in: def-language */
  def parseSortsArgument(e : Exp) : Option[SortSpecifications] =
  {
    var lst : List[(String, String)] = Nil

    // One sort notification minimum
    assert(e.children.tail.nonEmpty)

    // each c is one sort specification
    for (c <- e.children.tail)
    {
      c match
      {
        case Exp(ss,_) =>
        {
          // each sort specification has two elements
          assert(ss.length == 2)

          ss(1) match {
            case Exp(List(Str(name)),_) => ss(2) match {
              case Exp(js,_) =>
              {
                var str : String = ""
                if (js.length >= 2) {str = "("}
                for (j <- js)
                {
                  j match {
                    case Str(srt) => str = str + srt + " "
                    case _ => ()
                  }
                }
                str = str.trim
                if (js.length >= 2) {str = str + ")"}
                lst = lst ::: List((name, str))
              }
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    }

    if (lst.isEmpty) { None } else { Some(SortSpecifications(lst, e.src)) }
  }

  /* Parser for IMPS fixed theories objects
   * used in: def-quasi-constructor */
  def parseAccessors(e : Exp) : Option[Accessors] =
  {
    /* Can contain one or multiple usages */
    var accs : List[String] = List.empty

    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str(x)),_) => accs = accs ::: List(x)
          case _                   => None
        }
        i += 1
      }
      if (accs != List.empty)
      { Some(Accessors(accs, e.src)) } else { None }

    } else { None }
  }

  /**
    * Parser for IMPS proof argument objects, used in def-theorem
    * @param e The parsed LispExp (Exp, to be precise) from which to read the script.
    * @return A Proof object, which is just a wrapper for String at this time.
    */
  def parseProofScript(e : Exp) : Option[Proof] =
  {
    var proof : String = "(proof "
    for (c <- e.children)
    {
      proof = proof + "\n  " + c.toString

    }
    proof = proof + ")"
    Some(Proof(proof, e.src))
  }
}
