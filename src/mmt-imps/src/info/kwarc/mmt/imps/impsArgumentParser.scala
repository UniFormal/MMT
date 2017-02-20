package info.kwarc.mmt.imps

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
  def parseLanguage (e : Exp) : Option[Language] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Language(x, e.src))
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
