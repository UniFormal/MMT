package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.{Path, utils}
import info.kwarc.mmt.api.utils._

/** generates statistics for the RelStore */
trait RelStoreStatistics { this: ClassicRelStore =>

  /**
    * Make a statistic for the document or theory at the given path
    * @param p the path of the document or theory
    */
  def makeStatistics(q: Path): Statistics = {
    val decl = Transitive(+Declares) // anything declared by the theory
    val subtheory = (decl | Reflexive) * HasType(IsTheory)
    val align = decl * Transitive(+IsAlignedWith)
    // val subtheory = Transitive(+Declares *HasType(IsTheory) | Reflexive)
    val expMorph = decl * Transitive(+HasViewFrom)
    val morph = decl * Transitive(+HasMeta | +Includes | +IsImplicitly | +HasViewFrom)
    val expinduced = expMorph * decl * HasType(IsConstant)
    val induced = subtheory * morph * decl * HasType(IsConstant)

    var dsG = makeStatisticsFor(q, decl, None)
    dsG += makeStatisticsFor(q, align, Some(AlignmentPrefix))

    val (exMorph, anyMorph) = (querySet(q, expMorph).size, querySet(q, morph).size)
    if (exMorph > 0)
      dsG += (StatisticEntry(ExplicitMorphismEntry, exMorph), NoPrefix)
    if (anyMorph > 0)
      dsG += (StatisticEntry(AnyMorphismEntry, anyMorph), NoPrefix)

    dsG += makeStatisticsFor(q, expinduced, Some(ExplicitMorphismPrefix))
    dsG += makeStatisticsFor(q, align, Some(AnyMorphismPrefix))

    dsG
  }

  /**
    * Make a statistic for the given query and prepend the prefix to the descriptions of the found declarations
    * @param p the path of the document of theory to make the statistic for
    * @param q the Relation to find queries for
    */
  private def makeStatisticsFor(p:Path, q:RelationExp, prefix:Option[StatPrefixType]): Statistics = {
    val pre = prefix.getOrElse(NoPrefix)
    val ds = querySet(p, q)

    val dsG : List[StatisticEntry] =
      utils.histogram(ds.toList, getConstantType)
      .map({e => StatisticEntry(e._1, e._2)})
      .toList
    Statistics(Map((pre, dsG)))
  }

  /**
    * Discriminates the different sorts of constants into types untyped constant, data constructor, datatype constructor,
    * @param s the type of the constant
    * @param p the path of the constant
    * @note precondition: Every constant is annotated as having exactly one of the types untyped constant, data constructor,
    * datatype constructor, rule, judgement constructor, high universe
    */
  private val getConstantType = new PartialFunction[Path, StatEntryType] {
    def isDefinedAt(p: Path): Boolean = getType(p).isDefined
    def apply(p: Path): StatEntryType = getType(p).get match {
      case IsConstant =>
        try {
          val desiredTypes: List[Unary] = List(IsDataConstructor, IsDatatypeConstructor, IsRule, IsJudgementConstructor, IsHighUniverse, IsUntypedConstant)
          desiredTypes.filter(hasType(p, _)).collect({
            case IsDatatypeConstructor => DatatypeConstructorEntry
            case IsDataConstructor => DataConstructorEntry
            case IsRule => RuleEntry
            case IsJudgementConstructor => JudgementConstructorEntry
            case IsHighUniverse => HighUniverseEntry
            case IsUntypedConstant => UntypedConstantEntry
          }) match {
            case h :: t =>
              if (t.nonEmpty) {
                log(s"clashing relational data for: ${p.toPath}: ${h :: t}")
              }
              h
            case Nil =>
              log("Found unexpected Individual: " + p.toPath)
              TypedConstantEntry
          }
        } catch {
          case t: Exception =>
            log("Maltyped constant at: " + p.toString())
            MaltypedConstantEntry
        }
      case IsTheory => TheoryEntry
      case IsDocument => DocumentEntry
      case IsView => ViewEntry
      case IsStructure => StructureEntry
      case IsPattern => PatternEntry
      case _ => AnyOtherEntry
    }
  }


  // TODO: Move this into a static object
  def statDescription : JSONArray = {
    val prefs = JSONArray(StatPrefixType.all map(_.toJSON):_*)
    val entries = JSONArray(StatEntryType.all map(_.toJSON):_*)
    JSONArray(List(JSONObject("sorts of relations to the declaration" -> prefs), JSONObject("types of declarations" -> entries)):_*)
  }
}



/** statistics that are being returned */
case class Statistics(entries: collection.immutable.Map[StatPrefixType, List[StatisticEntry]]) {
  /** adds another statistics object to this one */
  def +(that: Statistics): Statistics = {
    var stat = this
    that.entries.foreach { case (e, l) => l.foreach {s => stat = stat + (s, e)} }
    stat
  }

  /** adds a StatisticEntry with a given prefix to this statistics object */
  def +(s: StatisticEntry, pre: StatPrefixType) = Statistics(
    if (entries.toList.map(_._1.key).contains(pre.key))
      entries map {
        case (p, stats) if p.key == pre.key =>
          if (stats map (_.stat) contains s.stat) {
            val statsRes = stats map { e =>
              if (e.stat.key == s.stat.key) StatisticEntry(e.stat, e.n + s.n)
              else e
            }
            (p, statsRes)
          } else
            (p, s :: stats)
        case e => e
      }
    else entries.+((pre, List(s)))
  )

  /**
    * Returns a JSON representation of the statistics
    */
  def toJSON : JSONArray = {
    val jstats = entries.toList map {case (pre, substat) => JSONObject(pre.key -> JSONArray(substat.map(_.toJSON):_*))}
    JSONArray(jstats:_*)
  }
}



/** a single generated statistics entry */
case class StatisticEntry(stat : StatEntryType, n: Int) {
  // when working with lists of stat entries, we maintain the invariance
  // of at most one type of each StatEntryType
  // TODO: We could try to use a Map instead of a list, might make this more efficient

  /** adds another [[StatisticEntry]] to this one */
  def +(other : StatisticEntry) : List[StatisticEntry] = other.stat match {
    case `stat` => List(copy(n = n + other.n))
    case _ => List(this, other)
  }

  /** adds a list of [[StatisticEntry]]s to this one */
  def :+(stats : List[StatisticEntry]) : List[StatisticEntry] = (this :: stats).groupBy(_.stat).map({
    g => StatisticEntry(g._1, g._2.map(_.n).sum)
  }).toList

  def toJSON : JSONObject = JSONObject(
    "key" -> JSONString(stat.key),
    "value" -> JSONInt(n)
  )
}

/** the overall type of statistics generated */
sealed abstract class StatPrefixType(val key: String, val teaser: String, val description: String) {
  def toJSON: JSON = JSONArray(JSONObject(
    "key" -> JSONString(key),
    "teaser" -> JSONString(teaser),
    "description" -> JSONString(description),
  ))
}
object StatPrefixType {
  val all: List[StatPrefixType] = List(NoPrefix, ExplicitMorphismPrefix, AnyMorphismPrefix, AlignmentPrefix)
  def unapply(entry: StatPrefixType): Option[(String, String, String)] = Some((entry.key, entry.teaser, entry.description))
}

case object NoPrefix extends StatPrefixType("decl","Declared declaration","Declarations declared (possibly via multiple hops) by the given theory/document/archive")
case object ExplicitMorphismPrefix extends StatPrefixType("exp_mor","Induced declaration by explicit morphisms (views)","Declarations induced via (possibly multiple concatenated) explicit theory morphisms (aka. views)")
case object AnyMorphismPrefix extends StatPrefixType("any_mor", "Induced declaration by any morphisms", "Declarations induced via any (possibly multiple concatenated) theory morphisms (views, includes, metas and implicit morphisms)")
case object AlignmentPrefix extends StatPrefixType("align", "Alignment", "Declarations aligned (possibly via multiple intermediate alignments) with declaration declared by the given theory/document/archive")


/** the type of a single statistics entry */
sealed abstract class StatEntryType(val key: String, val teaser: String, val description: String) {
  def toJSON: JSON = JSONArray(JSONObject(
    "key" -> JSONString(key),
    "teaser" -> JSONString(teaser),
    "description" -> JSONString(description),
  ))
}
object StatEntryType {
  val all: List[StatEntryType] = List(TheoryEntry, DocumentEntry, UntypedConstantEntry, TypedConstantEntry, MaltypedConstantEntry, StructureEntry, PatternEntry, JudgementConstructorEntry, DataConstructorEntry, DatatypeConstructorEntry, RuleEntry, ViewEntry, HighUniverseEntry, ExplicitMorphismEntry, AnyMorphismEntry, AnyOtherEntry)
  def unapply(entry: StatEntryType): Option[(String, String, String)] = Some((entry.key, entry.teaser, entry.description))
}

case object TheoryEntry extends StatEntryType("theo", "theory", "theory")
case object DocumentEntry extends StatEntryType("doc", "document", "document")
case object UntypedConstantEntry extends StatEntryType("unty_con", "untyped constant", "declaration of untyped constant")
case object TypedConstantEntry extends StatEntryType("ty_con", "typed constant", "declaration of typed constant")
case object MaltypedConstantEntry extends StatEntryType("mal_con", "maltyped constant", "maltyped (type can't be infered) constant declaration")
case object StructureEntry extends StatEntryType("struc", "structure", "structure")
case object PatternEntry extends StatEntryType("pat", "pattern", "pattern")
case object JudgementConstructorEntry extends StatEntryType("judg", "judgement constructor", "judgement constructor")
case object DataConstructorEntry extends StatEntryType("data", "data constructor", "data constructor")
case object DatatypeConstructorEntry extends StatEntryType("type", "datatype constructor", "datatype constructor")
case object RuleEntry extends StatEntryType("rule", "rule", "rule declarations")
case object ViewEntry extends StatEntryType("view", "view", "views in the theory")
case object HighUniverseEntry extends StatEntryType("high", "high universe", "constructors returning kinds or object in even higher typing universes")
case object ExplicitMorphismEntry extends StatEntryType("exp_mor", "explicit theory morphism", "the transitive closure of all theory morphisms into the theory")
case object AnyMorphismEntry extends StatEntryType("any_mor", "any theory morphism", "the transitive closure of all theory morphisms into the theory")
case object AnyOtherEntry extends StatEntryType("other", "other", "other")
