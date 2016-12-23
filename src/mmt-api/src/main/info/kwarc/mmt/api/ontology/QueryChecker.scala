package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.{LocalName, NamespaceMap, ParseError, Path}
import info.kwarc.mmt.api.objects._


object QueryChecker {
  // TODO: Actually write this theory and possible swap around the path
  private val QMTBaseTypes = Path.parseM("http://cds.omdoc.org/urtheories?QueryTypes", NamespaceMap.empty)
  private val pathPath = QMTBaseTypes ? "Path"
  private val objectPath = QMTBaseTypes ? "Object"
  private val xmlPath = QMTBaseTypes ? "XML"
  private val stringPath = QMTBaseTypes ? "String"

  /**
    * Checks that a proposition is well-formed or throws a [[ParseError]]
    *
    * @param p       Proposition to check
    * @param context Context to check proposition in
    */
  private def check(p: Prop)(implicit context: Context) {
    p match {

      /** the query has to be a single element */
      case IsA(e: Query, tp: Unary) => infer(e) match {
        case TupleQuery(_) =>
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected TupleQuery() as argument to IsA()")
      }

      /** PrefixOf expects to PathType queries */
      case PrefixOf(s, l) =>
        expectQueryType(s, ElementQuery(PathType))
        expectQueryType(l, ElementQuery(PathType))

      /** elem has to be an element and tp has to be a set of said type */
      case IsIn(elem, tp) =>
        (infer(elem), infer(tp)) match {
          case (TupleQuery(s), SetTupleQuery(t)) if s == t =>
          case _ => throw ParseError("illegal proposition: " + p + "\nExpected an TupleQuery() and SetTupleQuery() as arguments to IsIn()")
        }

      /** isEmpty can only check sets */
      case IsEmpty(r) => infer(r) match {
        case SetTupleQuery(_) =>
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected SetTuple() as argument to IsEmpty()")
      }

      /** Equal needs to be elements of the same type */
      case Equal(left, right) => (infer(left), infer(right)) match {
        case (TupleQuery(s), TupleQuery(t)) if s == t =>
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected elements of same type as arguments to Equal()")
      }

      /** takes any two valid props */
      case Or(left, right) => check(left); check(right)

      /** exists has to be a set and the scope has to match */
      case Exists(domain, vn, scope) => infer(domain) match {
        case SetTupleQuery(t) => check(scope)(context ++ VarDecl(vn, Some(QueryType.generate(TupleQuery(t))), None, None))
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected SetTupleQuery() as domain of Forall()")
      }

      /** Not can take any valid prop */
      case Not(arg) => check(arg)

      /** takes any two valid props */
      case And(left, right) => check(left); check(right)

      /** forall has to be a set and the scope has to match */
      case Forall(domain, vn, scope) => infer(domain) match {
        case SetTupleQuery(t) => check(scope)(context ++ VarDecl(vn, Some(QueryType.generate(TupleQuery(t))), None, None))
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected SetTupleQuery() as domain of Forall()")
      }

      /** a judgement has to hold about a single object.  */
      case Holds(about, varname, j) => infer(about) match {
        case ElementQuery(ObjType) =>
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected ElementQuery() as argument to Holds()")
      }
    }
  }

  /**
    * Check that two queries have compatible BaseTypes.
    *
    * @param l       Left query to check
    * @param r       Right query to check
    * @param context Context to check
    * @return
    */
  private def checkCompatibility(l: Query, r: Query)(implicit context: Context): QueryType = {
    val ltp :: rtp :: Nil = List(infer(l), infer(r)).map({
      case SetTupleQuery(tp) => tp
      case TupleQuery(tp) => tp
    })

    if (ltp != rtp) {
      throw ParseError("illegal queries. Expected identical BaseTypes, got " + ltp + " and " + rtp)
    }

    SetTupleQuery(ltp)
  }

  /**
    * Infers the type of a Query and throws [[ParseError]] if it does not match the expected type of tp
    *
    * @param q       Query to infer
    * @param tp      Type of query to expect
    * @param context Context under which to infer query
    */
  private def expectQueryType(q: Query, tp: QueryType)(implicit context: Context): QueryType = {
    val it = infer(q)
    if (it != tp) {
      throw ParseError("illegal Query: " + q + "\nExpected type: " + tp + "Actual type: " + it)
    }
    tp
  }

  private def expectLiftableQueryBaseType(q: Query, tp: QueryBaseType, result: QueryBaseType)(implicit context: Context): QueryType = infer(q) match {
    // single element of the base type
    case ElementQuery(`tp`) => ElementQuery(result)

    // a set of base types
    case SetElementQuery(`tp`) => SetElementQuery(result)

    // did not get the right type
    case _ => throw ParseError("illegal Query: " + q + "\nExpected type: ElementQuery(" + tp + ") or SetElementQuery(" + tp + ")")

  }

  /**
    * Checks a relational expression and throws [[ParseError]] if it is invalid
    *
    * @param rel     Relational expression to check
    * @param context Context to check it in
    */
  private def check(rel: RelationExp)(implicit context: Context) {
    rel match {
      case ToObject(_) =>
      case ToSubject(_) =>
      case HasType(_, _) =>
      case Transitive(r) => check(r)
      case Choice(rs@_*) => rs foreach { r => check(r) }
      case Sequence(rs@_*) => rs foreach { r => check(r) }
      case Reflexive =>
      case _ => throw ParseError("Illegal RelationExp: " + rel + "\nUnknown query type. ")
    }
  }

  /**
    * Infers the return type of a base type
    *
    * @param b BaseType to infer
    * @return
    */
  private def infer(b: BaseType): QueryBaseType = b match {
    case p: Path => PathType
    case s: StringValue => StringType
    case x: XMLValue => XMLType
    case o: Obj => ObjType
  }


  /**
    * Infers the type of a Query
    *
    * @param q       Query to infer
    * @param context Context under which to infer Query.
    * @return
    */
  def infer(q: Query)(implicit context: Context): QueryType = q match {
    /** I() just gives a hint, so the type does not change.  */
    case I(qq, h) =>
      infer(qq)

    /** infer the type of the bound variable from the context */
    case Bound(vn) =>
      QueryType.parse(context(vn).tp.get)

    /** component of paths */
    case Component(of, _) =>
      expectLiftableQueryBaseType(of, PathType, ObjType)

    /** subobject of an object */
    case SubObject(of, _) =>
      expectLiftableQueryBaseType(of, ObjType, ObjType)

    /** a set of paths related to a path or a set of paths */
    case Related(to, by) =>
      // need a valid proposition
      check(by)

      // check that we have a basic Path type
      infer(to) match {
        case ElementQuery(PathType) => SetElementQuery(PathType)
        case SetElementQuery(PathType) => SetElementQuery(PathType)
        case t => throw ParseError("illegal query: " + q + "\nExpected a set of paths inside Related()")
      }

    /** a single literal is just an element */
    case Literal(b) =>
      ElementQuery(infer(b))

    /** a list of literals is a literal for a tuple */
    case Literals(bs@_*) =>
      SetTupleQuery(bs.map(infer).toList)

    /** infer the sub-query and check that the type still works */
    case Let(vn: LocalName, v: Query, in: Query) =>

      val vI = infer(v) match {
        case TupleQuery(s) => TupleQuery(s)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a TupleQuery() inside of Let()")
      }

      infer(in)(context ++ VarDecl(vn, Some(QueryType.generate(vI)), None, None))

    /** turns a single element into a set containing just that element */
    case Singleton(e: Query) =>
      infer(e) match {
        case TupleQuery(t) => SetTupleQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a single element inside of Singleton()")
      }

    /** a set of paths */
    case Paths(_) =>
      SetElementQuery(PathType)

    /** a set of objects */
    case Unifies(_) =>
      SetElementQuery(ObjType)

    /** closure of a single path */
    case Closure(of) =>
      expectQueryType(of, ElementQuery(PathType))
      SetElementQuery(PathType)

    /** Union queries should be of the same type */
    case Union(l, r) =>
      checkCompatibility(l, r)

    /** Big Union: domain must be a set, inner union must be also be a set */
    case BigUnion(d, vn, of) => infer(d) match {
      case SetTupleQuery(s) => infer(of)(context ++ VarDecl(vn, Some(QueryType.generate(TupleQuery(s))), None, None)) match {
        case SetTupleQuery(t) => SetTupleQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected SetTupleQuery() for argument of BigUnion()")
      }
      case _ => throw ParseError("illegal query: " + q + "\nExpected SetTupleQuery() for domain of BigUnion()")
    }

    /** Intersections should be of the same type */
    case Intersection(l, r) =>
      checkCompatibility(l, r)

    /** Differences should be of the same set type */
    case Difference(o, w) =>
      checkCompatibility(o, w)

    /** Comprehensions need a set an a valid predicate as argument */
    case Comprehension(d, vn, p) =>
      infer(d) match {
        case SetTupleQuery(t) =>
          check(p)(context ++ VarDecl(vn, Some(QueryType.generate(TupleQuery(t))), None, None))
          SetTupleQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a SetTupleQuery() as argument of Comprehension()")
      }

    /** A tuple has to return an element each. */
    case Tuple(qs) =>
      val ts = qs map infer
      val bts = ts map {
        case ElementQuery(b) => b
        case _ => throw ParseError("illegal query: " + q + "\nExpected ElementQuery() as arguments to Tuple()")
      }
      TupleQuery(bts)

    /** for a projection we need to have enough elements */
    case Projection(p, i) => infer(p) match {
      case TupleQuery(s) =>
        if (0 < i && i <= s.length) {
          ElementQuery(s(i - 1))
        } else {
          throw ParseError("illegal query: " + q + "\nArgument to Projection() is too short")
        }
      case _ => throw ParseError("illegal query: " + q + "\nExpected List type as argument to Projection()")
    }

    /** query functions need to have the right input type but may be lifted to sets  */
    case QueryFunctionApply(fun, arg, param) =>
      val argType = infer(arg)
      if (argType == fun.in)
        fun.out
      else (fun.in, fun.out) match {
        // lifting simple functions to set-arguments
        case (TupleQuery(in), TupleQuery(out)) if argType == SetTupleQuery(in) =>
          SetTupleQuery(out)
        case _ => throw ParseError("illegal query: " + q + "\nWrong argument type for QueryFunctionApply()")
      }
  }
}