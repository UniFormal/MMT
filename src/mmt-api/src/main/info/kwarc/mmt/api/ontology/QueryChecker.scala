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
  private def check(p: Prop)(implicit context: Context): Unit = {
    p match {

      /** the query has to be a single element */
      case IsA(e: Query, tp: Unary) => infer(e) match {
        case ElementQuery(_) =>
        case o@_ => throw ParseError(s"illegal proposition: $p\nExpected TupleQuery() as argument to IsA() but got $o")
      }

      /** PrefixOf expects to PathType queries */
      case PrefixOf(s, l) =>
        expectQueryType(s, ElementQuery(PathType))
        expectQueryType(l, ElementQuery(PathType))

      /** elem has to be an element and tp has to be a set of said type */
      case IsIn(elem, tp) =>
        (infer(elem), infer(tp)) match {
          case (ElementQuery(s), SetQuery(t)) if s == t =>
          case o@_ => throw ParseError(s"illegal proposition: $p\nExpected an TupleQuery() and SetTupleQuery() as arguments to IsIn() but got $o")
        }

      /** isEmpty can only check sets */
      case IsEmpty(r) => infer(r) match {
        case SetQuery(_) =>
        case o@_ => throw ParseError(s"illegal proposition: $p\nExpected SetTuple() as argument to IsEmpty() but got $o")
      }

      /** Equal needs to be elements of the same type */
      case Equal(left, right) => (infer(left), infer(right)) match {
        case (ElementQuery(s), ElementQuery(t)) if s == t =>
        case o@_ => throw ParseError(s"illegal proposition: $p\nExpected elements of same type as arguments to Equal() but got $o")
      }

      /** takes any two valid props */
      case Or(left, right) => check(left); check(right)

      /** exists has to be a set and the scope has to match */
      case Exists(domain, vn, scope) => infer(domain) match {
        case SetQuery(t) => check(scope)(context ++ VarDecl(vn, QueryType.toTerm(ElementQuery(t))))
        case o@_ => throw ParseError(s"illegal proposition: $p\nExpected SetTupleQuery() as domain of Forall() but got $o")
      }

      /** Not can take any valid prop */
      case Not(arg) => check(arg)

      /** takes any two valid props */
      case And(left, right) => check(left); check(right)

      /** forall has to be a set and the scope has to match */
      case Forall(domain, vn, scope) => infer(domain) match {
        case SetQuery(t) => check(scope)(context ++ VarDecl(vn, QueryType.toTerm(ElementQuery(t))))
        case o@_ => throw ParseError(s"illegal proposition: $p\nExpected SetTupleQuery() as domain of Forall() but got $o")
      }

      /** a judgement has to hold about a single object.  */
      case Holds(about, j) => infer(about) match {
        case ElementQuery1(PathType) =>
        case o@_ => throw ParseError(s"illegal proposition: $p\nExpected ElementQuery(Path) as argument to Holds() but got $o")
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
      case SetQuery(tp) => tp
      case ElementQuery(tp) => tp
    })
    if (ltp != rtp) {
      throw ParseError("illegal queries. Expected identical BaseTypes, got " + ltp + " and " + rtp)
    }
    SetQuery(ltp)
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
    case ElementQuery1(`tp`) => ElementQuery1(result)

    // a set of base types
    case SetQuery1(`tp`) => SetQuery1(result)

    // did not get the right type
    case o@_ => throw ParseError(s"illegal Query: $q\nExpected type: ElementQuery($tp) or SetElementQuery($tp) but got $o")

  }

  /**
    * Checks a relational expression and throws [[ParseError]] if it is invalid
    *
    * @param rel     Relational expression to check
    * @param context Context to check it in
    */
  private def check(rel: RelationExp)(implicit context: Context): Unit = {
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

    /** Slice() needs to slice a QuerySet */
    case Slice(qq, from, to) =>
      infer(qq) match {
        case SetQuery(st) => SetQuery(st)
        case o@_ => throw ParseError(s"illegal query: $q\nExpected a set of paths inside Slice(), but got $o")
      }

    /** Element picks a single element from a query returning a set */
    case Element(qq, idx) =>
      infer(qq) match {
        case SetQuery(st) => ElementQuery(st)
        case o@_ => throw ParseError(s"illegal query: $q\nExpected a set of paths inside Element(), but got $o")
      }

    /** lookup type of bound variable in context */
    case Bound(vn) =>
      QueryType.fromTerm(context(vn).tp.get)

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
        case ElementQuery1(PathType) => SetQuery1(PathType)
        case SetQuery1(PathType) => SetQuery1(PathType)
        case t => throw ParseError("illegal query: " + q + "\nExpected a set of paths inside Related()")
      }

    /** a single literal is just an element */
    case Literal(b) =>
      ElementQuery(infer(b))

    /** a list of literals is a literal for a tuple */
    case Literals(bs@_*) =>
      SetQuery(bs.map(infer).toList)

    /** infer the sub-query and check that the type still works */
    case Let(vn: LocalName, v: Query, in: Query) =>
      val vI = infer(v) match {
        case ElementQuery(s) => ElementQuery(s)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a TupleQuery() inside of Let()")
      }
      infer(in)(context ++ VarDecl(vn, QueryType.toTerm(vI)))

    /** turns a single element into a set containing just that element */
    case Singleton(e: Query) =>
      infer(e) match {
        case ElementQuery(t) => SetQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a single element inside of Singleton()")
      }

    /** a set of paths */
    case Paths(_) =>
      SetQuery(PathType)

    /** closure of a single path */
    case Closure(of) =>
      expectQueryType(of, ElementQuery(PathType))
      SetQuery(PathType)

    /** Union queries should be of the same type */
    case Union(l, r) =>
      checkCompatibility(l, r)

    /** Big Union: domain must be a set, inner union must be also be a set */
    case BigUnion(d, vn, of) => infer(d) match {
      case SetQuery(s) => infer(of)(context ++ VarDecl(vn, QueryType.toTerm(SetQuery(s)))) match {
        case SetQuery(t) => SetQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected SetTupleQuery() for argument of BigUnion()")
      }
      case _ => throw ParseError("illegal query: " + q + "\nExpected SetTupleQuery() for domain of BigUnion()")
    }

    /** Mapping: domain must be a set or element of terms */
    case Mapping(d, vn, fn) => infer(d) match {
      case SetQuery1(ObjType) => SetQuery1(ObjType)
      case ElementQuery1(ObjType) => ElementQuery1(ObjType)
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
        case SetQuery(t) =>
          check(p)(context ++ VarDecl(vn, QueryType.toTerm(ElementQuery(t))))
          SetQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a SetTupleQuery() as argument of Comprehension()")
      }

    /** A tuple has to return an element each. */
    case Tuple(qs) =>
      val ts = qs map infer
      val bts = ts map {
        case ElementQuery1(b) => b
        case _ => throw ParseError("illegal query: " + q + "\nExpected ElementQuery() as arguments to Tuple()")
      }
      ElementQuery(bts)

    /** for a projection we need to have enough elements */
    case Projection(p, i) => infer(p) match {
      case ElementQuery(s) =>
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
        case (ElementQuery(in), ElementQuery(out)) if argType == SetQuery(in) =>
          SetQuery(out)
        case _ => throw ParseError("illegal query: " + q + "\nWrong argument type for QueryFunctionApply()")
      }
  }
}
