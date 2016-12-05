package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.{LocalName, NamespaceMap, ParseError, Path}
import info.kwarc.mmt.api.objects._

object QueryChecker {
  // TODO: Actually write this theory and possible swap around the path
  private val QMTBaseTypes = Path.parseD("http://cds.omdoc.org/urtheories?QMT?BaseTypes", NamespaceMap.empty)
  private val pathPath = QMTBaseTypes ? "Path"
  private val objectPath = QMTBaseTypes ? "Object"
  private val xmlPath = QMTBaseTypes ? "XML"
  private val stringPath = QMTBaseTypes ? "String"

  /** Encodes a QueryBaseType as a Term */
  def Term2BaseType(t : Term) : QueryBaseType = t match {
    case OMID(`pathPath`) => PathType
    case OMID(`objectPath`) => ObjType
    case OMID(`xmlPath`) => XMLType
    case OMID(`stringPath`) => StringType
  }

  /** Decodes a Term into a QueryBaseType */
  def BaseType2Term(tp : QueryBaseType) : Term = OMID(QMTBaseTypes ? tp.name)

  /**
    * Checks that a proposition is well-formed or throws a [[ParseError]]
    * @param p Proposition to check
    * @param context Context to check proposition in
    */
  private def check(p : Prop)(implicit context: Context) {
    p match {
      /** the query has to be a single element */
      case IsA(e : Query, tp: Unary) => infer(e) match {
        case TupleQuery(_) =>
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected an Element as argument to IsA()")
      }

      /** PrefixOf expects to PathType queries */
      case PrefixOf(s, l) =>
        check(s, ElementQuery(PathType))
        check(l, ElementQuery(PathType))

      /** elem has to be an element and tp has to be a set of said type */
      case IsIn(elem, tp) =>
        (infer(elem), infer(tp)) match {
          case (TupleQuery(s), SetTupleQuery(t)) if s == t =>
          case _ => throw ParseError("illegal proposition: " + p + "\nExpected an Element and a Set of the same type as arguments to IsIn()")
        }

      /** isEmpty can only check sets */
      case IsEmpty(r) => infer(r) match {
        case SetTupleQuery(_) =>
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected a set as argument to IsEmpty()")
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
        case SetTupleQuery(t) => check(scope)(context ++ VarDecl(vn, Some(BaseType2Term(t.head)), None, None))
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected set as domain of Forall()")
      }

      /** Not can take any valid prop */
      case Not(arg) => check(arg)

      /** takes any two valid props */
      case And(left, right) => check(left); check(right)

      /** forall has to be a set and the scope has to match */
      case Forall(domain, vn, scope) => infer(domain) match {
        case SetTupleQuery(t) => check(scope)(context ++ VarDecl(vn, Some(BaseType2Term(t.head)), None, None))
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected set as domain of Forall()")
      }

      /** a judgement has to hold about a single object.  */
      // TODO: Does it make sense to lift this to sets?
      case Holds(about, varname, j) => infer(about) match {
        case ElementQuery(ObjType) =>
        case _ => throw ParseError("illegal proposition: " + p + "\nExpected object as argument to Holds()")
      }
    }
  }

  /**
    * Checks that two queries are of the same set type.
    * @param l Left query to check
    * @param r Right query to check
    * @param context Context to check
    * @return
    */
  private def check(l: Query, r: Query)(implicit context: Context): QueryType = (infer(l), infer(r)) match {
    case (SetTupleQuery(s), SetTupleQuery(t)) if s == t => SetTupleQuery(s)
    case (TupleQuery(s), TupleQuery(t)) if s == t => SetTupleQuery(s)
    case (SetTupleQuery(s), TupleQuery(t)) if s == t => SetTupleQuery(s)
    case (TupleQuery(s), SetTupleQuery(t)) if s == t => SetTupleQuery(s)
    case _ => throw ParseError("illegal queries. Expected identical set types, but got " + l + " and " + r)
  }
  /**
    * Infers the type of a Query and throws [[ParseError]] if it does not match the expected type of tp
    * @param q Query to infer
    * @param tp Type of query to expect
    * @param context Context under which to infer query
    */
  private def check(q : Query, tp : QueryType)(implicit context: Context) : QueryType = {
    val it = infer(q)
    if (it != tp) {
      throw ParseError("illegal query: " + q + "\nExpected type: " + tp + "Actual type: " + it)
    }
    tp
  }

  /**
    * Checks a relational expression and throws [[ParseError]] if it is invalid
    * @param rel Relational expression to check
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
    }
  }

  /**
    * Infers the return type of a base type
    * @param b BaseType to infer
    * @return
    */
  private def infer(b: BaseType) : QueryBaseType  = b match {
    case p: Path => PathType
    case s: StringValue => StringType
    case x: XMLValue => XMLType
    case o: Obj => ObjType
  }


  /**
    * Infers the type of a Query
    * @param q Query to infer
    * @param context Context under which to infer Query.
    * @return
    */
  def infer(q : Query)(implicit context: Context): QueryType = q match {
    /** I() just gives a context free hint => this is fine. */
    case I(qq, h) => infer(qq)

    /** infer the type of the bound variable from the context */
    case Bound(vn) => ElementQuery(Term2BaseType(context(vn).tp.get))

    /** component of paths, lifted to set of paths */
    case Component(of, _) => infer(of) match {
      case ElementQuery(PathType) => ElementQuery(ObjType)
      case SetElementQuery(PathType) => SetElementQuery(ObjType)
      case _ => throw ParseError("illegal query: " + q + "\nExpected a path or set of paths inside Component()")
    }

    /** check that we actually have an object query inside */
    case SubObject(of, _) =>
      check(of, ElementQuery(ObjType))

    /** a set of paths related to a path or a set of paths */
    case Related(to, by) =>
      check(by)
      infer(to) match {
        case ElementQuery(PathType) => SetElementQuery(PathType)
        case SetElementQuery(PathType) => SetElementQuery(PathType)
        case t => throw ParseError("illegal query: " + q + "\nExpected a set of paths inside Related()")
      }

    /** a single literal is just an element */
    case Literal(b) =>
      ElementQuery(infer(b))

    /** a set of literals is a tuple */
    case Literals(bs@_*) =>
      if (bs.isEmpty) {
        throw ParseError("illegal query: " + q + "\nExpected a non-empty set of Literals()")
      } else {
        val bst = bs.map(infer)
        if (bst.toSet.size != 1) {
          throw ParseError("illegal query: " + q + "\nExpected a homogeneous set of Literals()")
        }
        SetTupleQuery(bst.toList)
      }

    /** infer the sub-query and check that the type still works */
    case Let(vn: LocalName, v: Query, in: Query) =>

      val vI = infer(v) match {
        case TupleQuery(s) => s
        case _ => throw ParseError("illegal query: " + q + "\nExpected a single element inside of Let()")
      }

      infer(in)(context ++ VarDecl(vn, Some(BaseType2Term(vI.head)), None, None))

    /** turns a single element into a set of just that element */
    case Singleton(e : Query) =>
      infer(e) match {
        case TupleQuery(t) => SetTupleQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a single element inside of Singleton()")
      }

    /** a set of paths */
    case Paths(_) => SetElementQuery(PathType)

    /** a set of objects */
    case Unifies(_) => SetElementQuery(ObjType)

    /** closure of a single path */
    case Closure(of) =>
      check(of, ElementQuery(PathType))
      SetElementQuery(PathType)

    /** Union queries should be of the same type */
    case Union(l, r) =>
      check(l, r)

    /** Big Union: domain must be a set, inner union must be also be a set */
    case BigUnion(d, vn, of) => infer(d) match {
      case SetTupleQuery(s) => infer(of)(context ++ VarDecl(vn, Some(BaseType2Term(s.head)), None, None)) match {
        case SetTupleQuery(t) => SetTupleQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a set for argument of BigUnion()")
      }
      case _ => throw ParseError("illegal query: " + q + "\nExpected a set for domain of BigUnion()")
    }

    /** Intersections should be of the same type */
    case Intersection(l, r) =>
      check(l, r)

    /** Differences should be of the same set type */
    case Difference(o, w) =>
      check(o, w)

    /** Comprehensions need a set an a valid predicate as argument */
    case Comprehension(d, vn, p) =>
      infer(d) match {
        case SetTupleQuery(t) =>
          check(p)(context ++ VarDecl(vn, Some(BaseType2Term(t.head)), None, None))
          SetTupleQuery(t)
        case _ => throw ParseError("illegal query: " + q + "\nExpected a set as argument of Comprehension()")
      }

    /** A tuple has to return an element each. */
    case Tuple(qs) =>
      val ts = qs map infer
      val bts = ts map {
        case ElementQuery(b) => b
        case _ => throw ParseError("illegal query: " + q + "\nExpected elements as arguments to Tuple()")
      }
      TupleQuery(bts)

    /** for a projection we need to have enough elements */
    case Projection(p, i) => infer(p) match {
      case TupleQuery(s) =>
        if (0 < i && i <= s.length){
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