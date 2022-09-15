package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import objects._
import objects.Conversions._

import scala.collection.mutable
import scala.collection.mutable.HashSet



object QueryEvaluator {
  val free = OMID(utils.mmt.mmtbase ? "mmt" ? "free")
  type QuerySubstitution = List[(LocalName, BaseType)]
}


object ResultSet {
  def singleton(b : BaseType) : mutable.HashSet[List[BaseType]] = fromElementList(List(b))

  def fromElementList( lst : Seq[BaseType]): mutable.HashSet[List[BaseType]] = {
    val r = new mutable.HashSet[List[BaseType]]
    lst.foreach {
      r += List(_)
    }
    r
  }
  def fromTupleList( lst : Seq[Seq[BaseType]]) : mutable.HashSet[List[BaseType]] = {
    val r = new mutable.HashSet[List[BaseType]]
    lst.foreach {
      r += _.toList
    }
    r
  }
}

/** evaluates a query expression to a query result */
class QueryEvaluator(controller: Controller) {

  private def evaluators : List[QueryExtension] = controller.extman.get(classOf[QueryExtension])

  /**
    * Evaluates a Query in memory, expects all I()s to be resolved already
    *
    * @param q
    * @return
    */
  def apply(q: Query): QueryResult = {
    log(q.toString)
    QueryChecker.infer(q)(Context.empty) match {
      case ElementQuery(_) => ElemResult(evalElem(q)(Nil))
      case SetQuery(_) => SetResult(evalSet(q)(Nil).map(ElemResult))
    }
  }

  private def log(msg: => String): Unit = {
    controller.report("query", msg)
  }

  private def empty = mutable.HashSet.empty[List[BaseType]]

  private def singleton(b: BaseType) = {
    val res = empty
    res += List(b)
    res
  }

  private lazy val rs = controller.depstore
  private lazy val lup = controller.globalLookup

  /**
    * Evaluates an element query.
    *
    * @param q Query to evaluate
    * @param subst Substiution (Context) to evaluate query in
    * @return
    */
  def evalElem(q: Query)(implicit subst: QueryEvaluator.QuerySubstitution): List[BaseType] = evalSet(q).head // just take the head

  /**
    * Evaluates a Elem[_] query
    *
    * @param q Query to evaluate
    * @param subst Substiution (Context) to evaluate query in
    *
    * @return
    */
  def evalSingleElem(q: Query)(implicit subst: QueryEvaluator.QuerySubstitution): BaseType = evalElem(q).head


  /**
    * Evaluates a Elem[Path] Query
    *
    * @param e Query to evaluate
    * @param subst Substiution (Context) to evaluate query in
    * @return
    */
  def evalSinglePath(e: Query)(implicit subst: QueryEvaluator.QuerySubstitution): Path = evalSingleElem(e).asInstanceOf[Path]

  /**
    * Evaluates a Elem[Obj] Query
    *
    * @param e Query to evaluate
    * @param subst Substiution (Context) to evaluate query in
    * @return
    */
  def evalElemObj(e: Query)(implicit subst: QueryEvaluator.QuerySubstitution): Obj = evalSingleElem(e).asInstanceOf[Obj]

  /**
    * Evaluates a SetTuple[_] query
    *c
    * @param q Query to evaluate
    * @param subst Substiution (Context) to evaluate query in
    * @return
    */
  def evalSet(q: Query)(implicit subst: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]] = q match {
    /** evaluate a query with a hint */
    case I(qq, Some(h)) =>
      val matching = evaluators.filter(_.name == h)
      if(matching.isEmpty){
        throw ImplementationError("ill-typed query: Missing extenstion for QueryHint. ")
      }
      matching.head.evaluate(qq, this)

    /** evaluate a query without a hint */
    case I(qq, None) =>
      log("Found I() with an empty hint, ignoring ... ")
      evalSet(qq)

    case Slice(qq, from, to) =>
      // we make the subquery
      val sub = evalSet(qq).toList

      // find out startIndex
      val startIndex = from.map(f => {
        if(f < 0) Math.max(0, sub.length - f) else Math.min(f, sub.length - 1)
      }).getOrElse(0)

      // find out endIndex
      val endIndex = to.map(t => {
        if(t < 0) Math.min(sub.length - 1, sub.length - t) else Math.max(0, t)
      }).getOrElse(sub.length - 1)

      // and return the appropriate list
      ResultSet.fromTupleList(sub.slice(startIndex, endIndex))

    /** pick a specific element from a set */
    case Element(qq, at) =>
      val sub = evalSet(qq).toList(at)
      ResultSet.fromTupleList(List(sub))

    /** bound variable => lookup in the substitution */
    case Bound(vn) =>
      singleton(subst.find(_._1 == vn).get._2)

    /** get the components of a path */
    case ontology.Component(of, comp) =>
      val res = empty
      evalSet(of) foreach {
        case List(p: ComponentParent) =>
          val se = lup.get(p)
          se.getComponent(comp) match {
            case Some(tc: AbstractObjectContainer) => tc.get foreach {
              res += List(_)
            }
            case Some(cc) => throw GetError(p $ comp, s"component exists but is not an object: $cc")
            case _ => throw GetError(p $ comp, "component does not exist")
          }
        case _ => throw ImplementationError("ill-typed query")
      }
      res

    /** retrieve a subobject of a single result */
    case SubObject(of, pos) =>
      val res = empty
      evalSet(of).foreach(e => {
        res += e.map(f => {
          val (con, obj) = f.asInstanceOf[Obj].subobject(pos)
          obj match {
            case t: Term => OMBIND(QueryEvaluator.free, con, t)
            case o => o //TODO assuming bound variables occur only in terms
          }
        })
      })
      res

    /** query the rs for all the objects */
    case Related(to, by) =>
      val res = empty
      def add(p : Path): Unit = {res += List(p)}
      evalSet(to) foreach { p =>
        rs.query(p.head.asInstanceOf[Path], by)(add) // p has type List(Path) by precondition
      }
      res

    /** Literal => return the item as is */
    case Literal(b) =>
      ResultSet.singleton(b)

    /** Literals => return the items as is */
    case Literals(bs@_*) =>
      val res = empty
      bs foreach {
        res += List(_)
      }
      res

    /** evaluate the outer query and add to the context */
    case Let(vn, v, in) =>
      val vE = evalSingleElem(v)
      evalSet(in)((vn, vE) :: subst)

    /** evaluate a singleton query */
    case Singleton(e) =>
      val res = empty
      res += evalElem(e)
      res

    /** get a set of paths to objects */
    case Paths(c) =>
      ResultSet.fromElementList(rs.getInds(c).toSeq)

    /** close of a set of paths */
    case ontology.Closure(of) =>
      evalSinglePath(of) match {
        case p: MPath =>
          val res = empty
          rs.theoryClosure(p) foreach {
            res += List(_)
          }
          res
        case p => throw GetError(p, "not a module path")
      }

    /** union of the result sets */
    case Union(s, t) =>
      evalSet(s) union evalSet(t)

    /** a big union of sets */
    case BigUnion(dom, vn, s) =>
      val res = empty
      evalSet(dom) foreach {
        e => evalSet(s)((vn, e.head) :: subst) foreach { x => res += x }
      }
      res

    /** a map over a domain */
    case Mapping(dom, vn, fn) =>
      evalSet(dom).map(x => {

        // create a substiution
        val sub = vn / (x match {
          case List(t:Term) => t
          case _ => throw ImplementationError("precondition failed: Argument must be a term")
        })

        // and apply it
        List(fn ^? sub)
      })

    /** intersection of sets */
    case Intersection(s, t) =>
      evalSet(s) intersect evalSet(t)

    /** set difference */
    case Difference(o, w) =>
      evalSet(o) diff evalSet(w)

    /** comprehension filters by a predicate */
    case Comprehension(dom, vn, pred) =>
      evalSet(dom) filter { e => evalProp(pred)((vn, e.head) :: subst) }

    /** Tuple: run each query one by query */
    case Tuple(l) =>
      val res = empty
      res += l map { q => evalSingleElem(q) }
      res

    /** Projection takes the ith element */
    case Projection(p, i) =>
      val t = evalElem(q)
      val res = empty
      res += List(t(i - 1))
      res

    /** apply a query function using the function itself */
    case QueryFunctionApply(fun, args, param) =>
      fun.evaluate(q, this)

    case _ => throw ImplementationError("unknown query")
  }

  private def evalProp(p: Prop)(implicit subst: QueryEvaluator.QuerySubstitution): Boolean = p match {
    /* check e is a c */
    case IsA(e, c) =>
      rs.getInds(c) contains evalSinglePath(e)

    /** check if a path is a prefix of another */
    case PrefixOf(short, long)
    => evalSinglePath(short) <= evalSinglePath(long)

    /** check if e is in t */
    case IsIn(e, t) =>
      evalSet(t) contains evalElem(e)

    /** check if a result set is empty */
    case IsEmpty(r) =>
      evalSet(r).isEmpty

    /** check if two elements are equal */
    case Equal(l, r) =>
      evalElem(l) == evalElem(r)

    /** check if one or the other is true */
    case Or(l, r) =>
      evalProp(l) || evalProp(p)

    /** check if a single value exists */
    case Exists(dom, vn, sc) =>
      evalSet(dom) exists { e => evalProp(sc)((vn, e.head) :: subst) }

    /** check that something is not the case */
    case Not(q) =>
      !evalProp(q)

    /** check that both are true */
    case And(l, r) =>
      evalProp(l) && evalProp(p)

    /** check that it is true for all values */
    case Forall(dom, vn, sc) =>
      evalSet(dom) forall { e => evalProp(sc)((vn, e.head) :: subst) }

    /** check that a judgement holds for a given item */
    case Holds(about, j) =>
      // fetch the item we are talking about
      val pth = evalSinglePath(about) match {
        case gn: GlobalName => gn
        case _ => throw ImplementationError("precondition failed: Argument to Holds() has to return GlobalName")
      }

      val properJudgement = j.toJudgement(pth)
      // TODO
      // @dennis: need to evaluate 'properJudgement' and return a boolean
      throw ImplementationError("Holds() predecate not implemented")
  }
}
