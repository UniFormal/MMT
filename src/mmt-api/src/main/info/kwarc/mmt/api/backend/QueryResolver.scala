package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.frontend.Controller

/**
  * An object that can resolve queries
  */
trait QueryResolver {
  /**
    * Resolves a query or throws [[NotApplicable]] and returns an iterable of matching paths
    *
    * @param q Query to resolve
    * @param controller Controller to use for query
    * @return an iterable of Paths that match the given query
    */
  def query(q: Query)(implicit controller: Controller): Iterable[Path] = throw NotApplicable("QueryResolver functionality not implemented")

  /**
    * Resolves a query or throws [[NotApplicable]] and returns an iterable of matching StructuralElements
    *
    * @param q
    * @param controller
    * @return
    */
  def queryAndGet(q : Query)(implicit controller: Controller) : Iterable[StructuralElement] = {
    query(q) map controller.get
  }
}

/**
  * A query resolver that uses other query resolvers in order and throws [[NotApplicable]] when
  * any one of them does not apply
  *
  * @param l
  */
class MultiResolver( l : List[QueryResolver]) extends QueryResolver {
  override def query(q : Query)(implicit controller : Controller) = {
    l.view.flatMap(_.query(q))
  }
  override def queryAndGet(q : Query)(implicit controller: Controller) = {
    l.view.flatMap(_.queryAndGet(q))
  }
}

/**
  * A query resolver that filters a given QueryResolver by two (optional) functions
  * @param qr
  * @param filterPath
  * @param filterGet
  */
class FilteredResolver(qr : QueryResolver, filterPath: Option[Path => Boolean] = None, filterGet : Option[StructuralElement => Boolean] = None) extends QueryResolver {

  private val filterPathFunction = filterPath.getOrElse((_:Path) => true)
  private val filterGetFunction = filterGet.getOrElse((_:StructuralElement) => true)

  override def query(q : Query)(implicit controller : Controller) = {
    qr.query(q).view.filter(filterPathFunction)
  }

  override def queryAndGet(q : Query)(implicit controller : Controller) = {
    qr.queryAndGet(q).view.filter(filterGetFunction)
  }
}

/**
  * A trait that resolves queries by calling the evaluator
  */
trait OntologyResolver extends QueryResolver {
  override def query(q: Query)(implicit controller: Controller) = {

    // TODO: Handle only this storages database
    // and not everything (is this possible)

    // TODO: Handle a set of list of paths also
    // by just joining them

    // check that the returned type is indeed a set of paths
    if (Query.infer(q)(Nil) == ESet(PathType)) {
      controller.evaluator.evaluate(q) match {
        case ElemResult(l:List[BaseType]) => l.asInstanceOf[List[Path]]
        case _ => throw NotApplicable("OntologyEvaluator not applicable: Query.infer() behaved unexpectedly")
      }

    // else it is not applicable
    } else {
      throw NotApplicable("OntologyEvaluator not applicable: Query did not return a list of paths")
    }
  }
}