package info.kwarc.mmt.moduleexpressions.operators.typeops

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.UnaryConstantScala
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType}
import info.kwarc.mmt.moduleexpressions.operators._

import scala.collection.mutable

object MultiTypeIndexifier extends UnaryConstantScala(Combinators._path, "multi_typeindexifier")

// Store which typeindexed sorts the declarations in our inputTheory (transitively) depend on
// E.g. if we have `op: tm a`, then we would have `op |-> Set(a)`
// E.g. if we have additionally `op2: tm b -> tm a ❘ = op`, then we would have `op |-> Set(a, b)`
final class ComputeMultiTypeIndexedHelperContext(val sortDependencies: mutable.HashMap[LocalName, List[LocalName]], val sortDependenciesSeeker: SortDependenciesSeeker) extends LinearTheoryOperatorContext {
}

object ComputeMultiTypeIndexed extends FunctorialLinearDiagramOperator[ComputeMultiTypeIndexedHelperContext](MultiTypeIndexifier) {
  override protected def initialTheoryHelperContext: ComputeMultiTypeIndexedHelperContext
  = new ComputeMultiTypeIndexedHelperContext(mutable.HashMap(), new SortDependenciesSeeker)

  override def applicableOnTheory(thy: AnonymousTheory): Boolean = {
    // TODO: Check that the meta-theory contains SFOL (ex. of implicit morphism, I guess) */
    true
  }

  override def transformSingleDeclaration(decl: OML, helperContext: ComputeMultiTypeIndexedHelperContext)
  : OperatorResult[(OML, ComputeMultiTypeIndexedHelperContext)] = decl.tp match {
    case Some(SFOL.FunctionOrPredicateType(_)) =>
      helperContext.sortDependencies.put(
        decl.name,
        helperContext.sortDependenciesSeeker.getDependenciesForDecl(decl, helperContext.sortDependencies)
      )
      val newDecl = MultiTypeIndexer.typeIndex(decl, helperContext.sortDependencies)

      TransformedResult((newDecl, helperContext))

    // vvv Remainder to potentially account for this in the future
    case Some(PL.ded(_)) => NotApplicable()
    case _ => NotApplicable()
  }
}

private object MultiTypeIndexer {

  /**
    * (Multi-)typeindex declaration
    *
    * @param decl The declaration to typeindex
    * @param allDependencies The (irreflexive and transitively-closed) dependencies of declarations to sorts.
    *                        Every value in this map shall be a duplicate-free (!) sequence.
    *                        NB: We use a sequence to force a consistent order of dependencies, which is important
    *                            because we will convert dependencies to depently type parameters, which naturally
    *                            have a position, i.e. order. [[SortDependenciesSeeker.getDependenciesForDecl()]]
    *                            takes care of acquiring a duplicate-free list of them. The caller should use this
    *                            method to produce the `allDependencies` argument passed to this function.
    * @return The typeindexed version of the input declaration `decl`.
    */
  def typeIndex(decl: OML, allDependencies: collection.Map[LocalName, collection.Seq[LocalName]]): OML = {
    assert(allDependencies.contains(decl.name))

    val adder = new DependenciesAndTypeOperatorAdder

    val tmpDecl = adder.traverse(decl)(Context.empty, allDependencies).asInstanceOf[OML] // return value is indeed an OML by recursion
    val ownDependenciesToAdd = allDependencies(decl.name)

    tmpDecl.copy(
      tp = tmpDecl.tp.map(dependentlyTypeTypeComponent(_, ownDependenciesToAdd)),
      df = tmpDecl.df.map(lambdaBindDefComponent(_, ownDependenciesToAdd)),

      // Adjust notation for dependent types we added
      nt = tmpDecl.nt.map(notation => notation.copy(fixity = notation.fixity.addInitialImplicits(ownDependenciesToAdd.size)))
    )
  }

  def dependentlyTypeTypeComponent(typeComponent: Term, dependenciesToAbstract: Seq[LocalName]): Term = {
    val dependentlyBoundVariables = dependenciesToAbstract.map(sortName =>
      (Some(sortName), OMID(TypedTerms.typeOfSorts))
    ).toList

    FunType(dependentlyBoundVariables, typeComponent)
  }

  def lambdaBindDefComponent(defComponent: Term, dependenciesToBind: Seq[LocalName]): Term = {
    val variablesToBind = dependenciesToBind.map(sortName =>
      (sortName, OMID(TypedTerms.typeOfSorts))
    ).toList

    FunTerm(variablesToBind, defComponent)
  }

  final private class DependenciesAndTypeOperatorAdder extends Traverser[collection.Map[LocalName, collection.Seq[LocalName]]] {
    def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      // We reference another decl, which is *not* shadowed by some binder
      case ApplySpine(referencedDecl@OML(_, _, _, _, _), args) if state.contains(referencedDecl.name) =>
        val sortDependencies = state.getOrElse(referencedDecl.name, Nil)

        val newArgs = sortDependencies.map(sortName => OML(sortName, None, None, None, None)).toList ::: args
        ApplySpine(referencedDecl, newArgs: _*)

      case ApplySpine(OMID(TypedTerms.termsOfSort), sort) =>
        ApplySpine(OMID(TypedTerms.termsOfSort), ApplySpine(OMID(TypeOperator.typeOp), sort: _*))

      case t => Traverser(this, t)
    }
  }

}

final class SortDependenciesSeeker extends Traverser[(collection.Map[LocalName, collection.Seq[LocalName]], mutable.HashSet[LocalName])] {

  def getDependenciesForDecl(decl: OML, currentDependencies: collection.Map[LocalName, collection.Seq[LocalName]]): List[LocalName] = {
    implicit val context: Context = Context.empty
    implicit val state: State = (currentDependencies, mutable.HashSet[LocalName]())

    // Beware to not call traverse on decl itself
    // Since it then would confuse the outer OML with an OML reference to another declaration
    // (The underlying "problem" is that OMLs can function both as declarations as well as references to declarations.)
    decl.tp.map(traverse)
    decl.df.map(traverse)

    // TODO: Enforce some arbitrary order, fix alphabetical order sometime
    state._2.toList
  }

  def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
    // We reference another decl, which is *not* shadowed by some binder
    case OML(referencedDecl, _, _, _, _) if !con.exists(_.name == referencedDecl) =>
      state._2 ++= state._1.getOrElse(referencedDecl, Set())
      null
    case ApplySpine(OMID(TypedTerms.termsOfSort), List(OML(referencedSort, _, _, _, _))) =>
      state._2 += referencedSort
      null
    case t => Traverser(this, t)
  }
}