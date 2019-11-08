package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects.{Term, _}
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.{SkipThis, URI}
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName}
import info.kwarc.mmt.lf.{ApplySpine, FunType}

import scala.collection.mutable

private object TypeOperator extends TheoryScala {
  val _base = DPath(URI("https://example.com/diagops"))
  val _name = LocalName("TypeOperator")

  val typeOp: GlobalName = _path ? "typeOp"
}

object TypeIndexifier extends UnaryConstantScala(Combinators._path, "typeindexifier") {
  /** the label of the distinguished node of the output diagram */
  val nodeLabel = LocalName("pres")
}

object ComputeTypeIndexed extends ComputationRule(TypeIndexifier.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {

    /* Unwrap input */
    val TypeIndexifier(inputTerm) = tm
    val inputDiagram: AnonymousDiagram = Common.asAnonymousDiagram(solver, inputTerm).getOrElse {
      return RecurseOnly(List(1))
    }
    val inputNode = inputDiagram.getDistNode.getOrElse(return Recurse)
    val inputTheory = inputNode.theory

    /* Sanity Check */
    /* TODO: Check that the meta-theory contains SFOL, not that it is SFOL */
    /*
        def translateTerm(tm: Term): Term = tm match {

        }*/

    def checkConformingAndCollectSorts(args: List[Term]): Option[List[LocalName]] = {
      var sortNames = new mutable.LinkedHashSet[LocalName]()

      val conforming = args.forall({
        case ApplySpine(TypedTerms.typeOfSorts, List(OML(sortName, _, _, _, _))) =>
          sortNames += sortName
          true
        case _ => false
      })

      if (conforming) {
        Some(sortNames.toList)
      } else {
        None
      }
    }

    object SFOLFunction {
      /**
        *
        * @param decl
        * @return List of unique occurring sort local names in the order they appear in the function type from left to right
        */
      def unapply(decl: Term): Option[List[LocalName]] = decl match {
        case FunType(args, returnType) => {
          if (!args.forall(_._1.isEmpty)) {
            None
          } else {
            checkConformingAndCollectSorts(returnType :: args.map(_._2))
          }
        }
        case _ => None
      }
    }

    object SFOLPredicate {
      def unapply(decl: Term): Option[List[LocalName]] = decl match {
        case FunType(args, OMS(PL.prop)) =>
          if (!args.forall(_._1.isEmpty)) {
            None
          } else {
            checkConformingAndCollectSorts(args.map(_._2))
          }
        case _ => None
      }
    }

    class TypeIndexerTraverser extends StatelessTraverser {
      def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case ApplySpine(TypedTerms.typeOfSorts, sort) =>
          ApplySpine(OMID(TypedTerms.typeOfSorts), ApplySpine(OMID(TypeOperator.typeOp), sort: _*))
        case t => Traverser(this, t)
      }
    }

    def typeindexTypeComponent(term: Term, sortNames: List[LocalName]): Term = {
      // e.g. original declaration was:  `op: tm a -> tm b -> tm c`
      // then sortNames as passed to this function should be List("a", "b", "c")
      // and we now construct the new type `{a: tp, b: tp, c: tp} tm (&a) -> tm (&b) -> tm (&c)`
      val dependentlyBoundVariables = sortNames.map(sortName => (Some(sortName), OMID(TypedTerms.typeOfSorts)))
      val translatedTerm = (new TypeIndexerTraverser).apply(term, Context())

      FunType(dependentlyBoundVariables, translatedTerm)
    }

    // TODO: How to add include to anonymous theory?
    /*
    val typeOperatorIncludeDeclaration = Include(home = OMID(newModulePath), from = R.path, args = Nil)
      outLinkDeclarations += Include(
        home = OMID(newMorphismPath),
        from = R.path,
        args = Nil,
      df = Some(RToS.toTerm)
    )
    */

    val outputDeclarations = inputTheory.getDeclarations.mapOrSkip(inputDecl => {
      val declType = inputDecl.tp.getOrElse(throw SkipThis)
      val newType = declType match {
        case SFOLFunction(sortNames) => typeindexTypeComponent(declType, sortNames)
        case SFOLPredicate(sortNames) => typeindexTypeComponent(declType, sortNames)

        case TypedTerms.typeOfSorts => throw SkipThis
        case PL.ded(_) => throw SkipThis
        case _ => throw SkipThis
      }

      // TODO Care for definiens as well, currently for simplicity just None'd out
      // Construct the new declaration
      new OML(inputDecl.name, Some(newType), None, inputDecl.nt, inputDecl.featureOpt)
    })

    /* Build output diagram */
    val outputDiagram = {
      val outputNode = DiagramNode(
        TypeIndexifier.nodeLabel,
        AnonymousTheory(inputTheory.mt, outputDeclarations)
      )

      AnonymousDiagram(List(outputNode), Nil, Some(TypeIndexifier.nodeLabel))
    }

    Simplify(outputDiagram.toTerm)
  }
}
