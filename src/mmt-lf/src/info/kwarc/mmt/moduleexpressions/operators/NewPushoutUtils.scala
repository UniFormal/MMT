package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.modules.{Link, Theory, View}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.SkipThis
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations.NotationContainer

import scala.collection.mutable

/**
  * Helper functions for computing pushouts of [[AnonymousTheory anonymous theories]].
  *
  * @todo Rename to sensible name, beware that [[PushoutUtils]] is already taken by Common.scala in the same directory.
  */
object NewPushoutUtils {
  /**
    * Compute the pushout of the diagram below:
    *
    * '''
    * A ---> B
    * (
    * v
    * C
    * '''
    *
    * where the morphism from A to C is an inclusion.
    *
    * @param thyB       Theory B in the above diagram.
    * @param thyAToThyB The morphism from A to B in the above diagram.
    * @param thyC       Theory C in the above diagram.
    * @return A tuple containing the generated morphism `C ---> pushoutTheory` and the `pushoutTheory` itself.
    */
  def computePushoutAlongInclusion(thyB: AnonymousTheory, thyAToThyB: AnonymousMorphism, thyC: AnonymousTheory): (AnonymousMorphism, AnonymousTheory) = {
    // Computing the pushout along an inclusion is rather straightforward:
    //
    // For every declaration in C, we effectively homomorphically apply the morphism thyAToThyB to the type and
    // definiens component of that declaration, and adopt that new declaration in our pushout theory.
    //
    // Care should be taken that [[AnonymousTheory]]s have flattened inclusions. Hence, we only do the above
    // procedure on declarations in C for which thyAToThyB does not provide a mapping yet (which would imply
    // that the declaration was already in thyA - which is left implicit).

    // [[Substitution]]s just recurse into def and type component on [[OML]]s and do not replace itself
    // if needed. Hence, we need an [[OMLReplacer]].
    val omlTranslator = OMLReplacer(thyAToThyB.toSubstitution).toTranslator()

    // Compute Set representations to later catch name clashes efficiently
    val alreadyMappedDecls = thyAToThyB.getDeclarations.map(_.name).toSet
    val reservedThyBNames = thyB.getDeclarations.map(_.name).toSet

    // Compute the main part of the pushout
    val (newMorphismDecls, newTheoryDecls) = thyC.getDeclarations.mapOrSkip(decl =>
      // We think of `decl` being included from thyA
      if (alreadyMappedDecls.contains(decl.name)) {
        throw SkipThis
      } else if (reservedThyBNames.contains(decl.name)) { // Name clash
        throw GeneralError(s"Cannot compute canonical pushout due to name clash of ${decl.name} in anonymous theory with equally called declaration in anonymous theory")
      } else { // Homomorphically translate and adopt in pushout
        val declInPushout = decl.copy(
          tp = decl.tp.map(omlTranslator.apply(Context.empty, _)),
          df = decl.df.map(omlTranslator.apply(Context.empty, _))
        )

        val declInMorphismIntoPushout = OML(decl.name, None, Some(OML(decl.name, tp = None, df = None)))

        (declInMorphismIntoPushout, declInPushout)
      }
    ).unzip

    // Finalize our computation by prepending the existing theory and morphism declarations
    val pushoutTheory = AnonymousTheory(thyB.mt, thyB.getDeclarations ::: newTheoryDecls)
    val morphismIntoPushout = AnonymousMorphism(thyAToThyB.getDeclarations ::: newMorphismDecls)

    (morphismIntoPushout, pushoutTheory)
  }
}

object NamedPushoutUtils {
  /*def computeCanonicalPushout(C: Theory, v: Link)(implicit lookup: Lookup) = {
    lookup.getImplicit(v.from, C.toTerm) match {
      case Some(implicitPath) =>
        // println(implicitPath)
      case None => ???
    }
  }*/

  /**
    * Create a [[Traverser]] applying a [[Link]]
    * @todo Does this work for links with inclusions of other links?
    */
  private def linkToTraverser(link: Link): Traverser[Unit] = {
    val assignments = link.getDeclarationsElaborated
    OMSReplacer(globalName =>
      assignments.find(decl =>
        decl.isInstanceOf[FinalConstant] && decl.name == LocalName(ComplexStep(globalName.module) :: globalName.name)
      ).flatMap(_.asInstanceOf[FinalConstant].df)
    )
  }

  /**
    * Compute the canonical pushout along a direct inclusion if possible.
    *
    * Pictorially, we compute the pushout
    *
    * '''
    *    v
    * A ---> B
    * (      )
    * v      v
    * C ---> D
    *    w
    * '''
    *
    * where C must directly include A.
    *
    * @param D_to_generate The module path to use for the newly generated theory D.
    * @param v The [[Link]] from A to B.
    * @param w_to_generate The module path to use for the newly generated view from C to D.
    * @return
    */
  def computeCanonicalPushoutAlongDirectInclusion(A: Theory, B: Theory, C: Theory, D_to_generate: MPath, v: Link, w_to_generate: MPath): (Theory, View) = {
    // Assert C including A directly
    assert(
      C.getAllIncludes.contains(IncludeData(OMMOD(C.path), A.path, Nil, None, total = false)),
      "C does not include A directly as required by this function"
    )
    assert(
      v.from == OMMOD(A.path) && v.to == OMMOD(B.path),
      "Link v does not go from A to B as required by this function"
    )

    // The decls of the to-be-generated pushout theory
    val newDecls: mutable.ListBuffer[Declaration] = mutable.ListBuffer()

    // The decls of the to-be-generated view into the pushout theory
    val newViewDecls: mutable.ListBuffer[Declaration] = mutable.ListBuffer()

    val pushedOutDecls: mutable.Map[GlobalName, GlobalName] = mutable.HashMap()

    // For every declaration in C we will apply our link v: A -> B homomorphically
    // to its type and definiens component and then adopt it with the same name
    // in D.
    // However, following declarations in C might use previously translated declarations.
    // These of course are not contained anymore in the domain of v.
    // Hence, we first rename them to the target theory D and then apply v homomorphically.
    //
    // We do the latter by means of a traverser to silently ignore unapplyable terms (e.g.
    // the ones we just renamed!)
    val traverser = Renamer(pushedOutDecls.get(_)).compose(linkToTraverser(v))

    for (decl <- C.getDeclarations) {
      decl match {
        case c: FinalConstant =>
          // TODO: How to check for name clash, i.e. non-existence of a canonical pushout?
          if (false) {
            throw GeneralError(s"Cannot compute canonical pushout due to name clash of to-be-generated ${decl.name} in context of ${B.path}")
          }

          val newType = TermContainer.asParsed(c.tp.map(traverser.apply(_, Unit)))
          val newDefiniens = TermContainer.asParsed(c.df.map(traverser.apply(_, Unit)))

          val newDecl = new FinalConstant(
            home = OMMOD(D_to_generate),
            name = c.name,
            alias = c.alias,
            tpC = newType,
            dfC = newDefiniens,
            rl = c.rl,
            notC = c.notC,
            vs = c.vs
          )
          newDecls += newDecl
          pushedOutDecls += (decl.path -> newDecl.path)

          newViewDecls += new FinalConstant(
            home = OMMOD(w_to_generate),
            name = LocalName(ComplexStep(decl.parent) :: decl.name),
            alias = Nil,
            tpC = new TermContainer(),
            dfC = TermContainer.asParsed(Some(newDecl.toTerm)),
            rl = None,
            notC = new NotationContainer,
            vs = Visibility.public
          )

        case SimpleStructure(_, fromPath) if fromPath == A.path =>
          newDecls += Include(OMMOD(D_to_generate), B.path, Nil, None)

          // Inherit v in the link w to generate
          newViewDecls += Include(OMMOD(w_to_generate), A.path, Nil, Some(v.toTerm))
        case _ => ???
      }
    }

    // Adopt meta theory from B
    val generated_theory = Theory.empty(D_to_generate.parent, D_to_generate.name, B.meta)
    newDecls.foreach(generated_theory.add(_))

    val generated_view = new View(
      w_to_generate.doc,
      w_to_generate.name,
      TermContainer.asParsed(C.toTerm),
      TermContainer.asParsed(OMMOD(D_to_generate)),
      new TermContainer(),
      isImplicit = false
    )
    newViewDecls.foreach(generated_view.add(_))

    (generated_theory, generated_view)
  }
}
