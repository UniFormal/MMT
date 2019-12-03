package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.OMLReplacer
import info.kwarc.mmt.api.utils.SkipThis

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
        throw GeneralError(s"Cannot compute pushout due to name clash of ${decl.name} in anonymous theory with equally called declaration in anonymous theory")
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
