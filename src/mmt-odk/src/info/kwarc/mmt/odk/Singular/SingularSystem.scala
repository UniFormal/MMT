package info.kwarc.mmt.odk.Singular

import info.kwarc.mmt.MitM.MitMSystems
import info.kwarc.mmt.MitM.VRESystem.VREWithAlignmentAndSCSCP
import info.kwarc.mmt.api.{GlobalName, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.refactoring.TranslationTarget
import info.kwarc.mmt.odk.OpenMath.OMSymbol

class SingularSystem extends VREWithAlignmentAndSCSCP("Singular",MitMSystems.singularsym, MitMSystems.evaluateSym, "ODK/Singular") {
  val namespace = Path.parse("https://www.singular.uni-kl.de")
  override protected lazy val translator_to = translator(new TranslationTarget {
    override def inTarget(path: GlobalName, controller: Controller): Boolean = namespace <= path
  },toTranslations)
}
