package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.objects.{Context, OMID, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.Simplifiability.NoRecurse
import info.kwarc.mmt.api.uom.{Simplifiability, SimplificationRule, Simplify}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.Utils
import info.kwarc.mmt.frameit.business.datastructures.MetadataUtils

import scala.util.Try

class LabelVerbalizationRule()(implicit lookup: Lookup) extends SimplificationRule(MetaAnnotations.LabelVerbalization.path) {
  override def apply(context: Context, t: Term): Simplifiability = t match {
    case MetaAnnotations.LabelVerbalization(args) =>
      val verbalizedArgs = args.map {
        case OMID(path) =>
          Utils
            .getAsO(classOf[Constant], path)
            .flatMap(c => Try(
              MetadataUtils.readTermMetaDatum(c.metadata, MetaKeys.label)
            ).toOption)
            .getOrElse(StringLiterals(
              s"could not label-verbalize symbol reference to `${path}`")
            )

        case term =>
          StringLiterals(s"cannot verbalize complex terms like `${term.toStr(true)}` yet")
      }

      Simplify(
        verbalizedArgs.reduceLeft(MitM.Foundation.StringConcat.apply)
      )

    case _ =>
      NoRecurse
  }
}
