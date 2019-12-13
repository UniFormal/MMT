/**
  * Various paths and helper objects for MMT theories in the MMT/LATIN2 archive.
  */

package info.kwarc.mmt.moduleexpressions.operators.typeops

import info.kwarc.mmt.api.uom.TheoryScala
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, LocalName}
import info.kwarc.mmt.lf._

object LATIN2Environment {
  trait LatinTheoryScala extends TheoryScala {
    // TODO Actually it should be `latin:/`, but that has strange bugs due to the strange URI
    final val _base = DPath(URI("https://example.com/diagops") / "typifier")
  }

  object Logic {
    /**
      * The Terms theory from the `MMT/LATIN2` archive
      */
    object Terms extends LatinTheoryScala {
      val _name = LocalName("Terms")

      object term extends NullaryLFConstantScala(_path, "term")
    }

    object Types extends LatinTheoryScala {
      val _name = LocalName("Types")

      object tp extends NullaryLFConstantScala(_path, "tp")
    }

    object TypedTerms extends LatinTheoryScala {
      val _name = LocalName("TypedTerms")

      object tm extends UnaryLFConstantScala(_path, "tm")
    }
  }
}