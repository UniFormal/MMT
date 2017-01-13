package info.kwarc.mmt.odk.SCSCP.CD

import info.kwarc.mmt.odk.OpenMath.OMSymbol


object logic1 {
  /* The name of the cd itself */
  def apply(name: String) = {
    OMSymbol(name, "scscp1", None, None)
  }

  final val True: String = "true"
  final val False: String = "false"
}
