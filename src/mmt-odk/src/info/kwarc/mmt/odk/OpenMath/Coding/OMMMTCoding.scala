package info.kwarc.mmt.odk.OpenMath.Coding
import info.kwarc.mmt.odk.OpenMath._

import info.kwarc.mmt.api.objects.Obj

/**
  * Decode / Encode MMT Terms as OpenMath objects.
  */
class OMMMTCoding extends OMCoding[Obj] {
  def encode(om : OMAny) : Obj = ???

  def decodeAnyVal(obj : Obj) : OMAnyVal = ???
  def decode(obj : Obj) : OMAny = ??? // should probably recurse into the above
}