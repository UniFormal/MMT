package info.kwarc.mmt.odk.OpenMath.Coding
import info.kwarc.mmt.api.{MPath, NamespaceMap, Path}
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.api.objects._

/**
  * Decode / Encode MMT Terms as OpenMath objects.
  */
class OMMMTCoding(default : MPath = Path.parseM("http://cds.omdoc.org/?Default",NamespaceMap.empty)) extends OMCoding[Term] {
  def encode(om : OMAny) : Term = om match {
    case OMObject(expr,_,_,_) => encode(expr)
  }

  def decodeAnyVal(t : Term) : OMAnyVal = ???
  def decode(t : Term) : OMAny = ??? // should probably recurse into the above
}