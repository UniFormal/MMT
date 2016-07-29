package info.kwarc.mmt.metamath

import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.{NamespaceMap, Path}

object MetaMath {
   val _base = Path.parseD("http://us.metamath.org",NamespaceMap.empty)
   val theory = _base ? "Metamath"

   val prop = OMS(theory ? "prop")
}