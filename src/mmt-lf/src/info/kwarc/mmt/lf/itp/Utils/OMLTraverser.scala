package info.kwarc.mmt.lf.itp.Utils

import info.kwarc.mmt.api.objects._

class OMLTraverser extends StatelessTraverser {
  override def traverse(t: Term)(implicit con: Context, state: State): Term = t match{
    case OML(nm , None , None , None , None) => OMV(nm)
    case _ => Traverser(this, t )
  }
}
