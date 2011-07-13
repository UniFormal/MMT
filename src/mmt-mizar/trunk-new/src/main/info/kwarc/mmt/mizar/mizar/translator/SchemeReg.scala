package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api.objects._

object SchemeRegTranslator {
	
	//Registration
	def translateRegistration(reg : MizRegistration) {
		reg.cluster match {
			case rc : MizRCluster => MMTRCluster(rc.nr, TypeTranslator.translateTyp(rc.typ), TypeTranslator.translateCluster(rc.cluster))
			case fc : MizFCluster => MMTFCluster(fc.nr, TypeTranslator.translateTerm(fc.functor), fc.args.map(TypeTranslator.translateTyp).toList, TypeTranslator.translateCluster(fc.cluster))
			case cc : MizCCluster => MMTCCluster(cc.nr, TypeTranslator.translateTyp(cc.typ),TypeTranslator.translateCluster(cc.first),TypeTranslator.translateCluster(cc.second))
		}
	}
}