package info.kwarc.mmt.mizar.mizar.objects


class MizSchemeBlock(val schemes : List[MizSchemeDef]) extends MizAny

class MizSchemeDef(val args : List[MizSchemeArg], val premises : List[MizProposition], prop : MizProposition) extends MizAny


trait MizSchemeArg 

class MizSchemeFunc(val argTypes : List[MizTyp], retType : MizTyp) extends MizSchemeArg

class MizSchemePred(val argTypes : List[MizTyp]) extends MizSchemeArg

trait MizClusterDef extends MizAny {
	val nr = 0
	def setNr(n : Int) = nr
}

class MizRCluster(val typ : MizTyp, val cluster : MizCluster) extends MizClusterDef
class MizFCluster(val functor : MizTerm, val args : List[MizTyp], val cluster : MizCluster) extends MizClusterDef
class MizCCluster(val typ : MizTyp, val first : MizCluster, val second : MizCluster) extends MizClusterDef

class MizRegistration(val cluster : MizClusterDef) extends MizAny

class XMLRegistrationBlock(regs: List[MizRegistration])
