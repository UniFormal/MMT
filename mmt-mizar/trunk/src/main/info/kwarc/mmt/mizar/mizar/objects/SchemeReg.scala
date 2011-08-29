package info.kwarc.mmt.mizar.mizar.objects


class MizSchemeBlock(val schemes : List[MizSchemeDef]) extends MizAny

class MizSchemeDef(val schemeNr : Int, val args : List[MizSchemeArg], val premises : List[MizProposition],val prop : MizProposition) extends MizAny


trait MizSchemeArg 

class MizSchemeFuncDecl(val argTypes : List[MizTyp], val retType : MizTyp) extends MizSchemeArg

class MizSchemePredDecl(val argTypes : List[MizTyp]) extends MizSchemeArg

trait MizClusterDef extends MizAny

class MizRCluster(val aid : String, val nr : Int, val args : List[MizTyp], val typ : MizTyp, val cluster : MizCluster) extends MizClusterDef
class MizFCluster(val aid : String, val nr : Int, val args : List[MizTyp], val functor : MizTerm, val cluster : MizCluster) extends MizClusterDef
class MizCCluster(val aid : String, val nr : Int, val args : List[MizTyp], val typ : MizTyp, val first : MizCluster, val second : MizCluster) extends MizClusterDef

class MizRegistration(val cluster : MizClusterDef) extends MizAny

class XMLRegistrationBlock(regs: List[MizRegistration])
