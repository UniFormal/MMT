package info.kwarc.mmt.mizar.mizarobjects

case class Scheme(name: MizarGlobalName, schemeVariables:List[Definition], schemeSentence:MizarType, schemePremises: List[MizarStatement], proof:MizarProof) extends MizarDeclaration {

}
