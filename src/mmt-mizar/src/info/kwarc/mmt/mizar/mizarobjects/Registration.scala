package info.kwarc.mmt.mizar.mizarobjects

case class Cluster(name: MizarGlobalName, attributes:List[MizarGlobalName], inhabitationProof:MizarProof) extends MizarDeclaration {}

case class Registration(name: MizarGlobalName, clusters:Cluster) extends MizarDeclaration {

}
