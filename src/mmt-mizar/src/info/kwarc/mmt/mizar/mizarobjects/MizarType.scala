package mmt-mizar.src.info.kwarc.mmt.mizar.mizarobjects

/**
 * Class for handling Mizar Types
 */
class MizarType(val aid : String, val kind : String, val absnr: Int, val vid : String, val clusters : List[MizCluster], val terms: List[MizTerm], var name : Option[String]) {
  def setName(n : Option[String]) {
    name = n
  }
  def ==(that : MizTyp) = this.aid == that.aid &&
    this.kind == that.kind &&
    this.absnr == that.absnr &&
    this.vid == that.vid &&
    this.clusters.length == that.clusters.length &&
    this.terms.length == that.terms.length

  def !=(that : MizTyp) = !(==(that))
}

