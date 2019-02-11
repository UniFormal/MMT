package info.kwarc.mmt.coq

package object coqxml {
  def makeParser = new info.kwarc.mmt.api.utils.XMLToScala("info.kwarc.mmt.coq.syntax")
}