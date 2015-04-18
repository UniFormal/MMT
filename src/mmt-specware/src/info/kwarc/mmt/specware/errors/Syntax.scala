package info.kwarc.mmt.specware.errors

import info.kwarc.mmt.api._

case class errors(_children: List[error])

case class error(shortMsg: String, sref: String) {
   def getSourceRef = parser.SourceRef.fromURI(utils.URI(sref))   
}