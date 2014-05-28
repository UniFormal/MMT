package main.scala.info.kwarc.mmt.api.webedit

class MMTResolveIncludesResponse(includes: List[(String,Any)]) {
	def getResponse() = includes
}