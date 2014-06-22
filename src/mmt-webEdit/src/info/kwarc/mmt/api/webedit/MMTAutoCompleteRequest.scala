package info.kwarc.mmt.api.webedit
import info.kwarc.mmt.api._

class MMTAutoCompleteRequest(prefix:String, mpath:String){
	def getPrefix() = prefix
	def getMPath() = mpath
}