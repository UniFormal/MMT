package main.scala.info.kwarc.mmt.api.webedit

class MMTSymbolCompletionRequest(prefix:String, mpath : String) {

  def getPrefix() = prefix
  def getMPath () = mpath
}