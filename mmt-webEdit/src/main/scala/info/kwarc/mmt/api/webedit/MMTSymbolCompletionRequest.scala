package main.scala.info.kwarc.mmt.api.webedit

class MMTSymbolCompletionRequest(symName:String, mpath : String) {

  def getSymName() = symName
  def getMPath () = mpath
}