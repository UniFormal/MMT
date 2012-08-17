package info.kwarc.mmt.api.frontend

abstract class Plugin {
   val dependencies : List[String]
   def init(c: Controller, args: List[String])
}