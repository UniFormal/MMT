package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import utils.File

abstract class Compiler {
   val kind : String
   def check(in: File, targetdir: File)
   def init
   def destroy
}