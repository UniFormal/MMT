package info.kwarc.mmt.api.backend
import java.io.File

abstract class Compiler {
   val kind : String
   def check(in: File, targetdir: File)
   def init
   def destroy
}