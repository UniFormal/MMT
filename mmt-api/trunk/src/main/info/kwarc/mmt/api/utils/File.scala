package info.kwarc.mmt.api.utils

/** File wraps around java.io.File to extend it with convenience methods */
case class File(toJava: java.io.File) {
   /** resolves an absolute or relative path string against this */
   def resolve(s: String) = {
      val sf = new java.io.File(s)
      val newfile = if (sf.isAbsolute)
         sf
      else
         new java.io.File(toJava, s)
      File(newfile)
   }
   /** appends one path segment */
   def /(s:String) : File = File(new java.io.File(toJava, s))
   /** appends a list of path segments */
   def /(ss:List[String]) : File = ss.foldLeft(this) {case (sofar,next) => sofar / next}
   override def toString = toJava.toString
}

object File {
   /** constructs a File from a string, using the java.io.File parser */  
   def apply(s: String) : File = File(new java.io.File(s))
}

/** implicit conversions between File and java.io.File */ 
object FileConversion {
   implicit def scala2Java(file: File) : java.io.File = file.toJava
   implicit def java2Scala(file: java.io.File) : File = File(file)
}