package info.kwarc.mmt.api.utils
import java.io._


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
   /** the list of file/directory/volume label names making up this file path */ 
   def segments: List[String] = {
      val name = toJava.getName
      val par = toJava.getParentFile
      if (par == null)
        List(name)
      else
        File(par).segments ::: List(name)
   }
   override def toString = toJava.toString
   def getExtension : Option[String] = {
       val name = toJava.getName
       val posOfDot = name.lastIndexOf(".")
       if (posOfDot == -1) None else Some(name.substring(posOfDot + 1))
   }
   def setExtension(ext: String) : File = getExtension match {
       case None => File(toString + "." + ext)
       case Some(s) => File(toString.substring(0, toString.length - s.length) + ext)
   }
}

/** Auxiliary methods to manage files */
object File {
   /** constructs a File from a string, using the java.io.File parser */  
   def apply(s: String) : File = File(new java.io.File(s))
   
   def Writer(f: File) = new PrintWriter(new OutputStreamWriter(new FileOutputStream(f.toJava), java.nio.charset.Charset.forName("UTF-8")))
   def Reader(f: File) = new BufferedReader(new InputStreamReader(new FileInputStream(f.toJava), java.nio.charset.Charset.forName("UTF-8")))
   def ReadLineWise(f: File)(proc: String => Unit) {
      val r = Reader(f)
      var line : String = null
      while ({line = r.readLine; line != null}) {
         try {
            proc(line)
         } catch {
            case e => r.close; throw e 
         }
      }
      r.close
   }
}

/** implicit conversions between File and java.io.File */ 
object FileConversion {
   implicit def scala2Java(file: File) : java.io.File = file.toJava
   implicit def java2Scala(file: java.io.File) : File = File(file)
}