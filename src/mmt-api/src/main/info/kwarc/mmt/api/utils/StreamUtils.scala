package info.kwarc.mmt.api.utils

import java.io.{BufferedReader, InputStream, InputStreamReader}

object StreamUtils {
   /** turns an inputStream into a string */
   def toString(stream : InputStream, encoding: String) : String = {
      val builder = new StringBuilder
      val reader = new BufferedReader(new InputStreamReader(stream, encoding))

      var line = reader.readLine()
      while(line != null){
         builder.append(line)
         builder.append('\n')
         line = reader.readLine()
      }

      builder.toString()
   }
}

