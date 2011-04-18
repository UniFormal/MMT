package info.kwarc.mmt.uom

import java.net._
import java.io._
import java.util.jar._
import info.kwarc.mmt.api.objects._

object UOMServer {
  def main(args :Array[String]) {
    try {
      val urlArray = new Array[URL](1)
      val jarFile = new File(args(0))
      urlArray(0) = jarFile.toURI.toURL 
      val child = new URLClassLoader(urlArray, this.getClass.getClassLoader)

  //  child.getURLs.toList.foreach((e)=>println(e))
      
      val packageName = ""
      //val packageName = "info.kwarc.mmt.uom"
      //packageName.replaceAll("\\." , "/")
      //println(packageName)
      val jarInput = new JarInputStream(new FileInputStream(args(0)))

      while(true) {
        val jarEntry = jarInput.getNextJarEntry
        if (jarEntry == null) {
          return
        }
        if ( jarEntry.getName.startsWith(packageName) && 
          (jarEntry.getName.endsWith(".class"))) {
          val className = jarEntry.getName()
            .replaceAll("/", "\\.")
            .replaceAll(".class","")
          println("Handling " + className)

          
          try {
            val classToLoad = Class.forName(className, true, child)
            classToLoad.getDeclaredFields.map ((field) => {
              //println(field.getType.getName)
              //if (field.getType.getName == 
              //    "info.kwarc.mmt.uom.Implementation") {

                //println("Found implementation\n\nfield = "+field+"\n\n")
                //val obj = field.getType
                //val method = obj.getDeclaredMethod("apply", 
                //  Class.forName("scala.collection.Seq"))

                println("Tashaka e : " + classToLoad.getName)
                val kurInstance = classToLoad.newInstance
                println("Instantiated that bitch")
                //println(classToLoad.getDeclaredMethods.map(x => println(x)))
                val kurMethod = classToLoad.getDeclaredMethod("apply", 
                  Class.forName("scala.collection.Seq"))
                val result = kurMethod.invoke(kurInstance, 
                  org.omdoc.cds.unsorted.uom.omdoc.lists.list::
                  org.omdoc.cds.unsorted.uom.omdoc.lists.list::Nil
                )

                if (result == null)
                  println("result is null")
                else
                  println(result)

                //println(method.toString)
                //val instance = obj.newInstance
                //val result = method.invoke(instance, 
                //  org.omdoc.cds.unsorted.uom.omdoc.lists.list,
                //  org.omdoc.cds.unsorted.uom.omdoc.lists.nil
                //)
               //if (result == nill 
             // }
           }
            )
//            val method = classToLoad.getDeclaredMethod("test", 
//            Class.forName("java.lang.String"))

//            val instance  = classToLoad.newInstance
//            val result = method.invoke(instance, className)
//            println(result)
          }
          catch {
            case e : IllegalAccessException => println("\n\n"+e.toString+"\n\n")
            case e : NoSuchMethodException =>
              // println("Skipping " + e.toString +"\n")
          }
        }
      }
    }
    catch {
      case e : Throwable => System.err.println("UOMServer: " + e.toString())
      System.exit(1)
    }
  }
}
