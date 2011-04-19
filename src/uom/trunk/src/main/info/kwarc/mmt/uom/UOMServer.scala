package info.kwarc.mmt.uom

import java.net._
import java.io._
import java.util.jar._
import info.kwarc.mmt.api.objects._

object UOMServer {

  implicit def reflector(ref: AnyRef) = new {
    def getV(name: String): Any = ref.getClass.getMethods
      .find(_.getName == name).get.invoke(ref)
  }


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
            val instance = classToLoad.newInstance
            classToLoad.getDeclaredMethods.map(method => { 
              if (method.getReturnType.getName.equals(
                "info.kwarc.mmt.uom.Implementation")) {
                println(method.getName)
                println("Invoking")
                val invokeResult = method.invoke(instance)
                println("Invoke OK ZOMG!!!")
                invokeResult match {
                  case impl : Implementation => { 
                    println("Pattern match OK")
                    println(impl.name)
                    val ex = new org.omdoc.cds.unsorted.uom.omdoc.lists
                    println(impl.apply(OMA(ex.cons, ex.elem::ex.nil::Nil), 
                      OMA(ex.cons, ex.elem::ex.nil::Nil)))
                  }
                  case _ => {
                    System.err.println("Wrong return type of method")
                    System.exit(1)
                  } 
                }
              }
            })
//              val constructors = classToLoad.getConstructors
//                println("Total of " + constructors.length)
//                constructors.map(x => {
//                x.newInstance()
//                println("Created a new instance\n")
//              })
//            if (classToLoad.getConstructors.length != 0)
//           {
//            println("Print methods")
//            classToLoad.getMethods.map(x=>println(x))
//            println("\nPrint fields")
//            classToLoad.getFields.map(x=>println(x))
//          }

//            classToLoad.getDeclaredFields.map ((field) => {
              //println(field.getType.getName)
              //if (field.getType.getName == 
              //    "info.kwarc.mmt.uom.Implementation") {

                //println("Found implementation\n\nfield = "+field+"\n\n")
                //val obj = field.getType
                //val method = obj.getDeclaredMethod("apply", 
                //  Class.forName("scala.collection.Seq"))

//                println("Tashaka e : " + classToLoad.getSimpleName)
//                val kurInstance = classToLoad.newInstance
//                println("Instantiated that bitch\n")
                //classToLoad.getFields.map(x => println(x))
//                val kurMethod = classToLoad.getDeclaredMethod("apply", 
//                  Class.forName("scala.collection.Seq"))

//                val result = kurMethod.invoke(kurInstance, 
//                  org.omdoc.cds.unsorted.uom.omdoc.lists.cons::
//                  org.omdoc.cds.unsorted.uom.omdoc.lists.list::Nil
//                )

//                if (result == null)
//                  println("result is null")
//                else
//                  println(result)

                //println(method.toString)
                //val instance = obj.newInstance
                //val result = method.invoke(instance, 
                //  org.omdoc.cds.unsorted.uom.omdoc.lists.list,
                //  org.omdoc.cds.unsorted.uom.omdoc.lists.nil
                //)
               //if (result == nill 
             // }
//           }
//            )

//            val method = classToLoad.getDeclaredMethod("test", 
//            Class.forName("java.lang.String"))

//            val instance  = classToLoad.newInstance
//            val result = method.invoke(instance, className)
//            println(result)
          }
          catch {
            case e : IllegalAccessException => println("\n\n"+e.toString+"\n\n")
            case e : NoSuchMethodException =>
              println("Skipping " + e.toString +"\n")
            case e : InstantiationException => println("\n\n" + e.toString + "\n\n")
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
