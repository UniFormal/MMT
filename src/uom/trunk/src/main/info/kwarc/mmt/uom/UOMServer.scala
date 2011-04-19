package info.kwarc.mmt.uom

import java.net._
import java.io._
import java.util.jar._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import scala.collection.mutable.HashMap

object UOMServer {

  val impls = new HashMap[GlobalName, Implementation]

  def register(jarFileName : String) {
    val urlArray = new Array[URL](1)
    val jarFile = new File(jarFileName)
    urlArray(0) = jarFile.toURI.toURL 
    val child = new URLClassLoader(urlArray, this.getClass.getClassLoader)

    val packageName = ""
    //val packageName = "info.kwarc.mmt.uom"
    //packageName.replaceAll("\\." , "/")
    //println(packageName)
    val jarInput = new JarInputStream(new FileInputStream(jarFileName))
    val classImplementation = Class.forName("info.kwarc.mmt.uom.Implementation")
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
        

        val classToLoad = Class.forName(className, true, child)
        classToLoad.getDeclaredMethods.map(method => { 
          if (method.getReturnType.getName.equals(
              "info.kwarc.mmt.uom.Implementation")) {
            val instance = classToLoad.newInstance
            val invokeResult = method.invoke(instance)
            invokeResult match {
              case impl : Implementation => { 
                impls(impl.name) = impl
              }
              case _ => {
                System.err.println("Wrong return type of method")
                System.exit(1)
              } 
            }
          }
        })
      }
    }
  }

  def main (args : Array[String]) {
    register(args(0))

    /* Test with some example terms */
    val base = DPath(new utils.xml.URI("http://cds.omdoc.org/unsorted/uom.omdoc"))
    val gname1 = GlobalName(OMMOD(MPath(base, LocalPath(List(
          "lists")))), LocalName(List(NamedStep("append_*"))))
    val gname2 = GlobalName(OMMOD(MPath(base, LocalPath(List(
          "lists_ext")))), LocalName(List(NamedStep("append_many_*"))))

    val ex = new org.omdoc.cds.unsorted.uom.omdoc.lists

    println("First example\n\n")
    println(impls(gname1).apply(OMA(ex.cons, ex.elem::ex.nil::Nil), 
      OMA(ex.cons, ex.elem::ex.nil::Nil)))

    println("\n\nSecond example\n\n")
    println(impls(gname2).apply(
      OMA(ex.cons, ex.elem::ex.nil::Nil), 
      OMA(ex.cons, ex.elem::ex.nil::Nil),
      OMA(ex.cons, ex.elem::ex.nil::Nil)
      ))
  }
}



