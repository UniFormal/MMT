package info.kwarc.mmt.uom

import java.net._
import java.io._
import java.util.jar._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.uom._
import scala.collection.mutable.{HashMap,HashSet}

class UOMServer(report: frontend.Report) {
  val impls = new HashMap[GlobalName, Implementation]
  
  def rules = new HashMap[(GlobalName,GlobalName), DepthRule]

  def log(msg: => String) {report("uom", msg)}
  def init {
    // preload Unit conversion implementations
    impls(UnitConvImplems.plus.name) = UnitConvImplems.plus
    impls(UnitConvImplems.mult.name) = UnitConvImplems.mult
  }

  def register(jarFileName : String) {
    val jarFile = new File(jarFileName)
    
    val urlArray = new Array[URL](1)
    urlArray(0) = jarFile.toURI.toURL 
    val child = new URLClassLoader(urlArray, this.getClass.getClassLoader)

    val jarInput = new JarInputStream(new FileInputStream(jarFileName))
    
    var jarEntry : JarEntry = null // a jar entry is a resource/file in the jar 
    while ({jarEntry = jarInput.getNextJarEntry; jarEntry != null}) {
      if (jarEntry.getName.endsWith(".class")) {
        val className = jarEntry.getName()
          .replaceAll("/", "\\.")
          .replaceAll(".class","")
        
        val classToLoad = Class.forName(className, true, child) // returns an object reflecting the class
        // call each method in instance that returns an Implementation (we really only care about the Scala-generated getter methods of Implementation-fields)
        // and register the result in impls
        classToLoad.getDeclaredMethods map {method => 
          if (method.getReturnType.getName == "info.kwarc.mmt.uom.Implementation") {

            val instance = classToLoad.newInstance  // do not move above for now
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
        }
      }
    }
  }
  
  def simplify(term : Term) : Term = term match {
     case OMA(OMID(p), args) =>
        log("simplifying " + term.toString)
        val recargs = args.map(simplify)
        impls.get(p) match {
           case Some(impl) => impl(recargs :_*)
           case None => OMA(OMID(p), recargs)
        }
        
     case _ => term
  }
}

object Test {
  def main (args : Array[String]) {
    /*
    val uom = new UOMServer(new frontend.FileReport(new java.io.File("uom.log")))
    args.map(uom.register)
    
    // Test with some example terms 
    val base = DPath(new utils.xml.URI("http://cds.omdoc.org/unsorted/uom.omdoc"))
    val gname1 = OMMOD(base ? "lists")  % LocalName("append_*")
    val gname2 = OMMOD(base ? "lists_ext") % LocalName("append_many_*")

    val ex  = new org.omdoc.cds.unsorted.uom.omdoc.lists
    val ext = new org.omdoc.cds.unsorted.uom.omdoc.lists_ext

    // append( [el], [el]) 
    val c1 = OMA(ex.append, OMA(ex.cons, ex.elem :: ex.nil::Nil) :: 
        OMA(ex.cons, ex.elem::ex.nil::Nil) ::Nil)
    println("XML BELOW\n\n" + c1.toNode)
    val t1 = (uom.simplify(c1))
    println(t1+"\n\n")

    // append_many([el], [el], [el]) 
    val c2 =(OMA(ext.append_many, List(
      OMA(ex.cons, ex.elem::ex.nil::Nil), 
      OMA(ex.cons, ex.elem::ex.nil::Nil),
      OMA(ex.cons, ex.elem::ex.nil::Nil)
      )))
    val t2 = uom.simplify(c2)
    println(t2+"\n\n")

    // append_many(c2)  
    val c2_equal = OMA(ext.append_many, c2::Nil)
    if (uom.simplify(c2_equal).equals(t2)) 
      println("They are the same as expceted\n\n")

    // append(c1, c2)  
    val c3 = OMA(ex.append, c1::c2::Nil)
    println(uom.simplify(c3)+"\n\n")

    // very composite, result should be a list with 10 el 
    val c4 = OMA(ext.append_many, c3::c2_equal::c1::Nil)
    println(uom.simplify(c4))
    */
  }
}

