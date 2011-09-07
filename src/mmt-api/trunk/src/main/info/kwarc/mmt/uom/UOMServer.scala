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
    impls(UnitConvImplems.minus.name) = UnitConvImplems.minus
    impls(UnitConvImplems.divide.name) = UnitConvImplems.divide
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
     case OMA(OMID(p), args:List[Term]) =>
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
    
    val uom = new UOMServer(new frontend.FileReport(new java.io.File("uom.log")))
    args.map(uom.register)
    
    // Do some tests
    /*
    val base = DPath(utils.URI("http", "www.openmath.org") / "cd")

    val constrs = new org.openmath.www.cd.constructors
    val arith1 = new org.openmath.www.cd.arith1
    
    val r1 = OMA(arith1.plus, OMF(3) :: OMF(2) :: OMF(7) :: Nil )
    println("12 = " + uom.simplify(r1))

    val r2 = OMA(arith1.minus, OMF(3) :: OMF(2) :: Nil )
    println("1 = " + uom.simplify(r2))

    val r3 = OMA(arith1.plus, (OMA(arith1.unary_minus, OMF(3)::Nil)):: OMF(2) :: Nil )
    println("-1 = " + uom.simplify(r3))

    val r4 = OMA(arith1.times, OMF(3) :: OMF(2) :: OMF(4) :: Nil )
    println("24 = " + uom.simplify(r4))

    val r5 = OMA(arith1.divide, OMF(3) :: OMF(2) :: Nil )
    println("1.5 = " + uom.simplify(r5))

    val r6 = OMA(arith1.power, OMF(2) :: OMF(3) :: Nil )
    println("8 = " + uom.simplify(r6))

    // abs(unary_minus(3)) TIMES 2 TIMES 6 
    val r7 = OMA(arith1.times, (OMA(arith1.abs, OMA(arith1.unary_minus, OMF(3)::Nil)::Nil)) 
        :: OMF(2) :: OMF(5) ::Nil )
    println("30 = " + uom.simplify(r7))

    // square_root(r7+6)
    val r8 = OMA(arith1.root, (OMA(arith1.plus, r7::OMF(6)::Nil))::OMF(2)::Nil)
    println("6 = " + uom.simplify(r8))

    // gcd(30, 6, 4) 
    val r9 = OMA(arith1.gcd, OMI(30)::OMI(24)::OMI(4)::Nil)
    println("2 = " + uom.simplify(r9))

    // lcm(2, 5, 10, 13) 
    val r10 = OMA(arith1.lcm, r9::OMI(13)::OMI(10)::OMI(5)::Nil)
    println("130 = " + uom.simplify(r10))
    */

/*  
    // Test cases for thesis example
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

