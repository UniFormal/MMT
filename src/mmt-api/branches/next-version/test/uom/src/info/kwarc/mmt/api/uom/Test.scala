package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import objects._

object Test {
  def main (args : Array[String]) {
    
    val controller = new frontend.Controller
    controller.handleLine("file startup.mmt")
    val uom = new UOM(controller.report)

    //args.map(uom.register)

    val arith1 = utils.OpenMath.base ? "arith1"
    val alg1 = utils.OpenMath.base ? "alg1"

    val plus = arith1 ? "plus"
    val uminus = arith1 ? "unary_minus"
    val times = arith1 ? "times"
    val bminus = arith1 ? "minus"
    val zero = alg1 ? "zero"
    val one = alg1 ? "one"
    val Zero = OMS(zero)
    val One = OMS(one)
    
    val rg = new CRing(plus, zero, uminus, times, one)
    uom.register(rg)
    
    val x = OMV("x")
    val y = OMV("y")
    
    val t = plus(plus(One,y,x),One,x,plus(Zero,uminus(One)))
    val p = times(plus(x,y),plus(x,uminus(y)))
    println(uom.simplify(p))
    
    /*
     unit conversion rules
    uom.breadthRules(UnitConvImplems.plus.name) += UnitConvImplems.plus
    uom.breadthRules(UnitConvImplems.mult.name) += UnitConvImplems.mult
    uom.breadthRules(UnitConvImplems.minus.name) += UnitConvImplems.minus
    uom.breadthRules(UnitConvImplems.divide.name) += UnitConvImplems.divide
    */
    
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

