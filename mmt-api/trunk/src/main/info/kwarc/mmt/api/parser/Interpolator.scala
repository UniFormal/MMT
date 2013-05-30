package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import objects._
import utils._

object MMT2ScalaConv {
  implicit def OM2int(tm : Term) : Int = tm match {
    case OMI(i) => i.toInt
    case _ => throw new Exception("Invalid Conversion")
  }
  
  implicit def OM2float(tm : Term) : Double = tm match {
    case OMI(i) => i.toDouble
    case OMF(i) => i
    case _ => throw new Exception("Invalid Conversion")

  }
  
  implicit def OM2List(tm : Term) : Vector[Term] = tm match {
    case OMA(OMS(p), args) 
      if (p == DPath(utils.URI("http://www.openmath.org/cd")) ? "linalg2" ? "vector") => 
        Vector(args :_*)
    case _ => throw new Exception("Invalid Conversion")
  }
}


class MMTInterpolator(controller: frontend.Controller) {
   implicit def int2OM(i: Int) = OMI(i)
   implicit def floatt2OM(f: Double) = OMF(f)
   
   /** a shortcut for running MMT shell commands while in the Scala interpreter */
   def shell(command: String) {
     controller.handleLine(command)
   }
   
   private def parse(sc: StringContext, ts: List[Term], top: Option[TextNotation], check: Boolean) = {
         val strings = sc.parts.iterator
         val args = ts.iterator
         val buf = new StringBuffer(strings.next)
         var cont = Context()
         var i = 0
         while(strings.hasNext) {
            val name = LocalName("$_" + i.toString)
            val arg = args.next
            cont = cont ++ VarDecl(name, None, Some(arg))
            buf.append(name)
            buf.append(strings.next)
            i += 1
        }
        val str = buf.toString
        val theory = controller.getBase match {
           case d: DPath => OMMOD(utils.mmt.mmtcd) 
           case p: MPath => OMMOD(p)
           case GlobalName(t,_) => t
           case CPath(par,_) => par match {
              case p: MPath => OMMOD(p)
              case GlobalName(t,_) => t
           }
        }
        val pu = ParsingUnit(SourceRef.anonymous(str), theory, cont, str, top) 
        val t = controller.termParser(pu)
        val tI = t ^ cont.toPartialSubstitution
        if (check) {
	        val stack = Stack(theory, cont)
	        val (tR, tpR) = Solver.check(controller, stack, tI).getOrElse {
	           throw InvalidObject(t, "term was parsed but did not type-check")
	        }
	        tR
        } else
          tI
   }
   
   implicit class MMTContext(sc: StringContext) {
      def mmt(ts: Term*): Term = parse(sc, ts.toList, None, false)
      def uom(ts: Term*): Term = {
         val t = mmt(ts : _*)
	     controller.uom.simplify(t)
      }
      def r(ts: Term*): Term = parse(sc, ts.toList, None, true)
      def rs(ts: Term*): Term = {
         val t = r(ts : _*)
	     controller.uom.simplify(t)
      }
      def cont(ts: Term*) : Context = {
         val t = parse(sc, ts.toList, Some(TextNotation.contextNotation), false)
         t match {
            case OMBINDC(_,con, Nil) => con
            case _ => throw ParseError("not a context")
         }
      }
   }
}