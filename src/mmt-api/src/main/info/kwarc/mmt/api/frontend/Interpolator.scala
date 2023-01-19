package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.uom.SimplificationUnit
import objects._
import utils._
import notations._
import parser._
import documents._

import scala.math.BigInt.int2bigInt
import uom.OMLiteral._

/**
 * provides convenience methods to be used in an interactive Scala interpreter session
 *
 * usage: val ip = new MMTInterpolator(controller); import i.MMTContext
 */
class MMTInterpolator(controller: frontend.Controller) {
   implicit def int2OM(i: Int) = OMI(i)
   implicit def floatt2OM(f: Double) = OMF(f)

   /** a shortcut for running MMT shell commands while in the Scala interpreter */
   def shell(command: String): Unit = {
     controller.handleLine(command)
   }

   private def theory = controller.getBase match {
        case d: DPath => utils.mmt.mmtcd
        case p: MPath => p
        case GlobalName(t,_) => t
        case CPath(par,_) => par match {
           case p: MPath => p
           case GlobalName(t,_) => t
           case dp: DPath => mmt.mmtcd
        }
     }
   def parse(ss: Iterable[String], ts: Iterable[Term], top: Option[ParsingRule], check: Boolean) = {
         val strings = ss.iterator
         val args = ts.iterator
         val buf = new StringBuffer(strings.next())
         var cont = Context.empty
         var i = 0
         while(strings.hasNext) {
            val name = LocalName("$_" + i.toString)
            val arg = args.next()
            cont = cont ++ VarDecl(name, None, None, Some(arg), None)
            buf.append(name)
            buf.append(strings.next())
            i += 1
        }
        val str = buf.toString
        val pu = ParsingUnit(SourceRef.anonymous(str), Context(theory) ++ cont, str, InterpretationInstructionContext(NamespaceMap(theory.doc)), top)
        val parser = controller.extman.get(classOf[Parser], "mmt").get
        val t = parser(pu)(ErrorThrower).toTerm
        val tI = t ^? cont.toPartialSubstitution
        if (check) {
           val stack = Stack(Context(theory) ++ cont)
           val (tR, tpR) = checking.Solver.check(controller, stack, tI).
                  left.getOrElse {
               throw InvalidObject(t, "term was parsed but did not type-check")
             }
           tR
        } else
          tI
   }

   /**
    * defines string interpolation methods
    *
    * use them as m"x" where m is the method name and x a string representing an Obj
    * notations are used according to the current theory,
    * integers and floats are turned into MMT objects
    *
    * example: val x = OMI(1); uom"1+$x" yields OMI(2) (if appropriate notation and simplification rule for + are registered)
    */
  implicit class MMTContext(sc: StringContext) {
      val ss = sc.parts
      /** mmt"s" parses s into a Term */
      def mmt(ts: Term*): Term = parse(ss, ts.toList, None, false)
      /** uom"s" parses and simplifies s */
      def uom(ts: Term*): Term = {
         val t = mmt(ts : _*)
         controller.simplifier(t, SimplificationUnit(Context(theory), false, false, true))
      }
      /** r"s" parses and type-checks s */
      def r(ts: Term*): Term = parse(ss, ts.toList, None, true)
      /** s"s" parses, type-checks, and simplifies s */
      def rs(ts: Term*): Term = {
         val t = r(ts : _*)
         controller.simplifier(t, SimplificationUnit(Context(theory), false, false, true))
      }
      /** cont"s" parses s into a Context */
      def cont(ts: Term*) : Context = {
         val t = parse(ss, ts.toList, None, false)
         t match {
            case OMBINDC(_,con, Nil) => con
            case _ => throw ParseError("not a context")
         }
      }
      /** present"s" parses and presents s */
      def present(ts : Term*) : String = {
        val t = parse(ss, ts.toList, None, false)
        controller.presenter.asString(t)
      }
      /** uomp"s" parses, simplifies, and presents s */
      def uomp(ts : Term*) : String = {
        val t = uom(ts : _*)
        controller.presenter.asString(t)
      }
      /** standard string interpolation that is executed immediately as an MMT action */
      def act(ss: String*): Unit = {
         shell(sc.s(ss))
      }
   }
}
