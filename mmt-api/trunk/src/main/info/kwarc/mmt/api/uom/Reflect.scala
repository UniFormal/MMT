package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._

/* This works but it's unclear how imports and definitions of one parse can be reused in the next one */
/*
class Reflect {
   private val u = scala.reflect.runtime.universe
   private val tbf = new scala.tools.reflect.ToolBoxFactory(u) {
      val mirror = u.runtimeMirror(getClass.getClassLoader)
   }
   private val tb = tbf.mkToolBox()
   import tb._
   
   def parseTerm(s: String): Term = {
      val p = parse(s)
      val check = typecheck(p, pt = tb.u.typeOf[Term])
      val t = eval(p).asInstanceOf[Term]
      t
   }
 
   /** an example for using context bounds and reflection */
   //def make[A: TypeTag](a: Any): A = process(a, implicitly[TypeTag[A]].tpe).asInstanceOf[A]
}*/