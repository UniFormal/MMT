package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import symbols._
import objects._
import uom._

/** A Realization is a special case of a DeclaredTheory constructed
 *  to make an unknown semantic entity (e.g., a model or an implementation) partially accessible to the syntax.
 *  
 *  It typically contains [[symbols.RealizedConstant]]s.
 */
class Realization(doc : DPath, name : LocalName, theory: MPath) extends DeclaredTheory(doc, name, None) {
   add(PlainInclude(theory, path))
}

object Realization {
   /** 
    *  @param mp the URI of the Realization to be created
    */
   def fromScala(mp: MPath): Option[Realization] = {
      val s = GenericScalaExporter.mpathToScala(mp)
      val c = try {Class.forName(s + "$")}
         catch {
            case _: java.lang.ClassNotFoundException | _: java.lang.NoClassDefFoundError => return None
            case _: java.lang.ExceptionInInitializerError | _: LinkageError =>
               throw AddError("realization for " + mp + " exists, but an error occurred when accessing it")
         }
      val r = try {c.getField("MODULE$").get(null).asInstanceOf[RealizationInScala]}
              catch {case _ : java.lang.Exception => return None}
      val real = new Realization(mp.parent, mp.name, r._domain._path)
      r._types foreach {rtL =>
         val (synType, rt) = rtL()   
         rt.init(synType, mp)
         val rc = new RealizedTypeConstant(real.toTerm, synType.name, rt)
         real.add(rc)
      }
      r._opers foreach {roL =>
         // add a RealizedConstant for every realization of an operator constant
         val ro = roL()
         val rc = new RealizedOperatorConstant(real.toTerm, ro.op.name, ro)
         real.add(rc)
      }
      Some(real)
   }
}

/** adds all rules of Realization to the RuleStore */
class RealizationListener extends frontend.ChangeListener {
   override val logPrefix = "realization-listener"
   override def onAdd(e: ContentElement) {
       e match {
          case r: Realization =>
             r.getDeclarations.foreach {
                case rc: RealizedOperatorConstant =>
                   log("adding rule for " + rc.path)
                   controller.extman.ruleStore.add(rc.real.toRule(r.path))
                case _ =>
             }
          case _ =>
       }
   }
}