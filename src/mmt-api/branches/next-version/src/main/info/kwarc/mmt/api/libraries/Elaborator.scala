package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import patterns._
import objects._
import utils.MyList.fromList
//import objects.Conversions._

/** an Elaborator takes a ContentElement and produces further ContentElements that are the result of elaborating the former */
abstract class Elaborator {
   def apply(e: StructuralElement)(implicit cont: StructuralElement => Unit) : Unit
}

/*
/** elaborates meta-theories and includes into theories in such a way that the transitive closure of include is taken
 *  if this is used, all includes are guaranteed to be direct
 *  this retrieves all included theories recursively
 *  @param controller a controller for lookups, no changes are made to the controller
 */
class IncludeElaborator(controller: Controller) extends Elaborator {
   private val content = controller.globalLookup
   /** 
    * @param e the ContentElement to elaborate
    * @param cont a continuation function to call on every generated StructuralElement (e.g., Controller.add)
    */
   def apply(e: StructuralElement)(implicit cont: StructuralElement => Unit) : Unit = e match {
     case t: DeclaredTheory => if (t.meta.isDefined) {
        val mt = t.meta.get
        val mincls = OMMOD(mt) :: content.importsTo(OMMOD(mt)).toList  
        val minclsflat = mincls map {from =>
           val i = Include(OMMOD(t.path), from)
           i.setOrigin(MetaInclude)
           i
        }
        minclsflat map cont
     }
     case incl: Structure if incl.name.isAnonymous =>
        if (! content.imports(incl.from, incl.home)) {
           // flattening (transitive closure) of includes, ignoring redundant imports
           val flat = content.importsTo(incl.from).toList.mapPartial {t =>
              if (content.imports(t, incl.home)) None
              else {
                 val i = Include(incl.home, t)
                 i.setOrigin(IncludeClosure)
                 Some(i)
              }
           }
           flat map cont
        }
     case a: DefLinkAssignment if a.name.isAnonymous =>
        // flattening of includes: this assignment also applies to every theory t included into s.from 
        val (l,d) = content.getSource(a)
        content.importsTo(a.from) foreach {t =>
          //here, the compatibility check can be added
           val r = new DefLinkAssignment(a.home, a.name, t, a.target)
           r.setOrigin(IncludeClosure)
           cont(r)
        }
     case _ =>
   }
}
*/
