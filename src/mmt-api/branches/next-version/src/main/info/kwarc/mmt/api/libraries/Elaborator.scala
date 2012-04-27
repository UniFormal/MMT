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
   def apply(e: ContentElement) : Unit
}

/** elaborates meta-theories and includes into theories in such a way that the transitive closure of include is taken
 *  if this is used, all includes are guaranteed to be direct
 *  this retrieves all included theories recursively
 */
class ElaborateIncludes(controller: Controller) extends Elaborator {
   private val content = controller.memory.content
   private def add(c: ContentElement) {content.add(c)}
   def apply(e: ContentElement) : Unit = e match {
     case t: DeclaredTheory => if (t.meta.isDefined) {
        val mt = t.meta.get
        val mincls = OMMOD(mt) :: content.importsTo(OMMOD(mt)).toList  
        val minclsflat = mincls map {from =>
           val i = new Include(OMMOD(t.path), from)
           i.setOrigin(MetaInclude)
           i
        }
        minclsflat map add
     }
     case incl: Include =>
        if (! content.imports(incl.from, incl.home)) {
           // flattening (transitive closure) of includes, ignoring redundant imports
           val flat = content.importsTo(incl.from).toList.mapPartial {t =>
              if (content.imports(t, incl.home)) None
              else {
                 val i = new Include(incl.home, t)
                 i.setOrigin(IncludeClosure)
                 Some(i)
              }
           }
           flat map add
        }
     case a: DefLinkAssignment =>
        // flattening of includes: this assignment also applies to every theory t included into s.from 
        val (l,d) = content.getSource(a)
        val dl = d match {
           case dl : DefinitionalLink => dl
           case _ => throw ImplementationError("deflink-assignment to non-deflink")
        }
        val flat = content.importsTo(dl.from).toList.mapPartial {t =>
           val name = a.name.thenInclude(t)
           if (l.declares(name)) None //here, the compatibility check can be added
           else {
              val r = new DefLinkAssignment(a.home, name, a.target)
              r.setOrigin(IncludeClosure)
              Some(r)
           }
        }
     case _ =>
   }
}

/** elaborates Instance declarations
 * this is also called the pragmatic-to-strict translation
 */
class ElaborateInstances extends Elaborator {
   def apply(e: ContentElement) : Unit = e match {
     case i: Instance =>
     case _ =>
   }
}