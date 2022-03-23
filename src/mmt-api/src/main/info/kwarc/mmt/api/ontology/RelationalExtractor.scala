package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import patterns._
import objects._
import frontend._
import opaque._

/** interface of any class that can be used by the [[RelationalManager]] to build the Abox from MMT content. */ 
abstract class RelationalExtractor extends Extension {
  /** all unary relations that this extractor can generate (extract) */
  def allUnary : List[Unary]

  /** all binary relations that this extractor can generate (extract) */
  def allBinary : List[Binary]

  /** apply a continuation function to every relational element of a StructuralElement */
  def apply(e : StructuralElement)(implicit f: RelationalElement => Unit) : Unit

}

/** The Extractor produces the declaration-level relational representation of a StructuralElement
 */
object MMTExtractor extends RelationalExtractor {
   val allUnary = List(IsDocument,ontology.IsTheory,IsView,IsConstant,IsStructure,IsConAss,
                          IsStrAss,IsNotation,IsDerivedDeclaration,IsPattern,IsInstance)
   val allBinary = List(RefersTo,DependsOn,Includes,IsAliasFor,IsInstanceOf,HasMeta,HasDomain,HasCodomain,Declares,
         IsAlignedWith, HasViewFrom, IsImplicitly)


   /** apply a continuation function to every relational element of a StructuralElement */
   def apply(e: StructuralElement)(implicit f: RelationalElement => Unit) {
      val path = e.path
      e match {
         case d: Document =>
            f(IsDocument(d.path))
            //TODO should use getLocalItems but then it wouldn't work for documents created from folders
            d.getDeclarations.foreach {i =>
               val cO = i match {
                  case inner: Document =>
                     apply(inner)
                     Some(inner.path)
                  case oe: OpaqueElement =>
                    None
                  case nr: NRef =>
                    Some(nr.target)
                  case ii: InterpretationInstruction =>
                    None
               }
               cO foreach {c => f(Declares(d.path, c))}
            }
         case n: NRef =>
         case oe: OpaqueElement =>
         case t: Theory =>
            f(ontology.IsTheory(path))
            t match {
               case t: Theory =>
                  t.meta foreach {p => f(HasMeta(path, p))}
               case _ =>
            }
         case v: View =>
            f(IsView(path))
            v.domainAsContext.getIncludes.foreach {p => f(HasDomain(path, p))}
            v.codomainAsContext.getIncludes.foreach {p => f(HasCodomain(path, p))}
            v.to match {
              case OMPMOD(to,_) =>
                v.domainAsContext.getIncludes.foreach {from => f(HasViewFrom(from,to))}
              case _ =>
            }
         case _ =>
      }
      e match {
         case t: Module =>
            t.getDeclarations foreach {d => {
               val dec = Declares(path,d.path)
               d match {
                  case c: Constant =>
                     f(dec)
                     f(IsConstant(c.path))
                     c.alias foreach {a =>
                       f(IsAliasFor(t.path ? a, c.path))
                     }
                     // extract dependencies - this may make the rel files a lot bigger
                     c.getComponents foreach {
                       case DeclarationComponent(dim, oc: ObjContainer[_]) => doDependencies(c.path $ dim, oc)
                       case _ =>
                     }
                  case s: Structure =>
                     val from = s.from match {
                        case OMPMOD(p,_) => p
                        case f => TheoryExp.simplify(s.from).toMPath
                     }
                     if (s.isInclude) {
                        if (!s.isGenerated)
                          // we do not export transitive includes
                          f(Includes(t.path, from))
                     } else {
                        f(dec)
                        f(HasDomain(s.path, from))
                        f(HasCodomain(s.path, TheoryExp.simplify(s.to).toMPath))
                        f(IsStructure(s.path))
                     }
                  case rc: RuleConstant =>
                     f(dec)
                     f(IsConstant(rc.path))
                  case dd: DerivedDeclaration =>
                     f(dec)
                     f(IsDerivedDeclaration(dd.path))
                     dd.feature match {
                       case Pattern.feature => f(IsPattern(dd.path))
                       case Instance.feature =>
                          f(IsInstance(dd.path))
                          dd.tpC.get match {
                            case Some(Instance.Type(p,_)) => f(IsInstanceOf(dd.path, p))
                            case _ =>
                          }
                       case _ =>
                     }
                  case nm: NestedModule =>
                     f(dec)
                     apply(nm.module)
               }
            }}
         case _ =>
      }
      e match {
        case l: Link if l.isImplicit =>
          f(IsImplicitly(l.to.toMPath,l.from.toMPath))
        case _ =>
      }
   }
   
   /** extract all dependencies of object containers */
   private def doDependencies(path: Path, oc: ObjContainer[_])(implicit f: RelationalElement => Unit) {
     oc.dependsOn foreach {p =>
       val r = DependsOn(path, p)
       f(r)
     }
   }
}
