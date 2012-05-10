package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import modules._
import utils._

object Morph {
  /** pre: m is a well-structured morphism */
	def domain(m : Term)(implicit lib : Lookup) : Term = m match {
      case OMIDENT(t) => t
      case OMCOMP(n :: _) => domain(n)
      case MEmpty(f,_) => f
      case MUnion(ms) => TUnion(ms map domain)
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => l.from}
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case OMDL(h,n) => try {
         val structure = lib.getStructure(h % n)
         structure.from
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case _ => throw Invalid("not a well-formed morphism: " + m)
    }

  /** pre: m is a well-structured morphism */
   def codomain(m : Term)(implicit lib : Lookup) : Term = m match {
      case OMIDENT(t) => t
      case OMCOMP(l) => codomain(l.last)
      case MEmpty(_,t) => t
      case MUnion(ms) =>
         val cs = ms map codomain
         if (cs.forall(_ == cs.head)) cs.head
         else TUnion(cs)
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => l.to}
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case OMDL(t, _ ) => t
      case _ => throw Invalid("not a well-formed morphism: " + m)
   }
}

/**
 * An OMMOD represents a reference to a module (which may be a theory or a morphism expression).
 * @param path the path to the link, structures are referenced by their module level path
 */
object OMMOD {
   def apply(path : MPath) : OMID = OMID(path)
   def unapply(term : Term) : Option[MPath] = term match {
     case OMID(m: MPath) => Some(m)
     case _ => None
   }
}

object TEmpty {
   def apply(meta: Option[MPath]) : Term = meta match {
     case None => OMID(mmt.tempty)
     case Some(m) => OMA(OMID(mmt.tempty), List(OMMOD(m)))
   }
   def unapply(t : Term) : Option[MPath] = t match {
     case OMA(OMID(mmt.tempty), List(OMMOD(m))) => Some(m)
     case _ => None
   }
}

object TUnion {
   /*def apply(thys: List[MPath]) : Term = thys match {
      case Nil => TEmpty(None)
      case hd :: Nil => OMMOD(hd)
      case hd :: tl => OMA(OMID(mmt.tunion), List(hd, apply(tl)))
   } */
   def apply(thys: List[Term]) : Term = thys match {
     case Nil => TEmpty(None)
     case hd :: Nil => hd
     case hd :: tl => OMA(OMID(mmt.tunion), tl.toList)
   }
   def unapply(union: Term) : Option[List[Term]] = union match {
     case OMA(OMID(mmt.tunion), thys) if (thys.forall(_.isInstanceOf[Term])) => Some(thys.asInstanceOf[List[Term]])
     case _ => None
   }
}

/**
 * An OMDL represents a structure
 */
object OMDL {
   def apply(cod: Term, name: LocalName) : Term = OMID(cod % name)
   def unapply(t: Term) : Option[(Term, LocalName)] = t match {
     case OMID(cod % name) => Some((cod, name))
     case _ => None
   }
}

/**
 * An OMCOMP represents the associative composition of a list of morphisms. Compositions may be nested.
 * @param morphisms the list of morphisms in diagram order
 */
object OMCOMP {
   def apply(morphs: List[Term]) : Term = morphs match {
     case Nil => throw ImplementationError("composition of 0 morphisms")
     case hd :: Nil => hd
     case _ => OMA(OMID(mmt.composition), morphs)
   }
   def apply(morphs: Term*) : Term = apply(morphs.toList)
   def unapply(t: Term) : Option[List[Term]] = t match {
     case OMA(OMID(mmt.composition), morphs) => Some(morphs)
     case _ => None
   }
}

/**
 * An OMIDENT represents the identity morphism of a theory.
 * @param theory the theory
 */
object OMIDENT {
   def apply(theory : Term) : Term = OMA(OMID(mmt.identity), List(theory))
   def unapply(t : Term) : Option[Term] = t match {
     case OMA(OMID(mmt.identity), List(theory)) => Some(theory)
     case _ => None
   }
}

object MEmpty {
   def apply(from: Term, to: Term) : Term = OMA(OMID(mmt.mempty), List(from, to))
   def unapply(t : Term) : Option[(Term, Term)] = t match {
     case OMA(OMID(mmt.mempty), List(from, to)) => Some((from,to))
     case _ => None
   }
}

object MUnion {
   def apply(morphs: List[Term]) : Term = morphs match {
      case Nil => throw ImplementationError("union of 0 morphisms")
      case hd :: Nil => hd
      case hd :: tl => OMA(OMID(mmt.munion), morphs)
   }
   def unapply(t : Term) : Option[List[Term]] = t match {
     case OMA(OMID(mmt.munion), morphs) if (morphs.forall(_.isInstanceOf[Term])) => Some(morphs.asInstanceOf[List[Term]])
     case _ => None
   }
}

/*
case class OMUNIONPIM(push: Morph, along: Morph, wth: Morph) extends Morph with ComposedModuleObject {
   def role = Role_pushout_im
   def components = List(push, along, wth)
   override def toString = push.toString + " + " + along.toString + " with " + wth.toString 
}

case class OMPIW(raise: Morph, to: TheoryObj) extends Morph with ComposedModuleObject {
   def role = Role_pushout_iim
   def components = List(raise, to)
   override def toString = raise.toString + " * " + to.toString
}
*/
