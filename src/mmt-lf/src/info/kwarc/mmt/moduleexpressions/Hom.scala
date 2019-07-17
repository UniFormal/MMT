package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import objects.{Term, _}
import utils._
import uom._
import info.kwarc.mmt.lf._

import scala.collection.mutable.HashSet

object Hom extends UnaryConstantScala(Combinators._path, "hom")

/** MMT declarations
 *  
 *  namespace http://cds.omdoc.org/examples
 *  fixmeta ?LF
 *
 *  theory PL = 
 *    prop : type
 *    ded : prop -> type
 *
 *  theory SFOL =
 *    sort : type
 *    term : sort -> type
 */

object PL extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("PL")
  val prop = _path ? "prop"
  object ded extends UnaryLFConstantScala(_path, "ded")
}

object SFOL extends TheoryScala {
  val _base = DPath(URI("http://cds.omdoc.org/examples"))
  val _name = LocalName("SFOL")
  val sort = _path ? "sort"
  /*
  object forall{
      val _path = SFOL._path ? "forall"
      def apply(context: Context,body : Term): Term ={
        OMBIND(OMS(_path),context,body)
        Apply(OMS(path), Lambda(..)
      }
      def unapply(t : Term): Option[(Term,Context,Term)] = t match {
        case OMBIND(binder,context,body) => Some((binder,context,body))
        case _ => None
      }
  }
  */
  object term extends UnaryLFConstantScala(_path, "term")
  object equal extends TernaryLFConstantScala(_path, "equal")
  object forall extends TypedBinderScala(_path, "forall", term)
  object exists extends TypedBinderScala(_path, "exists", term)
}

/* TODO: Use the mod */
object ComputeHom extends ComputationRule(Hom.path) {
  val homlabel = LocalName("hom")
  val m1label = LocalName("m1")
  val m2label = LocalName("m2")

  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Hom(thyT) = tm
    val dia : AnonymousDiagram = Common.asAnonymousDiagram(solver, thyT).getOrElse {return RecurseOnly(List(1))}
    val node : DiagramNode = dia.getDistNode.getOrElse(return Recurse)
    val thy = node.theory // Common.asAnonymousTheory(solver, thyT).getOrElse {return RecurseOnly(List(1))}

    /* TODO: Check that the meta-theory contains SFOL, not that it is SFOL */
    /*
    if (covered || !thy.mt.contains(SFOL._path)) {
      solver.error("wrong meta theory; expected: SFOL; found " + thy.mt)
      return Recurse
    }*/
 /* **************************************************************** */
 /* *************** Creating the two models ************************ */
 /* **************************************************************** */
    val decls : List[OML] = thy.decls /* The declarations of the theory */
    def copy(prefix: String) = {
      /* The list of new declarations */
      var renames : List[(LocalName,LocalName)] = Nil
      def replace(t: Term) = {
        val r = new OMLReplacer(l => utils.listmap(renames, l).map(n => OML(n)))
        r(t, Context.empty)
      }
      val declsR : List[OML] = decls map {o =>
        val newname = LocalName(prefix) /  o.name
        val d = new OML(newname, o.tp map replace, o.df map replace)
        renames ::= (o.name, newname)
        d
      }
      (declsR, renames)
   }
    val (mod1, ren1) = copy(ComputeHom.m1label.toString)
    val (mod2, ren2) = copy(ComputeHom.m2label.toString)

    def makeNames(base: String, n: Int) = Range(0,n).toList.map(i => LocalName(base+i))

    import SFOL._

    /* **************************************************************** */
    /* *************** Computing the homomorphisms ******************** */
    /* **************************************************************** */
    val homdecls = decls mapOrSkip {o =>
      val tp: Term = o.tp.get match {
      /* TODO: This OML part is wrong. I need to throw it away */
      case OML(n, typ, df, nt, featureOpt) =>
        Arrow(OML(m1label/o.name), OML(m2label/o.name))
      // sort symbol, i.e., c: sort
      // generate morphism map c: m1/c -> m2/c
      case OMS(SFOL.sort) =>
         Arrow(OML(m1label/o.name), OML(m2label/o.name))
      /* function symbol, i.e., c: tm s1 -> ... -> tm sn -> tm r
       * generate preservation axiom c: ded forall x1: tm s1, ..., xn: tm sn. r(m1/c x1 ... xn) = m2/c (s1 x1) ... (sn xn) */
      case FunType(args, SFOL.term(r)) =>
        val argsorts = args.map { /* list of sorts of the input to the function */
          case (None, SFOL.term(s)) => s
        }
        /* variables associated with their sorts */
        val vars = makeNames("x", args.length)
        val varSorts = vars zip argsorts
        val left = Apply(r, ApplySpine(OML(m1label/o.name), vars.map(OMV(_)):_*))
        val right = ApplySpine(OML(m2label/ o.name),varSorts map {case (x,s) => Apply(s,OMV(x))}:_*)
        val body = equal(left,right)
        val ax = forall.multiple(varSorts, body)
        PL.ded(ax)
     // predicate symbol, i.e., c: tm s1 -> ... -> tm sn -> prop
     // generate preservation axiom c: ded forall x1: tm s1, ..., xn: tm sn. m1/c x1 ... xn => m2/c (s1 x1) ... (sn xn)
      case FunType(args, OMS(PL.prop)) => throw SkipThis
        /*
        val argsorts = args.map {
          case (None, SFOL.term(s)) => s
        }
        val ax = ???
        PL.ded(ax) */
     // axiom, i.e., c: ded F
      case PL.ded(_) => throw SkipThis
       // skip
   }
   OML(o.name, Some(tp), None)
}
// val homNode = DiagramNode(homlabel,new AnonymousTheory(Some(SFOL._path),mod1:::mod2:::homdecls))
val arrow1 = DiagramArrow(m1label, node.label, homlabel , Rename.pairsToMorph(ren1), false)
val arrow2 = DiagramArrow(m2label, node.label, homlabel , Rename.pairsToMorph(ren2), false)

val hom = DiagramNode(homlabel, new AnonymousTheory(thy.mt, mod1:::mod2)) // TODO: include :::homdecls in the list of declarations
/* Try to use the Mod operator to get the two mdoels */

val result = new AnonymousDiagram(List(hom), List(arrow1,arrow2), Some(homlabel))
Simplify(result.toTerm)
}
}

/*
concrete syntax:
tm S --> tm S

OMDoc, MMT terms:
OMA(OMS(LF?Arrow), List(
OMA(OMS(LF?Apply),List(OMS(SFOL?term), OMV(S))))
OMA(OMS(LF?Apply),List(OMS(SFOL?term), OMV(S))))
)

MMT terms with LF helper objects
Arrow(
Apply(OMS(SFOL?term), OMV(S))
Apply(OMS(SFOL?term), OMV(S))
)

MMT terms with SFOL helper objects
Arrow(SFOL.term(OMV(S)), SFOL.term(OMV(S)))
*/