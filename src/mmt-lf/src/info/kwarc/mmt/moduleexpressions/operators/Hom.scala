package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects.{Term, _}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.{Apply, ApplyGeneral, Arrow, FunType}

object Hom extends UnaryConstantScala(Combinators._path, "hom")

/* TODO: Use the mod */
object ComputeHom extends ComputationRule(Hom.path) {
  val homlabel = LocalName("hom")
  val m1label = LocalName("m1")
  val m2label = LocalName("m2")

  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Hom(thyT) = tm
    val dia: AnonymousDiagram = Common.asAnonymousDiagram(solver, thyT).getOrElse {
      return RecurseOnly(List(1))
    }
    val node: DiagramNode = dia.getDistNode.getOrElse(return Recurse)
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
    val decls: List[OML] = thy.decls

    /* The declarations of the theory */
    def copy(prefix: String) = {
      /* The list of new declarations */
      var renames: List[(LocalName, LocalName)] = Nil
      val r = new OMLReplacer(l => utils.listmap(renames, l).map(n => OML(n)))

      def replace(t: Term) = {
        r(t, Context.empty)
      }

      val declsR: List[OML] = decls map { o =>
        val newname = LocalName(prefix) / o.name
        val d = new OML(newname, o.tp map replace, o.df map replace)
        renames ::= (o.name, newname)
        d
      }
      (declsR, renames.reverse)
    }

    val (mod1, ren1) = copy(ComputeHom.m1label.toString)
    val (mod2, ren2) = copy(ComputeHom.m2label.toString)
    val mor1 = Rename.pairsToMorph(ren1)
    val mor2 = Rename.pairsToMorph(ren2)

    def makeNames(base: String, n: Int) = Range(0, n).toList.map(i => LocalName(base + i))

    /* **************************************************************** */
    /* *************** Computing the homomorphisms ******************** */
    /* **************************************************************** */
    val homdecls = decls mapOrSkip { o =>
      val o1 = OML(m1label / o.name)
      val o2 = OML(m2label / o.name)
      val tpOld: Term = o.tp.get
      val tp: Term = tpOld match {
        // sort symbol, i.e., c: sort
        // generate morphism map c: m1/c -> m2/c
        case OMS(sort) =>
          Arrow(OML(m1label / o.name), OML(m2label / o.name))
        /* function symbol, i.e., c: tm s1 -> ... -> tm sn -> tm r
         * generate preservation axiom c: ded forall x1: tm s1, ..., xn: tm sn. r(m1/c x1 ... xn) = m2/c (s1 x1) ... (sn xn) */
        case FunType(args, SFOL.term(r)) =>
          if (o.df.isDefined) {
            // TODO emit warning that we are omitting the proof
          }
          val argsorts = args.map {
            /* list of sorts of the input to the function */
            case (None, SFOL.term(s)) => s
          }
          /* variables associated with their sorts */
          val vars = makeNames("x", args.length)
          val varSorts = vars zip argsorts
          val left = Apply(r, ApplyGeneral(o1, vars.map(OMV(_))))
          val right = ApplyGeneral(o2, varSorts map { case (x, s) => Apply(s, OMV(x)) })
          val r2 = mor2(r)
          val body = SFOL.equal(SFOL.term(r2), left, right)
          /* The first argument is the sort of the = */
          val ax = SFOL.forall.multiple(varSorts, body)
          PL.ded(ax)
        // predicate symbol, i.e., c: tm s1 -> ... -> tm sn -> prop
        // generate preservation axiom c: ded forall x1: tm s1, ..., xn: tm sn. m1/c x1 ... xn => m2/c (s1 x1) ... (sn xn)
        /* The case of prediacte symbols */
        case FunType(args, OMS(PL.prop)) =>
          if (o.df.isDefined) {
            // TODO this usually has to be forbidden, so throw error here
            // (technically, only forall, implies, negation are forbidden)
          }
          val argsorts = args.map {
            /* list of sorts of the input to the function */
            case (None, SFOL.term(s)) => s
          }
          /* variables associated with their sorts */
          val vars = makeNames("x", args.length)
          val varSorts = vars zip argsorts
          val left = ApplyGeneral(o1, vars.map(OMV(_)))
          val right = ApplyGeneral(o2, varSorts map { case (x, s) => Apply(s, OMV(x)) })
          val body = PL.implies(left, right)
          /* The first argument is the sort of the = */
          val ax = SFOL.forall.multiple(varSorts, body)
          PL.ded(ax)
        // axiom, i.e., c: ded F
        case PL.ded(_) => throw SkipThis
        // skip
      }
      OML(o.name, Some(tp), None)
    }
    // val homNode = DiagramNode(homlabel,new AnonymousTheory(Some(SFOL._path),mod1:::mod2:::homdecls))
    val arrow1 = DiagramArrow(m1label, node.label, homlabel, mor1, false)
    val arrow2 = DiagramArrow(m2label, node.label, homlabel, mor2, false)

    val hom = DiagramNode(homlabel, new AnonymousTheory(thy.mt, mod1 ::: mod2 ::: homdecls))
    /* Try to use the Mod operator to get the two mdoels */

    val result = new AnonymousDiagram(List(hom), List(arrow1, arrow2), Some(homlabel))
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