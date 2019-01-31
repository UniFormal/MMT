package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import objects._
import utils._
import uom._
import info.kwarc.mmt.lf._

import scala.collection.mutable.HashSet

object Hom extends UnaryConstantScala(Combinators._path, "homomorphism")

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
  object term extends UnaryLFConstantScala(_path, "term")
}

object ComputeHom extends ComputationRule(Hom.path) {
  val homlabel = LocalName("hom")
  val m1label = LocalName("m1")
  val m2label = LocalName("m2")

  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val Hom(thyT) = tm
    val dia = Common.asAnonymousDiagram(solver, thyT).getOrElse {return RecurseOnly(List(1))}
    val node = dia.getDistNode.getOrElse(return Recurse)
    val thy = node.theory // Common.asAnonymousTheory(solver, thyT).getOrElse {return RecurseOnly(List(1))}
    if (covered || !thy.mt.contains(SFOL._path)) {
      solver.error("wrong meta theory; expected: SFOL; found " + thy.mt)
      return Recurse
    }
    val decls = thy.decls
    def copy(prefix: String) = {
      var renames : List[(LocalName,LocalName)] = Nil
      def replace(t: Term) = {
        val r = new OMLReplacer(l => utils.listmap(renames, l).map(n => OML(n)))
        r(t, Context.empty)
      }
      val declsR = decls map {o =>
        val newname = LocalName(prefix) /  o.name
        val d = new OML(newname, o.tp map replace, o.df map replace)
        renames ::= (o.name, newname)
        d
      }
      (declsR, renames)
    }
    val (mod1, ren1) = copy(ComputeHom.m1label.toString)
    val (mod2, ren2) = copy(ComputeHom.m2label.toString)
    /*
    val homdecls = decls map {o =>
       val tp = o.tp.get match {
         case OMS(SFOL.sort) =>
           Arrow(OML(m1label / o.name), OML(m2label / o.name))
         case FunType(args, SFOL.term(r)) =>
           ???
         case FunType(args, OMS(PL.prop)) =>
           ???
         case PL.ded(_) =>
           // skip
       }
      OML(o.name, Some(tp), None)
    }
    */
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