package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.FormalAlignment
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath}

import scala.collection.mutable
import scala.util.Try

case class Consthash(name:GlobalName, hash: List[Any], pars: List[GlobalName],
                     isProp: Boolean, optDef : Option[(Int,List[GlobalName])]) {

  //def matches(l:List[(GlobalName,GlobalName)])(that:Consthash):Boolean = newPairs(l)(that).isEmpty
  override def toString = name.toString + "[" + pars.map(_.name.toString).mkString(", ") + "]" +
    optDef.map(p => " = [" + p._2.map(_.name.toString).mkString(", ") + "]").getOrElse("")

  def <>(that : Consthash) = this.hash == that.hash && this.pars.length == that.pars.length
  def !<>(that :Consthash) = !(this <> that)
}

class Theoryhash(val path:MPath) {
  protected var consts : List[Consthash] = Nil
  protected var includes : List[Theoryhash] = Nil
  protected var allincludes : List[Theoryhash] = Nil
  protected var allconsts : List[Consthash] = Nil

  def getLocal = consts
  def getAll = allconsts
  def getincludes = includes
  def getAllIncludes = allincludes

  private def toStringIndent(ind:String) : String =
    ind + path.name.toString +
      includes.map(x => "\n" + x.toStringIndent(ind + "  ")).mkString("\n") +
      consts.map(p => "\n  " + ind + p.toString).mkString("")

  override def toString = toStringIndent("")

}

class Consthasher(controller:Controller,
                  sources : List[DeclaredTheory],
                  targets : List[DeclaredTheory],
                  fixing : List[MPath],
                  alignments : List[FormalAlignment],
                  judgments : (Option[GlobalName],Option[GlobalName]),
                  doDefs : Boolean
                 ) {

  private var theories : scala.collection.mutable.HashMap[MPath,OpenTheoryhash] = mutable.HashMap[MPath,OpenTheoryhash]()
  private val translator = controller.extman.get(classOf[Translator]).headOption.getOrElse {
    val a = new Translator
    controller.extman.addExtension(a)
    a
  }
  private val translations = alignments.flatMap(translator.fromAlignment)

  private class OpenTheoryhash(p : MPath) extends Theoryhash(p) {
    def addConstant(c : Consthash) = consts::=c
    def addInclude(th : Theoryhash) = includes::=th
    def init: Unit = {
      allincludes = (includes:::includes.flatMap(_.getAllIncludes)).distinct
      allconsts = consts ::: allincludes.flatMap(_.getLocal)
    }
  }

  def getTheory(p : MPath) : Theoryhash = theories.getOrElse(p,{
    if (!fixing.contains(p)) get(controller.get(p).asInstanceOf[DeclaredTheory])
    else {
      val h = new OpenTheoryhash(p)
      h.init
      theories += ((p,h))
      h
    }
  })

  def getTheory(th : DeclaredTheory) : Theoryhash = theories.getOrElse(th.path,get(th))

  private def get(th : DeclaredTheory) : OpenTheoryhash = {
    val h = new OpenTheoryhash(th.path)
    if (!fixing.contains(th.path)) {
      th.getConstants.collect({ case c : FinalConstant => c }) foreach (c =>
        if (!alignments.exists(a => a.from.mmturi == c.path || a.to.mmturi == c.path)) h.addConstant(doConstant(c))
        )
      th.getIncludes.foreach(t => h.addInclude(getTheory(t)))
    }
    h.init
    theories += ((th.path,h))
    h
  }

  def doConstant(c:FinalConstant) : Consthash = {
    var isAxiom = false
    var pars : List[GlobalName] = Nil

    def traverse(t: Term)(implicit vars : List[LocalName]) : List[Int] = {
      val al = translations.find(_.isApplicable(t).isDefined)
      val allist = al.map(a => List(a.from,a.to)).getOrElse(Nil)
      if (judgments._1.exists(p => allist contains p) || judgments._2.exists(p => allist contains p)) isAxiom = true
      al.map(a => a.apply(t).head).getOrElse(t) match {
        case OMV(name) =>
          List(0, 2 * vars.indexOf(name))
        case OMS(path) =>
          if (judgments._1.contains(path) || judgments._2.contains(path)) isAxiom = true
          List(1, alignments.find(a => a.from.mmturi == path || a.to.mmturi == path).map( a =>
            2 * alignments.indexOf(a) + 1).getOrElse(2 * (alignments.length + {
            if (pars.contains(path)) pars.length - pars.indexOf(path) else {
              pars ::= path
              pars.length
            }
          }) + 1))
        case OMA(f, args) =>
          2 :: args.length :: traverse(f) ::: args.flatMap(traverse)
        case OMBINDC(f,con,bds) =>
          val (cont,newvars) = con.foldLeft((List(3,con.length), vars))((p,v) =>
            (p._1 ::: v.tp.map(traverse(_)(p._2)).getOrElse(List(-1)),v.name :: p._2))
            cont ::: List(bds.length) ::: bds.flatMap(traverse(_)(newvars))
        case OMLIT(value,rt) =>
          4 :: value.hashCode :: traverse(rt.synType)
        case UnknownOMLIT(s,tp) =>
          5 :: s.hashCode :: traverse(tp)
        case OML(VarDecl(name,tp,df,_)) =>
          6 :: tp.map(traverse).getOrElse(List(-1)) ::: df.map(traverse).getOrElse(List(-1))
        case tm =>
          println("Missing: " + tm.getClass)
          ???
      }
    }
    val hash = c.tp.map(traverse(_)(Nil)).getOrElse(Nil).asInstanceOf[List[Any]]
    val tppars = pars
    val optDef = if(doDefs) {
      pars = Nil
      c.df.map(traverse(_)(Nil))
    } else None
    Consthash(c.path,hash,tppars,isAxiom,optDef.map(d => (d.hashCode(),pars)))
  }

}

object AxiomHandler {

  /**
    * Tries to recursively find a/the [role Judgment] declaration used for axioms in a theory
    *
    * @param th .
    * @return the GlobalName for the Judgment declaration.
    */

  def findJudgment(ctrl:Controller,th:DeclaredTheory,includes:Option[Set[DeclaredTheory]]):Option[GlobalName] = {

    def findJudgmentIt(th:DeclaredTheory):Option[GlobalName] = {
      val list = for {o <- th.getConstants.filter(p => p.rl match {
        case t: Some[String] => true
        case _ => false
      }) if o.rl.get == "Judgment"} yield o match {
        case t: FinalConstant => t.path
        case _ => throw new Exception("FinalConstant Expected!")
      }
      list.headOption
    }

    val ths = includes match {
      case Some(set) => set
      case None =>
        ctrl.simplifier.apply(th)
        th.getIncludes.map(ctrl.get(_).asInstanceOf[DeclaredTheory])//closer.getIncludes(th,true)
    }
    ((ths map findJudgmentIt) collect {case Some(x) => x}).headOption

  }

  /**
    * Checks whether the FinalConstant
    *
    * @param a is an axiom by looking for occurences of
    * @param judg in the type of a. Used in the process of calculating ConstHash.
    * @return true, if a is deemed an axiom.
    */

  def isAxiom(a:FinalConstant,judg:GlobalName):Boolean = a.tp match {
    case None => false
    case Some(tp) => isAxiomIterator(tp,judg)
  }

  /**
    * Iterator method used by isAxiom.
    *
    * @param tp Some term.
    * @param judg as above.
    * @return true, if tp is deemed an axiom.
    */

  def isAxiomIterator(tp:Term,judg:GlobalName):Boolean = tp match {
    case t: OMV => false
    case t: OMID => t.head.get==judg
    case t: OMA => ((t.head.get==judg)::(for{o <- t.args} yield isAxiomIterator(o,judg))) contains true
    case t: OMBINDC => ((t.head.get==judg)::(for{o <- t.scopes} yield isAxiomIterator(o,judg))) contains true
    case _ => false
  }

}
