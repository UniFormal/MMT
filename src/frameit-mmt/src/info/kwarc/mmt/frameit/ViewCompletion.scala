package info.kwarc.mmt.frameit

import info.kwarc.mmt.api.{GlobalName, LocalName, MPath}
import info.kwarc.mmt.api.checking.Solver
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{Context, OMA, OMBIND, OMS, OMV, StatelessTraverser, Term, Traverser}
import info.kwarc.mmt.api.symbols.{Constant, IncludeData}

import scala.collection.immutable.{List, Nil}

/**
  * Utility methods for view completion, i.e. given arbitrary lists
  * of assignments (not necessarily constituting even a partial view),
  * infer gaps and types of gaps.
  *
  * @author first version by Dennis Müller
  */
object ViewCompletion {
  private def getAllSymbols(top : MPath)(implicit controller: Controller) = {
    var dones : List[MPath] = Nil
    def recurse(mp : MPath) : List[GlobalName] = if (dones contains mp) Nil else {
      dones ::= mp
      val th = controller.getAs(classOf[Theory],mp)
      th.getAllIncludes.flatMap {
        case IncludeData(_, from, Nil, _, _) =>
          recurse(from)
        case _ =>
          Nil
      } ::: th.getConstants.map(_.path)
    }
    recurse(top)
  }

  private def allSymbols(tm : Term) = {
    var symbols : List[GlobalName] = Nil
    val traverser = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMS(p) =>
          symbols ::= p
          t
        case _ => Traverser(this,t)
      }
    }
    traverser(tm,())
    symbols.distinct
  }

  /**
    * Computes the expected type for a constant of type
    * @param tp, using assignments stored in
    * @param assignments Provided assignments for putative view
    * @param metatheory: The meta-theory, whose symbols should be identified.
    * @return Some(tpi) if expected type contains no gaps, otherwise [[None]].
    */
  def expectedType(assignments : List[(GlobalName, Term)], metatheory : MPath, tp : Term)(implicit controller: Controller) : Option[Term] = {
    val assMap = scala.collection.mutable.HashMap.empty[GlobalName,Term]
    assignments.foreach {
      case (gn,tm) => assMap(gn) = tm
    }
    val varMap = scala.collection.mutable.HashMap.empty[GlobalName,LocalName]
    val metaSymbols = getAllSymbols(metatheory)
    val (ret,gaps) = expectedTypeInner(assMap,varMap,metaSymbols,0,tp)
    if (gaps > 0) None else Some(ret)
  }

  /**
    * Computes the expected type of tm under assignments in
    * @param assMap.
    * If gaps are found, new variables are introduced in
    * @param varMap.
    * @param metaSymbols: symbols from the meta theory that should be the identity.
    * @param solveVarOrig: index for introducing new variable names.
    * @param tm: Term to compute expected type for
    * @return Expected type, and new index for variable names
    */
  private def expectedTypeInner(assMap : scala.collection.mutable.HashMap[GlobalName,Term],
                                varMap : scala.collection.mutable.HashMap[GlobalName,LocalName],
                                metaSymbols : List[GlobalName],
                                solveVarOrig : Int,
                                tm : Term
                               ) = {
    var solveVar = solveVarOrig
    val traverser = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMS(s) if metaSymbols contains s =>
          t
        case OMS(s) if assMap.isDefinedAt(s) =>
          assMap(s)
        case OMS(s) if varMap.isDefinedAt(s) =>
          OMV(varMap(s))
        case OMS(s) =>
          val ln = LocalName() / "I" / solveVar.toString
          solveVar += 1
          varMap(s) = ln
          OMV(ln)
        case _ => Traverser(this,t)
      }
    }
    val ret = traverser(tm,())
    (ret,solveVar)
  }

  /**
    * Attempts to infer missing assignments uniquely implied by
    * @param assignments Provided assignments of putative view
    * @param metatheory: The meta theory on which all assignments should be the identity.
    * @return new assignments as List[(GlobalName, Term)].
    */
  def closeGaps(assignments : List[(GlobalName, Term)], metatheory : MPath)(implicit controller: Controller) : List[(GlobalName, Term)] = {
    val assMap = scala.collection.mutable.HashMap.empty[GlobalName,Term]
    assignments.foreach {
      case (gn,tm) => assMap(gn) = tm
    }

    val varMap = scala.collection.mutable.HashMap.empty[GlobalName,LocalName]
    val metaSymbols = getAllSymbols(metatheory)

    // Computes expected type and replaces gaps with variables
    var solveVar = 0

    // Compares expected type and type of image to close gaps
    def traverseParallel(tp1 : Term, tp2 : Term) : Unit = (tp1,tp2) match {
      case (_,_) if tp1 == tp2 =>
      case (OMBIND(s1,ct1,bd1), OMBIND(s2,ct2,bd2)) if s1 == s2 =>
        assert(ct1.length == ct2.length)
        ct1.indices.foreach {i =>
          (ct1(i).tp,ct2(i).tp) match {
            case (Some(tm1),Some(tm2)) => traverseParallel(tm1,tm2)
            case _ =>
          }
        }
        traverseParallel(bd1,bd2)
      case (OMA(f1,args1),OMA(f2,args2)) =>
        assert(args1.length == args2.length)
        traverseParallel(f1,f2)
        args1.indices.foreach{i =>
          traverseParallel(args1(i),args2(i))
        }
      case (OMV(ln),df) if varMap.values.toList contains ln =>
        varMap.find(_._2 == ln) match {
          case Some((gn,_)) =>
            if (assMap.isDefinedAt(gn)) {
              // TODO check equality?
            } else {
              assMap(gn) = df
            }
        }
      case _ => // terms differ structurally
    }

    // main loop: iterates over all assignments, looking for gaps to close
    assignments.foreach { case (sym,df) =>
      val const = controller.getAs(classOf[Constant],sym)
      const.tp match {
        case Some(tp) =>
          val (expectedType,nsolveVar) = expectedTypeInner(assMap,varMap,metaSymbols,solveVar,tp)
          if (nsolveVar > solveVar) { // There's at least one gap here
            solveVar = nsolveVar
            // infer type of df
            val context = allSymbols(df).map(_.module).distinct.foldLeft(Context())((c,mp) => c ++ Context(mp))
            val inftp = Solver.infer(controller,context,df,None)
            // compare two types to close gaps
            inftp match {
              case Some(itp) =>
                traverseParallel(expectedType,itp)
              case _ =>
            }
          }
        case _ =>
      }
    }
    val domain = assignments.map(_._1)
    assMap.toList.filterNot(domain contains _._1)
  }
}
