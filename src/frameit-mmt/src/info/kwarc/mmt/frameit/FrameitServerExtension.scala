package info.kwarc.mmt.frameit

import cats.effect.IO
import com.twitter.finagle.Http
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.checking.Solver
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Extension, Logger}
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.objects.{Context, OMA, OMBIND, OMS, OMV, Obj, StatelessTraverser, Term, Traverser}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, IncludeData, TermContainer}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, RuleSet}
import io.circe.generic.auto._
import io.finch._
import io.finch.circe._

import scala.collection.immutable._

class FrameitServerExtension extends Extension with Logger with Endpoint.Module[IO] {
  override val logPrefix = "frameit"
  val FRAMEIT_ARCHIVE_ID = "Playground/frameit"

  var frameitArchive: Archive = _
  var factTheory: Theory = _
  var presenter: MMTSyntaxPresenter = _

  implicit val factEncoder = FactEnconderWrapper.getFactEncoder(() => presenter)
  import MPathEncoderWrapper.mpathEncoder  // possibly shown as unused by IntelliJ. Do not remove, it IS used!

  override def start(args: List[String]): Unit = {
    super.start(args)

    val endpoints =
      init :+: build :+: rebuild :+: addPointFact :+: addDistanceFact :+: getAllFacts :+: getAllScrolls

    Http.server.serve(
      ":8081",
      endpoints.toServiceAs[Application.Json]
    )
  }

  def init: Endpoint[IO, Unit] = post(path("init")) {
    presenter = controller.extman.getOrAddExtension(classOf[MMTSyntaxPresenter], classOf[MMTSyntaxPresenter].getCanonicalName, Nil).get

    frameitArchive = {
      // Try quering the archive, possibly building once, then giving up
      controller.backend.getArchive(FRAMEIT_ARCHIVE_ID).getOrElse {
        controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID mmt-omdoc")
        controller.backend.getArchive(FRAMEIT_ARCHIVE_ID).getOrElse(
          throw info.kwarc.mmt.api.GetError(s"Archive $FRAMEIT_ARCHIVE_ID not found")
        )
      }
    }
    factTheory = controller.getTheory((DPath(frameitArchive.narrationBase) / "annotation") ? "FactTheory")

    Ok(())
  } handle {
    case e: info.kwarc.mmt.api.Error => ServiceUnavailable(e)
  }

  def build: Endpoint[IO, Unit] = post(path("build")) {
    controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID mmt-omdoc")

    Ok(())
  }

  def rebuild: Endpoint[IO, Unit] = post(path("rebuild")) {
    controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID -mmt-omdoc")
    controller.handleLine(s"build $FRAMEIT_ARCHIVE_ID mmt-omdoc")

    Ok(())
  }

  def addPointFact: Endpoint[IO, Unit] = post(path("fact") :: path("add") :: path("point") :: jsonBody[UnityPoint]) { (point: UnityPoint) =>
    factTheory.add(FrameIT.Annotations.PointAnnotation(point, factTheory.toTerm))

    Ok(())
  }

  def addDistanceFact: Endpoint[IO, Unit] = post(path("fact") :: path("add") :: path("distance") :: jsonBody[UnityDistance]) { distance: UnityDistance =>
    factTheory.add(FrameIT.Annotations.DistanceAnnotation(distance, factTheory.toTerm))

    Ok(())
  }

  def getAllFacts: Endpoint[IO, List[Fact]] = get(path("fact") :: path("list") :: paramOption[Boolean]("simplified").withDefault(false)) { simplified: Boolean => {
    // TODO How to get solver out of controller?
    val simplificationUnit = SimplificationUnit(factTheory.getInnerContext, expandDefinitions = true, fullRecursion = true, solverO = None)

    def simplify(obj: Obj): obj.ThisType = controller.simplifier.apply(obj, simplificationUnit)

    val declarations: Seq[Declaration] = {
      if (!simplified) {
        factTheory.getDeclarations
      } else {
        // Simplify type and definiens components of constant declarations
        factTheory.getDeclarations.map {
          case c: Constant =>
            new FinalConstant(
              home = c.home,
              name = c.name,
              alias = c.alias,
              tpC = TermContainer.asParsed(c.tp.map(simplify(_))),
              dfC = TermContainer.asParsed(c.df.map(simplify(_))),
              rl = c.rl,
              notC = c.notC,
              vs = c.vs
            )
          case x => x
        }
      }
    }

    val reconstructedFacts: Seq[Fact] = declarations.collect {
        case FrameIT.Annotations.PointAnnotation(point) => point
        case FrameIT.Annotations.DistanceAnnotation(distance) => distance
        case d => UnknownFact(d)
    }

    Ok(reconstructedFacts.toList)
  }
  }

  def getAllScrolls: Endpoint[IO, List[Scroll]] = get(path("scroll") :: path("list")) {
    val scrolls = Scroll.findAllIn(
      loadFrameItModules().map(controller.getAs(classOf[Module], _))
    ).toList

    Ok(scrolls)
  }

  def loadFrameItModules(): List[MPath] = {
    controller.getAs(classOf[Document], DPath(frameitArchive.narrationBase)).getModules(controller.globalLookup)
  }

  /*lazy val checker = {
    val ret = new MMTStructureChecker(new RuleBasedChecker)
    controller.extman.addExtension(ret)
    ret
  }*/
  /*implicit lazy val ce : CheckingEnvironment = new CheckingEnvironment(controller.simplifier,ErrorThrower,RelationHandler.ignore, this)*/

  private def getAllSymbols(top : MPath) = {
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
   * @param assignments.
   * @param metatheory: The meta-theory, whose symbols should be identified.
   * @return Some(tpi) if expected type contains no gaps, otherwise [[None]].
   */
  def expectedType(assignments : List[(GlobalName, Term)], metatheory : MPath, tp : Term) : Option[Term] = {
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
   * @param assignments.
   * @param metatheory: The meta theory on which all assignments should be the identity.
   * @return new assignments as List[(GlobalName, Term)].
   */
  def closeGaps(assignments : List[(GlobalName, Term)], metatheory : MPath) : List[(GlobalName, Term)] = {
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
