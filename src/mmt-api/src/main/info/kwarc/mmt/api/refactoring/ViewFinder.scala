package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.{Controller, Extension, Logger, Report}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.FormalAlignment
import info.kwarc.mmt.api.parser.ParseResult
import info.kwarc.mmt.api.refactoring.Hasher.Targetable
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.utils.time.{Duration, Time}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class FinderConfig(val finder : ViewFinder, protected val report : Report) extends Logger {
  override def logPrefix: String = "viewfinder"

  private var fromTheories_var : List[Theory] = Nil
  private var toTheories_var : List[Theory] = Nil
  private var commonTheories_var : List[Theory] = Nil
  private var fixing_var : List[AlignmentTranslation] = Nil
  private var judg1_var: Option[GlobalName] = None
  private var judg2_var: Option[GlobalName] = None
  private var doDefs_var : Boolean = false
  private var minimal_parameter_length_var = 0
  private var multithreaded : Boolean = false

  // def fromTheories : List[Theory] = fromTheories_var
  // def toTheories : List[Theory] = toTheories_var
  // def commonTheories : List[Theory] = commonTheories_var
  def fixing : List[AlignmentTranslation] = fixing_var
  def judg1: Option[GlobalName] = judg1_var
  def judg2: Option[GlobalName] = judg2_var
  def doDefs : Boolean = doDefs_var
  def minimal_parameter_length = minimal_parameter_length_var
  def isMultithreaded = multithreaded

  def setMultithreaded(b : Boolean) = multithreaded = b

  private def addAlignments(al : List[FormalAlignment]) = {
    fixing_var :::= al.map(finder.makeAlignment)
  }

  // val translations = fixing.map(AlignmentTranslation.apply(_,controller))

  def addJudgmentFrom(gn : GlobalName) = judg1_var = Some(gn)
  def addJudgmentTo(gn : GlobalName) = judg2_var = Some(gn)

  def setDoDefs(b : Boolean) = doDefs_var = b
  def setMinParLength(i : Int) = minimal_parameter_length_var = i

  def findJudgments(from : List[MPath], to : List[MPath]) = {
    // log("finding Judgments...")
    val j1 = if (judg1.isDefined) judg1 else finder.getJudgment(from)
    val j2 = if (judg2.isDefined) judg2 else finder.getJudgment(to)
    // log("Judgments are " + judg1 + " and " + judg2)
    j1.foreach(addJudgmentFrom)
    j2.foreach(addJudgmentTo)
  }

}

object NotDone extends Throwable {
  override def toString: String = "Viewfinder not initialized"
}

class ViewFinder extends frontend.Extension {
  override def logPrefix: String = "viewfinder"
  implicit private val ec = ExecutionContext.global //.fromExecutor(Executors.newFixedThreadPool(10000))

  private var initialized : Boolean = false
  private val theories : mutable.HashMap[String,(List[Theory],Option[GlobalName])] = mutable.HashMap()

  private lazy val preprocs = controller.extman.get(classOf[Preprocessor])

  def targets : List[String] = theories.keys.toList

  def isInitialized : Boolean = initialized

  override def start(args: List[String]): Unit = {
    val as = if (args.nonEmpty) {
      log("Archives: " + args.mkString(", "))
      args.map(controller.backend.getArchive(_).getOrElse(???))
    } else controller.backend.getArchives
    Future {
      log("Getting Archives...")
      val (t, _) = Time.measure {
        as.foreach { a => try {
          log("Doing " + a.id)
          preprocs.find(p => p.key != "" && a.id.startsWith(p.key)) match {
            case Some(pp) =>
              getArchive(a) match {
                case Some((ths, judg)) =>
                  theories(a.id) = (ths.map(pp.apply), judg)
                case _ =>

              }
            case _ =>
              log("No preproccesor for archive " + a.id)
            getArchive(a) match {
            case Some(r) => theories(a.id) = r
            case _ =>
          }
          }
        } catch {case t : Throwable => t.printStackTrace()} }
      }
      println("Finished after " + t)
    }.onComplete{_ => initialized = true}
  }

  def getJudgment(ths: List[MPath]): Option[GlobalName] = {
    var i = 0
    var judg: Option[GlobalName] = None
    while (judg.isEmpty && i < ths.length) {
      judg = findJudgment(controller.getAs(classOf[Theory],ths(i)))
      i += 1
    }
    judg
  }

  private[refactoring] def makeAlignment(al : FormalAlignment) =
    AlignmentTranslation(al, controller)

  private def findJudgment(th:Theory):Option[GlobalName] = {

    def findJudgmentIt(th:Theory):Option[GlobalName] = {
      val list = for {o <- th.getConstants.filter(p => p.rl match {
        case t: Some[String] => true
        case _ => false
      }) if o.rl.get == "Judgment"} yield o match {
        case t: FinalConstant => t.path
        case _ => throw new Exception("FinalConstant Expected!")
      }
      list.headOption
    }

    findJudgmentIt(th)
  }

  def getFlat(mpaths : List[MPath]) : List[Theory] = { // TODO properly

    var dones : List[Theory] = Nil
    def flatten(mp : MPath): Unit = {
      if (dones.exists(_.path == mp)) return ()
      controller.getO(mp) match {
        case Some(t : Theory) if !t.getDeclarations.exists(_.isInstanceOf[NestedModule]) =>
          t.getIncludes.foreach(flatten)
          dones ::= t
        case Some(t : Theory) =>
          var ns : List[Theory] = Nil
          val th = new Theory(t.parent,t.name,t.meta,t.paramC,t.dfC)
          t.getDeclarations.foreach {
            case c : FinalConstant => th add c
            case inc : Structure =>
              flatten(inc.from.toMPath)
              th add inc
            case nm : NestedModule if nm.module.isInstanceOf[Theory] =>
              // TODO this case does not match DerivedDeclaration anymore
              val old = nm.module.asInstanceOf[Theory]
              val in = new Theory(old.parent,old.name,old.meta,old.paramC,old.dfC)
              th.getDeclarations.foreach(in.add(_))
              old.getDeclarations.foreach(in.add(_))
              ns ::= in

            case _ =>
          }
          ns = ns.reverse
          ns.flatMap(_.getIncludes).foreach(flatten)
          ns.foreach(dones ::= _)
          dones ::= th
        case _ =>
          // log("MISSING: " + mp)
      }
    }

    mpaths foreach { mp => Try(flatten(mp)).getOrElse(log("Failed: " + mp)) }
    dones.reverse
/*
    // guarantees that the dependency closure is ordered
    val dones : mutable.HashMap[MPath,Option[List[Theory]]] = mutable.HashMap.empty
    def flatten(mp : MPath) : Option[List[Theory]] = {
      dones.getOrElseUpdate(mp, {
        // log("Doing: " + mp.toString)
        // println(mp)
        controller.getO(mp) match {
          case Some(t : Theory) =>
            var ns : List[Theory] = Nil
            val th = new Theory(t.parent,t.name,t.meta,t.paramC,t.dfC)
            t.getDeclarations.foreach {
              case c : FinalConstant => th add c
              case inc : Structure => th add inc
              case nm : NestedModule if nm.module.isInstanceOf[Theory] =>
                val old = nm.module.asInstanceOf[Theory]
                val in = new Theory(old.parent,old.name,old.meta,old.paramC,old.dfC)
                th.getDeclarations.foreach(in.add(_))
                old.getDeclarations.foreach(in.add(_))
                ns ::= in
              case _ =>
            }
            ns = ns.reverse
            val rec = (ns.flatMap(_.getIncludes) ::: th.getIncludes).distinct.map(flatten)
            if (rec.contains(None)) None else
              Some((rec.flatMap(_.get) ::: ns ::: List(th)).distinct)
          case _ =>
            log("MISSING: " + mp)
            None
        }
      })
    }

    mpaths foreach flatten
    dones.values.collect {
      case Some(thl) => thl
    }.toList.flatten.distinct
*/
  }

  def getArchive(a : Archive) : Option[(List[Theory], Option[GlobalName])] = Some{
    log("Collecting theories in " + a.id)
    val (t0,cont) = Time.measure { a.allContent }
    //log("Loaded after " + t0)
    //log("Flat sorting...")
    val (t1,ths) = Time.measure{ getFlat(cont) }
    // log("Flattened after " + t1)
    val judg = getJudgment(ths.map(_.path))
    (ths,judg)
  }

  def getHasher : Hasher = new HashesNormal(new FinderConfig(this,report),controller)

  def addArchives(a1 : Archive, a2 : Archive, hasher : Hasher) : Unit = {
    var lf = a1.root / "viewfinder_order"
    val froms = if (lf.exists) {
      log("Reading file " + lf)
      val ls = File.read(lf).split("\n").toList.distinct
      val mps = ls.map(s => Path.parseM(s,NamespaceMap.empty))
      log("Adding " + mps.length + " theories...")
      val ret = mps.indices.map{i =>
        print("\r  " + (i + 1) + " of " + mps.indices.length)
        controller.getAs(classOf[Theory],mps(i))
      }.toList
      println(" Done.")
      ret
    } else {
      log("Collecting theories in " + a1.id)
      val ths = getFlat(a1.allContent)
      File.write(lf,ths.map(_.path.toString).distinct.mkString("\n"))
      ths
    }

    lf = a2.root / "viewfinder_order"
    val tos = if (lf.exists) {
      log("Reading file " + lf)
      val ls = File.read(lf).split("\n").toList.distinct
      val mps = ls.map(s => Path.parseM(s,NamespaceMap.empty))
      log("Adding " + mps.length + " theories...")
      val ret = mps.indices.map{i =>
        print("\r  " + (i + 1) + " of " + mps.indices.length)
        controller.getAs(classOf[Theory],mps(i))
      }.toList
      println(" Done.")
      ret
    } else {
      log("Collecting theories in " + a2.id)
      val ths = getFlat(a2.allContent)
      File.write(lf,ths.map(_.path.toString).distinct.mkString("\n"))
      ths
    }

    log("Hashing...")
    val (t,_) = Time.measure {
      val commons = froms.filter(tos.contains)
      hasher.cfg.findJudgments(froms.map(_.path),tos.map(_.path))
      commons foreach (t => hasher.add(t,Hasher.COMMON))
      froms.filterNot(commons.contains) foreach (t => hasher.add(t,Hasher.FROM))
      tos.filterNot(commons.contains) foreach (t => hasher.add(t,Hasher.TO))
    }
    log("Done after " + t)
  }

  private val domainpath = Path.parseM("http://viewfinder.results?View",NamespaceMap.empty)

  def run(hasher : Hasher) = {
    val proc = new FindingProcess(this.report,hasher)
    log("Multithreaded: " + hasher.cfg.isMultithreaded)
    log("Judgment symbols are " + hasher.cfg.judg1 + " and " + hasher.cfg.judg2)
    val ret = proc.run()
    val (t1,ret1) = Time.measure {
      log("Making views...")
      proc.makeviews(domainpath,ret)
    }
    log("Done after " + t1 + " - " + ret1.length + " Views found")
    ret1
  }

  def find(mp : MPath, to : String, pre : Option[Preprocessor] = None) = {
    if (!initialized && !theories.keys.toList.contains(to)) throw NotDone

    val hasher = getHasher
    val fromsSimple = getFlat(List(mp))//.map(t => preproc.map(_.apply(t)).getOrElse(t))
    val meta = fromsSimple.find(_.path == mp).get.meta.getOrElse(???)
    val metaSimple = getFlat(List(meta))
    val preprocessor : Option[Preprocessor] = if (pre.isDefined) pre else
      metaSimple.reverse.collectFirst{
        case th if preprocs.exists(_.meta.contains(th.path)) => preprocs.find(_.meta.contains(th.path)).get
      }
    if (preprocessor.isDefined) log("Preproccesor found")
    val (metas,froms) = preprocessor match {
      case Some(pp) => (metaSimple.map(pp.apply),fromsSimple.map(pp.apply))
      case None => (metaSimple,fromsSimple)
    }
    val (tos,judg2) = theories(to)
    val judg1 = getJudgment(froms.map(_.path))
    val commons = tos.filter(t => metas.exists(_.path == t.path))
    judg1.foreach(hasher.cfg.addJudgmentFrom)
    judg2.foreach(hasher.cfg.addJudgmentTo)
    val (t,_) = Time.measure {
      log("Commons")
      commons.indices.foreach({i =>
        print("\r" + (i+1) + " of " + commons.length)
        hasher.add(commons(i), Hasher.COMMON)
      })
      println("")
      log("Froms")
      val nfrom = froms.filterNot(t => commons.exists(_.path == t.path))
      nfrom.indices.foreach({ i =>
        print("\r" + (i+1) + " of " + nfrom.length)
        hasher.add(nfrom(i), Hasher.FROM)
      })
      println("")
      log("Tos")
      val ntos = tos.filterNot(commons.contains)
      ntos.indices.foreach({ i =>
        print("\r" + (i+1) + " of " + ntos.length)
        hasher.add(ntos(i), Hasher.TO)
      })
    }
    println("")
    log("Hashing done after " + t)
    val proc = new FindingProcess(this.report,hasher)
    val ret = proc.run(from = List(hasher.get(mp).get))
    log(ret.map(_.asString()).mkString("\n"))
    val (t1,ret1) = Time.measure {
      log("Making views...")
      proc.makeviews(domainpath,ret)
    }
    log("Done after " + t1 + " - " + ret1.length + " Views found")
    ret1
  }

}

class FindingProcess(val report : Report, hash : Hasher) extends MMTTask with Logger {
  override def logPrefix: String = "viewfinder"
  implicit private val ec = ExecutionContext.global //.fromExecutor(Executors.newFixedThreadPool(10000))

  def run(from : List[Theoryhash]= Nil, to : List[Theoryhash] = Nil) = {
    log("Selecting Theories to use...")
    val (t1,tops1) = if (from.nonEmpty) (Duration(0.toLong),from) else Time.measure(select(hash.from))
    val (t2,tops2) = if (to.nonEmpty) (Duration(0.toLong),to) else Time.measure(select(hash.to))
    log("Done after " + (t1 + t2))

    log(tops1.length + " and " + tops2.length + " selected elements")
    log("Finding Views...")
    val (t,alls : Set[List[Map]]) = Time.measure(if (hash.cfg.isMultithreaded) {
      // One thread for each pair, should speed things up
      val ps = tops1.flatMap(t1 => tops2.map((t1,_)))
      val fs = Future.sequence(ps.map(p =>
        Future {
          // log("   Looking for: " + t1.path + " -> " + t2.path)
          val ret = findViews(p._1, p._2)
          // log("   " + t1.path.name + " -> " + t2.path.name + ": " + ret.size + " Results.")
          ret
        }(ec)
      ))
      while (!fs.isCompleted) {
        Thread.sleep(500)
      }
      fs.value.get.get.flatten.toSet
    } else {
      tops1.flatMap(t1 => tops2.flatMap { t2 =>
        // log("   Looking for: " + t1.path + " -> " + t2.path)
        // println(t1.toString)
        // println(t2.toString)
        val ret = findViews(t1, t2)
        // log("   " + t1.path.name + " -> " + t2.path.name + ": " + ret.size + " Results.")
        ret
      })
    }.toSet)
    log("Done after " + t)

    log("Postprocessing...")
    val (tlast,res) = Time.measure(postProcess(alls))
    log("Done after " + tlast)
    log(res.length + " maps found.")
    res.toList
  }

  // selects which theories to use. By default only maximal theories
  private def select(theories: List[Theoryhash]) : List[Theoryhash] = theories.indices.collect {
    case i if !theories.drop(i + 1).exists(p => p.getAllIncludes.contains(theories(i))) => theories(i)
  }.toList

  // the method used for matching two constants. Defaults to hash equality with all parameters pairwise matched up
  protected def matches(c: Consthash, d: Consthash)(l: List[(GlobalName, GlobalName)]): Boolean = {
    if (c !<> d) false
    else (c.pars zip d.pars).forall { case (Hasher.Symbol(p), Hasher.Symbol(q)) => p == q || l.contains((p, q))
    case _ => true
    }
  }

  //selects potential matches to work with; defaults to hash-equality
  protected def potentialMatches(t1: List[Consthash], t2: List[Consthash])
  = t1.flatMap(c => t2 collect { case d if c <> d => (c, d) })

  // gets starting points for viewfinding. Defaults to Axioms and minimal_parameter_length
  protected def getStartingPoints(implicit allpairs : List[(Consthash, Consthash)]): List[(Consthash, Consthash)]
  = (if (hash.cfg.judg1.isDefined || hash.cfg.judg2.isDefined) allpairs.filter(p => p._1.isProp || p._2.isProp)
  else allpairs).filter(p => p._1.pars.length >= hash.cfg.minimal_parameter_length)

  // does something with the resulting lists of pairs.

  protected def postProcess (views: Set[List[Map]]) = {
    val valued : mutable.HashMap[Map,Map] = mutable.HashMap.empty
    def eval(m : Map) : Map = valued.getOrElseUpdate(m, {
      val v = views.count(_.contains(Map(m.from,m.to,m.requires,0))).toDouble * 100.0 / views.size.toDouble
      Map(m.from,m.to,m.requires.map(eval),v)
    })
    log("  Evaluating...")
    val evals = views.toList.flatMap(_.map(eval))

    val order = hash.from.flatMap(_.getLocal.reverse)
    evals.distinct.sortBy(v => (order.indexWhere(p => Hasher.Symbol(p.name) == v.from),-v.value))
  }

  /*
  /**
    * Helper method, maximizing a partial morphism by adding all unique matches under an input morphism
    *
    * @param viewset the morphism as list of pairs of GlobalName
    * @return a new View as list of pairs of GlobalName
    */

  private def makeMaximal(viewset: List[(GlobalName, GlobalName)])(implicit allpairs : List[(Consthash, Consthash)])
  : List[(GlobalName, GlobalName)] = {
    def iterate(view: List[(GlobalName, GlobalName)], pairs: List[(Consthash, Consthash)]): List[(GlobalName, GlobalName)] = {
      val matches1 = pairs.filter(a => matches(a._1, a._2)(view)) //for{a <- pairs if a._1.matches(view)(a._2)}yield a
      val imm_matches = matches1.filter(p => !matches1.exists(q => q != p && (q._1 == p._1 || p._2 == q._2)))
      if (imm_matches.isEmpty) view
      else {
        val newview = view ::: imm_matches.map(p => (p._1.name, p._2.name))
        iterate(newview, pairs.filter(p => !imm_matches.exists(q => p._1.name == q._1.name || p._2.name == q._2.name)))
      }
    }

    iterate(viewset, allpairs).distinct
  }
  */

  private object MapStore {
    private var maps: mutable.HashMap[(GlobalName, GlobalName),Option[Map]] = mutable.HashMap.empty

    def get(n1 : GlobalName,n2 : GlobalName)(implicit allpairs : List[(Consthash, Consthash)]) : Option[Map] =
      maps.getOrElseUpdate((n1,n2),{
        allpairs.collectFirst { case p if p._1.name == n1 && p._2.name == n2 => p } match {
          case None =>
            None
          case Some((f,t)) => get(f, t)
        }
      })

    private def makeMap(gn1 : GlobalName,gn2 : GlobalName,requires : List[Map]) : Map =
      Map(Hasher.Symbol(simplify(gn1)),Hasher.Symbol(simplify(gn2)),requires.distinct,0)

    private def simplify(gn : GlobalName) =
      if (gn.name.steps.last == SimpleStep("defexp")) gn.module ? gn.name.steps.init else gn

    def get(from : Consthash, to : Consthash)(implicit allpairs : List[(Consthash, Consthash)]) : Option[Map] =
      maps.getOrElseUpdate((simplify(from.name),simplify(to.name)), {
        if (from !<> to) return None
        if (from.pars == to.pars) return Some(makeMap(from.name,to.name,Nil))

        val rec = from.pars.indices.map(i =>
          (from.pars(i),to.pars(i)) match {
            case (Hasher.Symbol(f),Hasher.Symbol(t)) => get(f,t)
            case (t1,t2) => Some(Map(t1,t2,Nil,0))
          }
        )
        if (rec.forall(_.isDefined)) Some(makeMap(from.name,to.name,rec.map(_.get).toList)) else None
      })
  }


  // finds views between two theories; returns a list of results as List of (GlobalName,GlobalName) pairs
  protected def findViews(th1: Theoryhash, th2: Theoryhash): Set[List[Map]] = {
    def iterate(pairs: List[(Consthash, Consthash)],
                current: (Consthash, Consthash),
                currentPath: List[(GlobalName, GlobalName)])
    : Option[List[(GlobalName, GlobalName)]] = {
      val cp = currentPath
      if (current._1 !<> current._2) None
      else if (current._1.pars == current._2.pars) Some((current._1.name, current._2.name) :: cp)
      else {
        current._1.pars.indices.foldLeft(
          Some((current._1.name, current._2.name) :: cp).asInstanceOf[Option[List[(GlobalName, GlobalName)]]]
        )((pt, i) =>
          (pt,current._1.pars(i),current._2.pars(i)) match {
            case (None,_,_) =>
              None /*
            case Some(path) if path contains ((current._1.pars(i), current._2.pars(i))) =>
              Some(((current._1.pars(i), current._2.pars(i)) :: path).distinct) */
            // case Some(path) if path exists (p => p._1 == current._1.pars(i)) => None
            case (Some(path),Hasher.Symbol(s1),Hasher.Symbol(s2)) =>
              pairs.collectFirst { case p if p._1.name == s1 && p._2.name == s2 => p } match {
              /* case None if s1 == s2 =>
                  Some(path) */
              case None =>
                None
              case Some(x) =>
                iterate(pairs, x, path)
            }
            case _ =>
              pt
          }
        ).map(_.distinct)
      }
    }

    // println(th1.path + " --> " + th2.path)

    implicit val allpairs : List[(Consthash,Consthash)] = potentialMatches(th1.getAll, th2.getAll)
    if (allpairs.isEmpty) {
      // log("No potential matches")
      return Set()
    }
    // log("Potential matches: " + allpairs.length)

    val startingpoints = getStartingPoints
    if (startingpoints.isEmpty) {
      // log("No starting points")
      return Set()
    }
    // log("Starting Points: " + startingpoints.length)
    val ret = startingpoints.indices.map(i => iterate(allpairs, startingpoints(i), List()).getOrElse(Nil)).toSet - Nil
    ret.map { ls =>
      // val frs = ls.map(_._1)
      // val tos = ls.map(_._2)
      // val newfrom = hash.from.find(t => frs.forall(t.getAll.map(_.name).contains)).getOrElse(th1)
      // val newto = hash.to.find(t => tos.forall(t.getAll.map(_.name).contains)).getOrElse(th2)
      ls.map(p => MapStore.get(p._1,p._2).getOrElse{
        // println("Missing: " + p._1 + " --> " + p._2)
        // sys.exit()
        Map(Hasher.Symbol(p._1),Hasher.Symbol(p._2),Nil,0)
      })
    }
  }

  private class InternalView(from : MPath, to : MPath) {
    var maps: List[Map] = Nil

    val validfroms = hash.get(from).map(_.getAll.map(_.name)).getOrElse(???)
    val validtos = hash.get(to).map(_.getAll.map(_.name)).getOrElse(???)

    def copy = {
      val ret = new InternalView(from,to)
      ret.maps = maps
      ret
    }


    def canAdd(newmap: Map): Boolean = {
      if (maps.exists(m => newmap.from == m.from && newmap.to != m.to)) false
      else {
        validfroms.contains(newmap.sfrom) &&
        validtos.contains(newmap.sto) &&
        newmap.requires.forall(canAdd)
      }
    }

    def add(newmap: Map): Unit = {
      maps ::= newmap
      newmap.requires foreach add
    }

    def toView(path : MPath) : View = {
      maps = maps.distinct
      val v = new View(path.parent,path.name,TermContainer(OMMOD(from)),TermContainer(OMMOD(to)),new TermContainer(),false)
      maps.foreach(m => if (m.from != m.to && !v.declares(m.sfrom.name)) v.add(Constant(v.toTerm,ComplexStep(m.sfrom.module)/m.sfrom.name,Nil,None,Some(OMS(m.sto)),None)))
      v
    }
  }

  private def nview(map : Map) = {
    val newview  = new InternalView(map.sfrom.module,map.sto.module)
    newview.add(map)
    newview
  }

  def makeviews(path : MPath, omaps : List[Map]) : List[View] = {
    val imaps = omaps.reverse.filter(_.isSimple).map(_.simple).distinct
    var views : List[InternalView] = Nil
    imaps.indices.foreach {i =>
      val map = imaps(i)
      val canadds = views.filter(_.canAdd(map))
      if (canadds.nonEmpty) canadds.foreach(_.add(map))
      else {
        val prevs = imaps.take(i).filter(_.compatible(map)) ::: map :: Nil
        prevs.collectFirst{case m if nview(m).canAdd(map) => nview(m)} match {
          case Some(s) =>
            val newview = prevs.foldLeft(s) { (v, m) =>
              if (v.canAdd(m)) {
                val w = v.copy
                w.add(m)
                if (w.canAdd(map)) w else v
              } else v
            }
            views ::= newview
          case _ =>
        }
      }
    }
    views.indices.map(i => views(i).toView(path.parent ? (path.name + i.toString))).toList.sortBy(_.getDeclarations.length).reverse
  }
}

case class Map(from : Targetable, to : Targetable, irequires : List[Map], value : Double) {
  val requires = irequires.distinct
  val isSimple = from.isInstanceOf[Hasher.Symbol] && to.isInstanceOf[Hasher.Symbol]
  def asString() = "(" + value + ") " + from + " --> " + to + " requires: " + requires.mkString("["," , ","]")
  def allRequires : List[Map] = (requires.flatMap(_.requires) ::: requires).distinct
  override def toString: String = from + " -> " + to
  def simple : Map = Map(from,to,requires.filter(_.isSimple).map(_.simple),value)

  private[refactoring] def sfrom = from match {
    case Hasher.Symbol(gn) => gn
    case _ => ???
  }
  private[refactoring] def sto = to match {
    case Hasher.Symbol(gn) => gn
    case _ => ???
  }
  private[refactoring] def compatible(that : Map) : Boolean = {
    val these = this :: allRequires
    val those = that :: that.allRequires
    !these.exists(m => those.exists(n => n.from == m.from && n.to != m.to))
  }
}

trait Preprocessor extends Extension { self =>
  var key : String = ""
  var meta : Option[MPath] = None

  def withKey(s : String) = {
    key = s
    this
  }
  def withKey(mp : MPath) = {
    meta = Some(mp)
    this
  }

  def apply(th : Theory) : Theory = {
    val nth = new Theory(th.parent,th.name,th.meta,th.paramC,th.dfC)
    th.getDeclarations foreach {
      case c : FinalConstant =>
        val nc = Constant(nth.toTerm,c.name,c.alias,c.tp map doTerm,c.df map doTerm,c.rl,c.notC)
        nth add nc
      case o => nth add o
    }
    nth
  }

  protected def doTerm(tm : Term) : Term = tm


  def +(that : Preprocessor) : Preprocessor = new Preprocessor {
    override def init(controller: Controller): Unit = {
      that.init(controller)
      self.init(controller)
      super.init(controller)
    }
    override def apply(th : Theory) = that.apply(self.apply(th))
  }
}

object SimpleParameterPreprocessor extends Preprocessor {
  import info.kwarc.mmt.api.objects.Conversions._

  val trav = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case OMV(n) if con.index(n).isEmpty =>
        Hasher.Complex(Hasher.Bind(n))
      case _ => Traverser(this,t)
    }
  }
  override def doTerm(tm: Term): Term = trav(ParseResult.fromTerm(tm).term,())
}

object CovariantParameterPreprocessor extends Preprocessor {
  import info.kwarc.mmt.api.objects.Conversions._

  override def apply(th: Theory): Theory = {
    val nth = new Theory(th.parent,th.name,th.meta,ContextContainer(Nil),TermContainer(None))
    var sub = Substitution()
    th.parameters foreach {v =>
      val c = Constant(nth.toTerm,LocalName("Parameter") / v.name,Nil,v.tp.map(_ ^? sub),v.df.map(_ ^? sub),None,NotationContainer(None))
      nth add c
      sub = Substitution(sub.subs.toList ::: (v.name / OMS(c.path) :: Nil) :_*)
    }
    // val sub = Substitution(th.parameters.map(v => v.name / Hasher.Complex(OMV(v.name))):_*)
    th.getDeclarations foreach {
      case Include(id) /* if id.args.nonEmpty */ => // get rid of parametric includes
        nth.add(Include(nth.toTerm,id.from,Nil))
      case c : FinalConstant if sub.asContext.nonEmpty =>
        val nc = Constant(nth.toTerm,c.name,c.alias,c.tp map(_ ^? sub),c.df map(_ ^? sub),c.rl,c.notC)
        nth add nc
      case o => nth add o
    }
    nth
  }
  // override def doTerm(tm: Term): Term = trav(ParseResult.fromTerm(tm).term,())
}

object DefinitionExpander extends Preprocessor {
  // private val defs : mutable.HashMap[GlobalName,Option[Term]] = mutable.HashMap.empty
  private def getExpanded(gn : GlobalName) : Option[Term] = /* defs.getOrElseUpdate(gn,*/ {
    val c = Try(controller.getAs(classOf[FinalConstant],gn)).toOption
    // println(gn)
    // println(c.flatMap(_.df))
    c.flatMap(_.df.map(traverser(_,())))
  } //)

  private val traverser = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case OMS(gn) =>
        getExpanded(gn).getOrElse(t)
      case _ => Traverser(this,t)
    }
  }

  private lazy val simplifier = controller.simplifier

  override def apply(th: Theory): Theory = {
    val nth = new Theory(th.parent,th.name,th.meta,th.paramC,TermContainer(None))
    th.getDeclarations foreach {
      case Include(id) if id.args.nonEmpty => // get rid of parametric includes
        nth.add(Include(nth.toTerm,id.from,Nil))
      case c : FinalConstant =>
        val ntp = c.tp.map { tp =>
          val ret = traverser(tp, ())
          try {
            simplifier(ret, SimplificationUnit(th.getInnerContext, false,false, true), RuleSet.collectRules(controller, th.getInnerContext))
          } catch {
            case _ : info.kwarc.mmt.api.Error => ret
          }
        }
        nth add c
        ntp match {
          case Some(tm) if !c.tp.contains(tm) =>
            nth add Constant(nth.toTerm,c.name / "defexp",c.alias,Some(tm),c.df,c.rl,c.notC)
          case _ =>
        }
      case o => nth add o
    }
    nth
  }
}

/*
case class FinderResult(from : Theoryhash,to : Theoryhash, entries : List[(GlobalName,GlobalName,Int)], includes : List[FinderResult]) {

  lazy val allentries : List[(GlobalName,GlobalName,Int)] = includes.flatMap(_.allentries) ::: entries

  private def le(that : FinderResult) : Boolean = (that.from.getAllIncludes contains this.from) ||
    ((that.from == this.from) && (this.entries.size < that.entries.size))

  def <(that : FinderResult) : Boolean =
    (this le that) || ((!that.le(this)) && this.value > that.value)

  lazy val value : Long =
    Math.round(allentries.map(_._3).sum / allentries.length)

  def evaluate(eval : (GlobalName,GlobalName) => Int) : FinderResult =
    FinderResult(from,to,entries.map(tr => (tr._1,tr._2,eval(tr._1,tr._2))),includes.map(_.evaluate(eval)))

  def contains(p1 : GlobalName, p2 : GlobalName) : Boolean =
    allentries.exists(tr => tr._1 == p1 && tr._2 == p2)

  private def allIncludes : List[FinderResult] = includes.flatMap(_.allIncludes) ::: includes


  def asString(s : String = "") : String = s + this.hashCode().toString + ": " + from.path + " --> " + to.path + " (" + value + ") {\n" +
    includes.map(_.asString(s + "  ")).mkString("\n") + "\n" +
    entries.map(tr => s + "  " + tr._1.module.name + "?" + tr._1.name + " -> " + tr._2.module.name + "?" + tr._2.name +
      " (" + tr._3 + ")").mkString("\n") + "\n" + s + "}"

  override def toString() = this.hashCode().toString + ": " + from.path + " --> " + to.path + " (" + value + ")" + "\n" +
    includes.map(i => "  include " + i.hashCode().toString).mkString("\n") + "\n" +
    entries.map(tr => "  " + tr._1.module.name + "?" + tr._1.name + " -> " + tr._2.module.name + "?" + tr._2.name +
     " (" + tr._3 + ")").mkString("\n")

  private def pairs : List[(GlobalName,GlobalName)] = entries.map(tr => (tr._1,tr._2))

  override def equals(obj: scala.Any): Boolean = obj match {
    case that : FinderResult => this.from == that.from && this.to == that.to &&
      this.pairs == that.pairs
    case _ => false
  }
}
*/