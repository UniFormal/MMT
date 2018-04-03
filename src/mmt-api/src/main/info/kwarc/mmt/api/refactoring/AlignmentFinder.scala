package info.kwarc.mmt.api.refactoring

import java.util.concurrent.Executors

import info.kwarc.mmt.api.frontend.{Logger, Report}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.FormalAlignment
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.utils.time.Time

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class FinderConfig(finder : AlignmentFinder, protected val report : Report) extends Logger {
  override def logPrefix: String = "viewfinder"

  private var fromTheories_var : List[DeclaredTheory] = Nil
  private var toTheories_var : List[DeclaredTheory] = Nil
  private var commonTheories_var : List[DeclaredTheory] = Nil
  private var fixing_var : List[AlignmentTranslation] = Nil
  private var judg1_var: Option[GlobalName] = None
  private var judg2_var: Option[GlobalName] = None
  private var doDefs_var : Boolean = false
  private var minimal_parameter_length_var = 0
  private var multithreaded : Boolean = false

  def fromTheories : List[DeclaredTheory] = fromTheories_var
  def toTheories : List[DeclaredTheory] = toTheories_var
  def commonTheories : List[DeclaredTheory] = commonTheories_var
  def fixing : List[AlignmentTranslation] = fixing_var
  def judg1: Option[GlobalName] = judg1_var
  def judg2: Option[GlobalName] = judg2_var
  def doDefs : Boolean = doDefs_var
  def minimal_parameter_length = minimal_parameter_length_var
  def isMultithreaded = multithreaded

  def setMultithreaded(b : Boolean) = multithreaded = b

  def addFrom(th : List[DeclaredTheory]) = {
    fromTheories_var :::= th
    val cs = fromTheories_var.filter(t => toTheories_var.contains(t) || commonTheories_var.contains(t))
    commonTheories_var :::= cs
    fromTheories_var = fromTheories_var.filterNot(commonTheories_var.contains).distinct
    toTheories_var = toTheories_var.filterNot(commonTheories_var.contains).distinct
    commonTheories_var = commonTheories_var.distinct
  }
  def addTo(th : List[DeclaredTheory]) = {
    toTheories_var :::= th
    val cs = fromTheories_var.filter(t => toTheories_var.contains(t) || commonTheories_var.contains(t))
    commonTheories_var :::= cs
    fromTheories_var = fromTheories_var.filterNot(commonTheories_var.contains).distinct
    toTheories_var = toTheories_var.filterNot(commonTheories_var.contains).distinct
    commonTheories_var = commonTheories_var.distinct
  }
  private def addAlignments(al : List[FormalAlignment]) = {
    fixing_var :::= al.map(finder.makeAlignment)
  }

  // val translations = fixing.map(AlignmentTranslation.apply(_)(controller))

  def addJudgmentFrom(gn : GlobalName) = judg1_var = Some(gn)
  def addJudgmentTo(gn : GlobalName) = judg2_var = Some(gn)

  def setDoDefs(b : Boolean) = doDefs_var = b
  def setMinParLength(i : Int) = minimal_parameter_length_var = i

  def findJudgments = {
    log("finding Judgments...")
    val judg1 = if (judg1_var.isDefined) judg1_var else finder.getJudgment(fromTheories ::: commonTheories)
    val judg2 = if (judg2_var.isDefined) judg2_var else finder.getJudgment(toTheories ::: commonTheories)
    log("Judgments are " + judg1 + " and " + judg2)
    judg1.foreach(addJudgmentFrom)
    judg2.foreach(addJudgmentTo)
  }
}

class AlignmentFinder extends frontend.Extension {
  override def logPrefix: String = "viewfinder"
  implicit private val ec = ExecutionContext.global //.fromExecutor(Executors.newFixedThreadPool(10000))

  private[refactoring] def getJudgment(ths: List[DeclaredTheory]): Option[GlobalName] = {
    var i = 0
    var judg: Option[GlobalName] = None
    while (judg.isEmpty && i < ths.length) {
      judg = findJudgment(ths(i))
      i += 1
    }
    judg
  }

  private[refactoring] def makeAlignment(al : FormalAlignment) =
    AlignmentTranslation(al)(controller)

  private def findJudgment(th:DeclaredTheory):Option[GlobalName] = {

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

    // controller.simplifier.apply(th)
    // val ths = th.getIncludes.map(controller.get(_).asInstanceOf[DeclaredTheory])//closer.getIncludes(th,true)
    findJudgmentIt(th)
  }

  def getConfig : FinderConfig = new FinderConfig(this,report)

  def getConfig(a1 : Archive, a2 : Archive) : FinderConfig = {
    val cfg = getConfig
    var dones : mutable.HashMap[MPath,Option[List[DeclaredTheory]]] = mutable.HashMap.empty

    // guarantees that the dependency closure is ordered
    def flatten(mp : MPath) : Option[List[DeclaredTheory]] = {
      dones.getOrElse(mp, {
        log("Doing: " + mp.toString)
        // println(mp)
        val ret = controller.getO(mp) match {
          case Some(th : DeclaredTheory) =>
            val rec = th.getIncludes.map(flatten)
            if (rec.contains(None)) None else
              Some((rec.flatMap(_.get) ::: List(th)).distinct)
          case _ => None
        }
        dones += ((mp,ret))
        ret
      })
    }

    var lf = a1.root / "viewfinder_order"
    if (lf.exists) {
      log("Reading file " + lf)
      val ls = File.read(lf).split("\n").toList.distinct
      val mps = ls.map(s => Path.parseM(s,NamespaceMap.empty))
      log("Adding " + mps.length + " theories...")
      cfg.addFrom(mps.indices.map{i =>
        print("\r  " + (i + 1) + " of " + mps.indices.length)
        controller.getAs(classOf[DeclaredTheory],mps(i))
      }.toList)
      println(" Done.")
    } else {
      log("Collecting theories in " + a1.id)
      val ths = a1.allContent.flatMap(flatten).flatten
      File.write(lf,ths.map(_.path.toString).distinct.mkString("\n"))
      cfg.addFrom(ths)
      dones.clear()
    }

    lf = a2.root / "viewfinder_order"
    if (lf.exists) {
      log("Reading file " + lf)
      val ls = File.read(lf).split("\n").toList.distinct
      val mps = ls.map(s => Path.parseM(s,NamespaceMap.empty))
      log("Adding " + mps.length + " theories...")
      cfg.addTo(mps.indices.map{i =>
        print("\r  " + (i + 1) + " of " + mps.indices.length)
        controller.getAs(classOf[DeclaredTheory],mps(i))
      }.toList)
      println(" Done.")
    } else {
      log("Collecting theories in " + a2.id)
      val ths = a2.allContent.flatMap(flatten).flatten
      File.write(lf,ths.map(_.path.toString).distinct.mkString("\n"))
      cfg.addTo(ths)
      dones.clear()
    }

    cfg
  }

/*
  def getConfigMultithreaded(a1: Archive, a2: Archive) : (Long,FinderConfig) = Time.measure {
    val cfg = getConfig

    object Dones {
      private var dones : mutable.HashMap[MPath,Future[List[DeclaredTheory]]] = mutable.HashMap.empty
      private def flattenIn(mp : MPath) : Future[List[DeclaredTheory]] = dones.get(mp) match {
        case None =>
          val f = Future {
            log("Doing: " + mp.toString)
            controller.getO(mp) match {
              case Some(th: DeclaredTheory) =>
                val rec = Future.sequence(th.getIncludes.map(flattenIn))
                rec.map { ls =>
                  val r = (ls.flatten ::: List(th)).distinct
                  log("DONE: " + mp.toString)
                  r
                }(ec)
              // Await.result(rec,Duration.Inf).flatten ::: List(th).distinct
              case _ => Future(Nil)
            }
          }(ec).flatten
          synchronized(dones += ((mp, f)))
          f
        case Some(f) =>
          f
      }

      def flatten(mp : MPath) : Unit = {
        flattenIn(mp)
      }


      private def finished = dones.values.forall(_.isCompleted)

      def getAll : List[DeclaredTheory] = {
        while(!finished) {
          // log("Waiting...")
          Thread.sleep(1000)
        }
        dones.values.flatMap(_.value.get.get).toList
      }
      def reset = dones.clear()
    }

    log("Collecting theories in " + a1.id)
    a1.allContent.foreach(Dones.flatten)
    cfg.addFrom(Dones.getAll)

    Dones.reset
    log("Collecting theories in " + a2.id)
    a2.allContent.foreach(Dones.flatten)
    cfg.addTo(Dones.getAll)

    cfg
  }
  */

  def run(cfg : FinderConfig) = {
    val proc = new FindingProcess(this.report,cfg)
    log("Multithreaded: " + cfg.isMultithreaded)
    proc.hash.run
    proc.run
  }

}

class FindingProcess(val report : Report, cfg : FinderConfig) extends MMTTask with Logger {
  override def logPrefix: String = "viewfinder"
  implicit private val ec = ExecutionContext.global //.fromExecutor(Executors.newFixedThreadPool(10000))

  import cfg._

  trait Hasher {
    // starts the hashing process
    protected def irun: Unit
    def run {
      log("Compute hashes... ")
      val (t,_) = Time.measure(irun)
      log("Hashing done after " + t + "ms.")
    }

    def from : List[Theoryhash]
    def to : List[Theoryhash]
  }

  val hash : Hasher = if (cfg.isMultithreaded) HashesMultithreaded else HashesNormal

  private object HashesNormal extends Hasher {
    private val theories : scala.collection.mutable.HashMap[MPath,Theoryhash] = mutable.HashMap()
    lazy val alltheories = commonTheories ::: fromTheories ::: toTheories

    // def getTheory(th : DeclaredTheory) : Theoryhash = theories.getOrElse(th.path,get(th))

    private def getTheory(p : MPath) : Theoryhash = theories.getOrElse(p, {
      get(alltheories.find(_.path == p).getOrElse{
        println(p)
        ???
      }) // TODO shouldn't happen though
    })

    private def get(th : DeclaredTheory) : Theoryhash = {
      val h = new Theoryhash(th.path)
      // log("Hashing " + th.path)
      if (!(commonTheories contains th)) {
        th.getConstants.collect({
          case c : FinalConstant
            if !cfg.fixing.exists(a => a.alignment.from.mmturi == c.path || a.alignment.to.mmturi == c.path) => c
        }) foreach (c =>
          /* if (!alignments.exists(a => a.from.mmturi == c.path || a.to.mmturi == c.path)) */ h.addConstant(doConstant(c))
          )
        th.getIncludes.foreach(t => h.addInclude(getTheory(t)))
      }
      h.init
      theories += ((th.path,h))
      h
    }

    private lazy val numbers = {
      var inner : List[(GlobalName,Int)] = Nil
      commonTheories.foreach { th =>
        th.getConstants.foreach(c => inner ::= (c.path, inner.length + 1))
      }
      fixing.foreach { a =>
        a.alignment.to.mmturi match {
          case gn : GlobalName => (gn,inner.length + 1)
        }
      }
      inner
    }

    private def doConstant(c:FinalConstant) : Consthash = {
      var isAxiom = false
      var pars : List[GlobalName] = Nil

      def traverse(t: Term)(implicit vars : List[LocalName]) : List[Int] = {
        // assumption: at most one alignment applicable
        val al = fixing.find(_.applicable(t))
        val tm = al.map(_.apply(t)).getOrElse(t)
        tm match {
          case OMV(name) =>
            List(0, 2 * vars.indexOf(name))
          case OMS(path) =>
            if (judg1.contains(path) || judg2.contains(path)) isAxiom = true
            val nopt = numbers.find(_._1 == path)
            val (i,j) = nopt match {
              case Some((_,n)) => (0,n)
              case _ => (1,if (pars.contains(path)) pars.length - (pars.indexOf(path)+1) else {
                pars ::= path
                pars.length-1
              })
            }
            List(1,i,j)
          case OMA(f, args) =>
            2 :: args.length :: traverse(f) ::: args.flatMap(traverse)
          case OMBINDC(f, con, bds) =>
            val (cont, newvars) = con.foldLeft((List(3, con.length), vars))((p, v) =>
              (p._1 ::: v.tp.map(traverse(_)(p._2)).getOrElse(List(-1)), v.name :: p._2))
            cont ::: List(bds.length) ::: bds.flatMap(traverse(_)(newvars))
          case OMLIT(value, rt) =>
            4 :: value.hashCode :: traverse(rt.synType)
          case UnknownOMLIT(s, tp) =>
            5 :: s.hashCode :: traverse(tp)
          case OML(name, tp, df, _,_) =>
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

    // starts the hashing process
    def irun: Unit = {
      alltheories.indices.foreach(i => {
        print("\r  " + (i + 1) + " of " + alltheories.length)
        get(alltheories(i))
      })
      println(" Done.")
    }

    lazy val from = fromTheories.map(t => getTheory(t.path))
    lazy val to = toTheories.map(t => getTheory(t.path))
  }

  private object HashesMultithreaded extends Hasher {
    private val theories: scala.collection.mutable.HashMap[MPath, Future[Theoryhash]] = mutable.HashMap()

    // def getTheory(th : DeclaredTheory) : Theoryhash = theories.getOrElse(th.path,get(th))

    private def getTheory(p: MPath): Future[Theoryhash] = theories.getOrElse(p, {
      get((commonTheories ::: fromTheories ::: toTheories).find(_.path == p).getOrElse {
        println(p)
        ???
      }) // TODO shouldn't happen though
    })

    private def get(th: DeclaredTheory): Future[Theoryhash] = {
      val f = Future {
        // log("Hashing " + th.path)
        val h = new Theoryhash(th.path)
        if (!(commonTheories contains th)) {
          th.getConstants.collect({
            case c: FinalConstant
              if !cfg.fixing.exists(a => a.alignment.from.mmturi == c.path || a.alignment.to.mmturi == c.path) => c
          }) foreach (c =>
            /* if (!alignments.exists(a => a.from.mmturi == c.path || a.to.mmturi == c.path)) */ h.addConstant(doConstant(c))
            )
          val rec = Future.sequence(th.getIncludes.map(getTheory))
          rec.foreach { ls =>
            ls.foreach(h.addInclude)
            h.init
          }
        }
        h
      }
      synchronized(theories += ((th.path, f)))
      f
    }

    private lazy val numbers = synchronized {
      var inner: List[(GlobalName, Int)] = Nil
      commonTheories.foreach { th =>
        th.getConstants.foreach(c => inner ::= (c.path, inner.length + 1))
      }
      fixing.foreach { a =>
        a.alignment.to.mmturi match {
          case gn: GlobalName => (gn, inner.length + 1)
        }
      }
      inner
    }

    private def doConstant(c: FinalConstant): Consthash = {
      var isAxiom = false
      var pars: List[GlobalName] = Nil

      def traverse(t: Term)(implicit vars: List[LocalName]): List[Int] = {
        // assumption: at most one alignment applicable
        val al = fixing.find(_.applicable(t))
        val tm = al.map(_.apply(t)).getOrElse(t)
        tm match {
          case OMV(name) =>
            List(0, 2 * vars.indexOf(name))
          case OMS(path) =>
            if (judg1.contains(path) || judg2.contains(path)) isAxiom = true
            val nopt = numbers.find(_._1 == path)
            val (i, j) = nopt match {
              case Some((_, n)) => (0, n)
              case _ => (1, if (pars.contains(path)) pars.length - (pars.indexOf(path) + 1) else {
                pars ::= path
                pars.length - 1
              })
            }
            List(1, i, j)
          case OMA(f, args) =>
            2 :: args.length :: traverse(f) ::: args.flatMap(traverse)
          case OMBINDC(f, con, bds) =>
            val (cont, newvars) = con.foldLeft((List(3, con.length), vars))((p, v) =>
              (p._1 ::: v.tp.map(traverse(_)(p._2)).getOrElse(List(-1)), v.name :: p._2))
            cont ::: List(bds.length) ::: bds.flatMap(traverse(_)(newvars))
          case OMLIT(value, rt) =>
            4 :: value.hashCode :: traverse(rt.synType)
          case UnknownOMLIT(s, tp) =>
            5 :: s.hashCode :: traverse(tp)
          case OML(name, tp, df, _, _) =>
            6 :: tp.map(traverse).getOrElse(List(-1)) ::: df.map(traverse).getOrElse(List(-1))
          case tm =>
            println("Missing: " + tm.getClass)
            ???
        }
      }

      val hash = c.tp.map(traverse(_)(Nil)).getOrElse(Nil).asInstanceOf[List[Any]]
      val tppars = pars
      val optDef = if (doDefs) {
        pars = Nil
        c.df.map(traverse(_)(Nil))
      } else None
      Consthash(c.path, hash, tppars, isAxiom, optDef.map(d => (d.hashCode(), pars)))
    }

    // starts the hashing process
    def irun: Unit = {
      (commonTheories ::: fromTheories ::: toTheories).foreach(t => getTheory(t.path))
      while (!theories.values.forall(_.isCompleted)) {
        // log("Waiting...")
        Thread.sleep(500)
      }
    }

    lazy val from: List[Theoryhash] = fromTheories.map(t => getTheory(t.path).value.get.get)
    lazy val to: List[Theoryhash] = toTheories.map(t => getTheory(t.path).value.get.get)
  }


  def run = {
    log("Selecting Theories to use...")
    val (t1,tops1) = Time.measure(select(hash.from))
    val (t2,tops2) = Time.measure(select(hash.to))
    log("Done after " + (t1 + t2) + "ms")

    log(tops1.length + " and " + tops2.length + " selected elements")
    log("Finding Views...")
    val (t,alls : Set[List[Map]]) = Time.measure(if (cfg.isMultithreaded) {
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
        val ret = findViews(t1, t2)
        // log("   " + t1.path.name + " -> " + t2.path.name + ": " + ret.size + " Results.")
        ret
      })
    }.toSet)
    log("Done after " + t + "ms")

    log("Postprocessing...")
    val (tlast,res) = Time.measure(postProcess(alls))
    log("Done after " + tlast + "ms")
    log(res.length + " maps found.")
    res
  }

  // selects which theories to use. By default only maximal theories
  private def select(theories: List[Theoryhash]) : List[Theoryhash] = theories.indices.collect {
    case i if !theories.drop(i + 1).exists(p => p.getAllIncludes.contains(theories(i))) => theories(i)
  }.toList

  // the method used for matching two constants. Defaults to hash equality with all parameters pairwise matched up
  protected def matches(c: Consthash, d: Consthash)(l: List[(GlobalName, GlobalName)]): Boolean = {
    if (c !<> d) false
    else (c.pars zip d.pars).forall { case (p, q) => p == q || l.contains((p, q)) }
  }

  //selects potential matches to work with; defaults to hash-equality
  protected def potentialMatches(t1: List[Consthash], t2: List[Consthash])
  = t1.flatMap(c => t2 collect { case d if c <> d => (c, d) })

  // gets starting points for viewfinding. Defaults to Axioms and minimal_parameter_length
  protected def getStartingPoints(implicit allpairs : List[(Consthash, Consthash)]): List[(Consthash, Consthash)]
  = (if (judg1.isDefined || judg2.isDefined) allpairs.filter(p => p._1.isProp || p._2.isProp)
  else allpairs).filter(p => p._1.pars.length >= minimal_parameter_length)

  // does something with the resulting lists of pairs.

  protected def postProcess (views: Set[List[Map]]) = {
    log("  Evaluating...")
    val evals = views.toList.flatMap(_.map(m => Map(m.from,m.to,m.requires,
      views.count(_.contains(Map(m.from,m.to,m.requires,0))).toDouble * 100.0 / views.size.toDouble)))
    /*
    val evals = views.par.map(_.evaluate((a,b) =>
      (views.count(_.entries.contains((a,b,0))).toDouble * 100.0 / views.size.toDouble).toInt)) */

    /* TODO optimize or throw away
    log("  Doing complex stuff...")
    var dones : List[FinderResult] = Nil
    // var remains = evals.toList
    def iterate(i : Int, remains : List[FinderResult]) : Unit = {
      if (i == hash.from.length) return ()
      val th = hash.from(i)
      print("\r  " + i + " of " + hash.from.length)

      var allpairs : List[(GlobalName,GlobalName,Int)] = Nil
      var paired = remains.map(fr => {
        val these = fr.entries.filter(_._1.module == th.path)
        allpairs :::= these
        (FinderResult(fr.from,fr.to,fr.entries.filterNot(these.contains),fr.includes),these,Nil.asInstanceOf[List[(GlobalName,GlobalName,Int)]])
      })
      allpairs = allpairs.distinct
      val inorder = th.getLocal.map(_.name).filter(p => allpairs.exists(_._1 == p))
      def split(ps : List[(GlobalName,GlobalName,Int)]) = {
        paired = ps.flatMap {p =>
          paired.flatMap{tr => if (tr._2 contains p) {
            Some((tr._1,tr._2,p :: tr._3))
          } else if (tr._2.exists(_._1 == p._1)) {
            None
          } else Some((tr._1,tr._2,p :: tr._3))
          }
        }
      }
      inorder.map(p => allpairs.filter(_._1 == p)).foreach(split)
      val npaired = paired.map{ tr =>
        val tos = tr._3.map(_._2)
        val to = hash.to.find(t => tos.forall(t.getAll.map(_.name).contains)).getOrElse(tr._1.to)
        val v = FinderResult(th, to,tr._3,tr._1.includes)
        if (v.entries.isEmpty) (tr._1,v) else
        (FinderResult(tr._1.from,tr._1.to,tr._1.entries,List(v)),v)
      }
      val nviews = npaired.map(_._2).distinct
      synchronized(dones :::= nviews.filter(_.entries.nonEmpty))
      if (nviews.isEmpty) iterate(i+1,remains)
      else nviews.map(v => npaired.filter(_._2 == v).map(_._1)).par.foreach(iterate(i+1,_))

    }
    iterate(0,evals.toList)
    dones.reverse
    */
    val order = hash.from.flatMap(_.getLocal.reverse)
    evals.view.distinct.sortBy(v => (order.indexWhere(_.name == v.from),-v.value))
  }

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

  private object MapStore {
    private var maps: mutable.HashMap[(GlobalName, GlobalName),Option[Map]] = mutable.HashMap.empty

    def get(n1 : GlobalName,n2 : GlobalName)(implicit allpairs : List[(Consthash, Consthash)]) : Option[Map] =
      maps.getOrElse((n1,n2),{
        allpairs.collectFirst { case p if p._1.name == n1 && p._2.name == n2 => p } match {
          case None =>
            maps((n1,n2)) = None
            None
          case Some((f,t)) => get(f, t)
        }
      })

    def get(from : Consthash, to : Consthash)(implicit allpairs : List[(Consthash, Consthash)]) : Option[Map] =
      maps.getOrElseUpdate((from.name,to.name), {
        if (from !<> to) return None
        if (from.pars == to.pars) return Some(Map(from.name, to.name, Nil, 0))

        val rec = from.pars.indices.map(i => get(from.pars(i), to.pars(i)))
        if (rec.forall(_.isDefined)) Some(Map(from.name,to.name,rec.map(_.get).toList,0)) else None
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
          pt match {
            case None => None /*
            case Some(path) if path contains ((current._1.pars(i), current._2.pars(i))) =>
              Some(((current._1.pars(i), current._2.pars(i)) :: path).distinct) */
            // case Some(path) if path exists (p => p._1 == current._1.pars(i)) => None
            case Some(path) => pairs.collectFirst { case p if p._1.name == current._1.pars(i) && p._2.name == current._2.pars(i) => p } match {
              case None => None
              case Some(x) => iterate(pairs, x, path)
            }
          }
        ).map(_.distinct)
      }
    }

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
      makeMaximal(ls).map(p => MapStore.get(p._1,p._2).get)
    }
  }

}

case class Map(from : GlobalName, to : GlobalName, requires : List[Map], value : Double) {
  def asString() = "(" + value + ") " + from + " --> " + to + " requires: " + requires.mkString("["," , ","]")
  def allRequires : List[Map] = (requires.flatMap(_.requires) ::: requires).distinct
  override def toString: String = from + " -> " + to
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