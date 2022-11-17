package info.kwarc.mmt.stex

import info.kwarc.mmt.api.{Error, Level}
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.stex.STeXUtils._

/**
  * Monoidal accumulator for SMS content and dependencies.
  * @param smslines SMS content (List[String])
  * @param deps Collected dependencies (List[Dependency])
  */
case class STeXStructure(smslines: List[String], deps: List[Dependency])
{
  def empty : STeXStructure = STeXStructure(List.empty,List.empty)

  def <>(that : STeXStructure) : STeXStructure =
    if (that.smslines.isEmpty && that.deps.isEmpty) {
      this
    } else STeXStructure(that.smslines ++ smslines, that.deps ++ deps)
}

/**
  * Created by maeder on 11.11.15.
  */
trait STeXAnalysis {
  self: TraversingBuildTarget =>

  protected def logSuccess(f: FilePath): Unit = {
    logResult("success " + f)
  }

  protected def logFailure(f: FilePath): Unit = {
    logResult("failure " + f)
  }

  def mkDep(a: Archive, ar: String, fp: FilePath): List[Dependency] = {
    val root = a.root.up.up / ar
    controller.addArchive(root)
    controller.backend.getArchive(root) match {
      case None =>
        log("missing archive " + ar + " for " + fp)
        Nil
      case Some(arch) => List(mkFileDep(arch, fp))
    }
  }

  def mkFileDep(archive: Archive, filePath: FilePath): Dependency =
    FileBuildDependency("tex-deps", archive, filePath)

  def mhRepos(a: Archive, r: String, b: String): List[Dependency] = {
    val fp = entryToPath(b)
    val optRepo = Option(r).map(getArgMap)
    optRepo match {
      case Some(m) => mkDep(a, m.getOrElse("mhrepos", archString(a)), fp)
      case None => List(mkFileDep(a, fp))
    }
  }

  def mhReposClosure(archive: Archive, parents : Set[File], r : String, b : String) : List[Dependency] =
  {
    // Single dependency
    val nu_dep : List[Dependency] = mhRepos(archive, r, b)

    // Find distant archive given by r.
    val distantArchive : Archive = Option(r).map(getArgMap) match
    {
      case Some(m) =>
        // Compare mkDep
        assert(m.contains("mhrepos"))
        val root = archive.root.up.up / m("mhrepos")
        controller.addArchive(root)
        val thearchive = controller.backend.getArchive(root)
        if (thearchive.isDefined) {
          thearchive.get
        } else {
          throw new STeXLookupError(msg = "missing archive: " + root, None, severity = Some(Level.Error))
        }

      case None => archive // fallback
    }

    val theFile : File = if (archive == distantArchive) { entryToPath(b).toFile }
                         else { (distantArchive.root / "source" / b).setExtension("tex") }

    val closure : List[Dependency] = getDeps(distantArchive,theFile,parents + theFile)
    nu_dep ::: closure
  }

  protected def toKeyDep(d: Dependency, key: String): Dependency = d match {
    case FileBuildDependency(_, ar, fp) => FileBuildDependency(key, ar, fp)
    case fd => fd
  }

  protected def matchPathAndRepo(archive: Archive, inFile: File, line: String, parents: Set[File]) : List[Dependency] =
  {
    line match
    {
      case beginModnl(_, _, b) => List(mkFileDep(archive, entryToPath(b)))

      // The next four take relative paths so it's okay not to include that.
      case input(_, _, _, b) =>
        // As below with slight alterations for libinputs.
        val p = if (line.startsWith("\\lib"))
          STeXUtils.getAmbleFile(b + ".tex", archive, None)
        else (inFile.up / b).setExtension("tex")
        val d = PhysicalDependency(p)
        d :: (if (!parents.contains(p)) getDeps(archive, p, parents + p) else Nil)

      case includeAssignment(_,_,b) =>
        // "a13" -> File(a13.tex)
        val pfile : File = (inFile.up / b).setExtension("tex")
        // The including file naturally physically depends on the included file.
        val pdep : Dependency = PhysicalDependency(pfile)
        // All dependencies of the included file are now also dependencies of the including file.
        if (!parents.contains(pfile)) {
          pdep :: getDeps(archive, pfile, parents + pfile)
        } else { List(pdep) }

      case inputAssignment(_,_,b) =>
        // As above.
        val pfile : File       = (inFile.up / b).setExtension("tex")
        val pdep  : Dependency = PhysicalDependency(pfile)
        if (!parents.contains(pfile)) {
          pdep :: getDeps(archive, pfile, parents + pfile)
        } else { List(pdep) }

      case includeProblem(_,_,b) =>
        // As above.
        val pfile : File       = (inFile.up / b).setExtension("tex")
        val pdep  : Dependency = PhysicalDependency(pfile)
        if (!parents.contains(pfile)) {
          pdep :: getDeps(archive, pfile, parents + pfile)
        } else { List(pdep) }

      case includeGraphics(_, _, b) =>
        val gExts = List("png", "jpg", "eps", "pdf")
        val p = inFile.up / b
        if (p.getExtension.exists(gExts.contains)) List(PhysicalDependency(p))
        else {
          val os = gExts.find { s =>
            p.setExtension(s).exists
          }
          if (os.isEmpty) log(inFile.toString + " misses graphics file: " + b)
          os.toList.map(s => PhysicalDependency(p.setExtension(s)))
        }

      case importOrUseModule(r) =>
        getArgMap(r).get("load").map(f => PhysicalDependency(File(f).setExtension(".sms"))).toList

      case mhinputRef(_, r, b) =>
        val fp = entryToPath(b)
        val alldeps = getDeps(archive,fp.toFile,Set.empty)

        Option(r) match {
          case Some(id) => mkDep(archive, id, fp)   ::: alldeps
          case None => List(mkFileDep(archive, fp)) ::: alldeps
        }

      case tikzinput(_, r, b) => mhRepos(archive, r, b).map(toKeyDep(_, key = "tikzsvg"))

      case guse(r, b) => mkDep(archive, r, entryToPath(b))

      // These three are supposed to work exactly alike, see https://github.com/UniFormal/MMT/issues/465.
      case inputMhAssignment(_,r,b)   => mhReposClosure(archive, parents, r, b)
      case includeMhAssignment(_,r,b) => mhReposClosure(archive, parents, r, b)
      case includeMhProblem(_, r, b)  => mhReposClosure(archive, parents, r, b)

      case _ => Nil
    }
  }

  private def mkImport(a: Archive, r: String, p: String, s: String, ext: String): String =
    "\\importmodule[load=" + a.root.up.up + "/" + r + "/source/" + p + ",ext=" + ext + "]" + s + "%"

  private def mkMhImport(a: Archive, r: String, p: String, s: String) : STeXStructure = {
    STeXStructure(List(mkImport(a, r, p, s, "sms")), mkDep(a, r, entryToPath(p)).map(toKeyDep(_, "sms")))
  }

  private def mkGImport(a: Archive, r: String, p: String): STeXStructure =
    STeXStructure(List(mkImport(a, r, p, "{" + p + "}", "tex"), "\\mhcurrentrepos{" + r + "}%"),
      mkDep(a, r, entryToPath(p)))

  // line list is reversed!

  private def splitArgs(r: String): List[List[String]] = r.split(",").toList.map(_.split("=").toList)

  private def mkArgMap(ll: List[List[String]]): Map[String, String] = {
    var m: Map[String, String] = Map.empty
    ll.foreach {
      case List(k, v) => m += ((k.trim, v.trim))
      case _ =>
    }
    m
  }

  private def getArgMap(r: String): Map[String, String] = mkArgMap(splitArgs(r))

  private def createMhImport(a: Archive, r: String, b: String) : List[STeXStructure] = {
    val m = getArgMap(r)
    val repos : String = m.getOrElse("mhrepos", m.getOrElse("repos", archString(a)))

    val result : List[STeXStructure] = if (m.isDefinedAt("dir")) {
      /* \importmhmodule[dir=foo]{bar} is supposed to be the same as \importmhmodule[path=foo/bar]{bar}
         as per https://github.com/UniFormal/MMT/issues/499. */

      def addb(p : String) : String = {
        assert(b.startsWith("{") && b.endsWith("}"))
        p + java.io.File.separator + b.tail.init
      }
      m.get("dir").toList.map(p => mkMhImport(a, repos, addb(p), b))
    } else if (m.isDefinedAt("path")) {
      m.get("path").toList.map(p => mkMhImport(a, repos, p, b))
    } else {
      List.empty
    }
    result
  }

  private def createGImport(a: Archive, r: String, p: String): STeXStructure = {
      Option(r) match {
        case Some(id) => mkGImport(a, id, p)
        case None     => mkGImport(a, archString(a), p)
    }
  }

  private def createImport(r: String, p: String): STeXStructure =
    STeXStructure(List("\\importmodule[" + r + "]{" + p + "}%"), Nil)

  /** Collect sms content and write to outFile. */
  def createSms(a: Archive, inFile: File, outFile: File) : Unit = {
    val smsContent = mkSTeXStructure(a, inFile, readSourceRebust(inFile).getLines(), Set.empty).smslines
    if (smsContent.isEmpty) {log("no sms content")}
    else { File.write(outFile, smsContent.reverse.mkString("", "\n", "\n")) }
  }

  /** get dependencies */
  def getDeps(a: Archive, in: File, parents: Set[File], amble: Option[File] = None): List[Dependency] = {
    val f = amble.getOrElse(in)
    if (f.exists) {
      val struct = mkSTeXStructure(a, in, readSourceRebust(f).getLines(), parents)
      struct.deps
    } else { Nil }
  }

  /** Method for creating STeXStructures, used for generating sms content and finding dependencies.
    * inFile is used for relative \input paths */
  def mkSTeXStructure(a: Archive, in: File, lines: Iterator[String], parents: Set[File]): STeXStructure =
  {
    var localStruct = STeXStructure(Nil,Nil)
    def combine(s: STeXStructure) : Unit = { localStruct = localStruct <> s }

    lines.foreach { line =>
      val l = stripComment(line).trim
      val isVerbatim = l.contains("\\verb")
      if (!isVerbatim)
      {
        val sl = matchSmsEntry(a, l)
        sl.foreach(combine)
        if (key != "sms")
        {
          val od  : List[Dependency] = matchPathAndRepo(a, in, l, parents)
          // Forget all tikzsvg dependencies unless we're inside a latexml target.
          val odp : List[Dependency] = if (key == "latexml") { od } else { od.filter {
            case FileBuildDependency(akey, _, _) => akey != "tikzsvg"
            case _ => true
          }}
          odp.foreach(d => combine(STeXStructure(Nil, List(d))))
        }
      }
    }
    localStruct
  }

  def matchSmsEntry(a: Archive, line: String): List[STeXStructure] =
  {
    line match
    {
      case importMhModule(r, b) =>
        createMhImport(a, r, b)

      case useMhModule(opt,t) =>
        val argmap = getArgMap(opt)

        if (argmap.contains("path") || argmap.contains("dir")) {
          val repos = argmap.getOrElse("mhrepos", argmap.getOrElse("repos", archString(a)))
          val entry = if (argmap.contains("dir")) { argmap("dir") + java.io.File.separator + t } else { argmap("path") }
          val deps  = mkDep(a,repos,entryToPath(entry))
          assert(deps.length == 1)

          List(STeXStructure(Nil,List(toKeyDep(deps.head,key = "sms"))))
        } else { Nil }

      case gimport(_, r, p) =>
        List(createGImport(a, r, p))

      case guse(opt,k) =>

        val argmap = getArgMap(opt)
        val repos = argmap.getOrElse("mhrepos", argmap.getOrElse("repos", archString(a)))

        if (argmap.contains("path")) {
          val deps = mkDep(a,repos,entryToPath(argmap("path")))
          assert(deps.length == 1)
          List(STeXStructure(Nil,List(toKeyDep(deps.head,key = "sms"))))
        } else { Nil }

      case smsGStruct(_, r, _, p) =>
        List(createGImport(a, r, p))

      case smsMhStruct(r, _, p) =>
        createMhImport(a, r, p)

      case smsSStruct(r, _, p) =>
        List(createImport(r, p))

      case smsViewsig(r, _, f, t) =>
        val m = getArgMap(r)
        val fr = m.getOrElse("fromrepos", archString(a))
        val tr = m.getOrElse("torepos", archString(a))
        List(mkGImport(a, fr, f), mkGImport(a, tr, t))

      case smsViewnl(_, r, p) =>
        List(createGImport(a, archString(a), p))

      case smsMhView(r, _, f, t) =>
        val m = getArgMap(r)
        val ofp = m.get("frompath")
        val otp = m.get("topath")
        val fr = m.getOrElse("fromrepos", archString(a))
        val tr = m.getOrElse("torepos", archString(a))
        (ofp, otp) match {
          case (Some(fp), Some(tp)) =>
            List(mkMhImport(a, fr, fp, f), mkMhImport(a, tr, tp, t))
          case _ => Nil
        }

      case smsView(r, f, t) =>
        val m = getArgMap(r)
        val ofr = m.get("from")
        val otr = m.get("to")
        (ofr, otr) match {
          case (Some(fr), Some(tr)) =>
            List(createImport(fr, f), createImport(tr, t))
          case _ => Nil
        }

      case _ if smsRegs.findFirstIn(line).isDefined => List(STeXStructure(List(line + "%"), Nil))

      case _ => Nil
    }
  }
}
