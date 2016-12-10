package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import archives._
import info.kwarc.mmt.api.utils.AnaArgs.OptionDescrs
import utils._
import checking._
import web._
import parser._
import backend._
import ontology._
import objects._

/** an auxiliary class to split the [[Controller]] class into multiple files */
trait ActionHandling {self: Controller =>

  private def getOAFOrError = getOAF.getOrElse {
    throw GeneralError("no OAF configuration entry found")
  }

  // ******************************** handling actions

  // some actions are defined in separate methods below

  /** executes a string command */
  def handleLine(l: String, showLog: Boolean = true) {
    try {
      val act = Action.parseAct(l, getBase, getHome)
      handle(act, showLog)
    } catch {
      case e: Error =>
        log(e)
    }
    report.flush
  }

  /** executes an Action */
  def handle(act: Action, showLog: Boolean = true) {
    state.currentActionDefinition match {
      case Some(Defined(file, name, acts)) if act != EndDefine =>
        state.currentActionDefinition = Some(Defined(file, name, acts ::: List(act)))
        if (showLog) report("user", "  " + name + ":  " + act.toString)
      case _ =>
        if (act != NoAction && showLog) report("user", act.toString)
        act match {
          case AddMathPathFS(uri, file) =>
            val lc = new LocalCopy(uri.schemeNull, uri.authorityNull, uri.pathAsString, file)
            backend.addStore(lc)
          case AddMathPathJava(file) =>
            backend.openRealizationArchive(file)
          case Local =>
            val currentDir = new java.io.File(".").getCanonicalFile
            val b = URI.fromJava(currentDir.toURI)
            backend.addStore(LocalSystem(b))
          case AddArchive(f) =>
            addArchive(f)
          case ConfBuild(mod, targets, profile) =>
            confBuildAction(mod, targets, profile)
          case MakeAction(key, args) =>
            makeAction(key, args)
          case ArchiveBuild(ids, key, mod, in) =>
            archiveBuildAction(ids, key, mod, in)
          case ArchiveMar(id, file) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
            arch.toMar(file)
          case AddExtension(c, args) =>
            extman.addExtension(c, args)
          case AddMWS(uri) =>
            extman.addExtension(new MathWebSearch(uri.toURL))
          case OAFInit(path) =>
            getOAFOrError.init(path)
          case OAFClone(path) =>
            cloneRecursively(path)
          case OAFPull =>
            getOAFOrError.pullAll
          case OAFPush =>
            getOAFOrError.pushAll
          case SetBase(b) =>
            state.nsMap = state.nsMap(b)
            report("response", "base: " + getBase)
          case ServerOn(port) => server match {
            case Some(serv) => logError("server already started on port " + serv.port)
            case None if Util.isTaken(port) => logError("port " + port + " is taken, server not started.")
            case _ =>
              val serv = new Server(port, this)
              serv.start
              log("Server started at http://localhost:" + port)
              server = Some(serv)
          }
          case ServerOff => server match {
            case Some(serv) =>
              serv.stop
              log("Server stopped")
              server = None
            case None => log("server not running")
          }
          case Scala(fOpt) =>
            val interp = new MMTILoop(this)
            interp.run(fOpt)
          case MBT(file) =>
            new MMTScriptEngine(this).apply(file)
          case Clear => clear
          case ExecFile(f, nameOpt) => execFileAction(f, nameOpt)
          case Define(name) =>
            state.currentActionDefinition match {
              case None =>
                state.currentActionDefinition = Some(Defined(state.home, name, Nil))
              case Some(_) =>
                throw ParseError("end of definition expected")
            }
          case EndDefine =>
            state.currentActionDefinition match {
              case Some(a) =>
                state.actionDefinitions ::= a
                state.currentActionDefinition = None
              case None =>
                throw ParseError("no definition to end")
            }
          case Do(file, name) => doAction(file, name)
          case AddReportHandler(h) => report.addHandler(h)
          case LoggingOn(g) => report.groups += g
          case LoggingOff(g) => report.groups -= g
          case NoAction => ()
          case Read(f, interpret) =>
            if (!f.isFile)
               throw GeneralError("file not found: " + f)
            val ps = backend.resolvePhysical(f) match {
              case Some((arch, p)) => ParsingStream.fromSourceFile(arch, FilePath(p))
              case None => ParsingStream.fromFile(f)
            }
            read(ps, interpret, mayImport = true)(new ErrorLogger(report))
            ps.stream.close
          case Check(p, id) =>
            checkAction(p, id)
          case Navigate(p) =>
            notifyListeners.onNavigate(p)
          case a: GetAction => a.make(this)
          case PrintAllXML => report("response", "\n" + library.getModules.map(_.toNode).mkString("\n"))
          case PrintAll => report("response", "\n" + library.toString)
          case PrintConfig =>
             println(state.config.toString)
          case Compare(p, r) => //TODO
          case WindowClose(w) => winman.deleteWindow(w)
          case WindowPosition(w, x, y) => winman.getWindow(w).setLocation(x, y)
          case BrowserAction(c) => c match {
            case "on" => winman.openBrowser
            case "off" => winman.closeBrowser
          }
          case Exit =>
            cleanup
            sys.exit()
        }
        if (act != NoAction && showLog) report("user", act.toString + " finished")
    }
  }
    
  /** handles [[ConfBuild]] */
  def confBuildAction(modS : String, targets : List[String], profile : String) = {
    val config = getConfig
    val mod  = modS match {
      case "Build" => Build
      case "Clean" => Clean
      case _ => Build(Update(Level.Error))
    }
    val archives = try {
      config.getProfile(profile).archives.map(aid => config.getArchive(aid))
    } catch {
      case e : Exception => config.getWritableArchives
    }
    archives foreach {a =>
      config.getArchive(a.id).formats foreach {f =>
        val imps = config.getImporters(f)
        val exps = config.getExporters(f)
        var foundChanged = false
        imps foreach { imp =>
          if (targets.contains(imp) || targets.isEmpty) foundChanged = true
          if (foundChanged) archiveBuildAction(List(a.id), imp, mod, EmptyPath)
        }
        exps foreach { exp =>
          if (targets.contains(exp) || foundChanged || targets.isEmpty) archiveBuildAction(List(a.id), exp, mod, EmptyPath)
        }
      }
    }
  }

  /** auxiliary method of makeAction */
  def usageOption: OptionDescrs = List(
    OptionDescr("usage", "", NoArg, "display usage message"),
    OptionDescr("help-command", "", NoArg, "help about the build target"))
  
  /** handles [[MakeAction]] */
  @deprecated //TODO this is now handled by the :make shell extension; some code may still have to be migrated there
  def makeAction(key: String, allArgs: List[String]) {
    report.addHandler(ConsoleHandler)
    val optPair = BuildTargetModifier.splitArgs(allArgs, s => logError(s))
    optPair.foreach { case (mod, restArgs) =>
      val (args, fileNames) = AnaArgs.splitOptions(restArgs)
      val home = getHome
      val files = fileNames.map(s => File(home.resolve(s)))
      val realFiles = if (files.isEmpty)
        List(home)
      else {
        files.filter { f =>
          val ex = f.exists
          if (!ex)
            logError("file \"" + f + "\" does not exist")
          ex
        }
      }
      val inputs = realFiles flatMap collectInputs
      val (usageOpts, _) = AnaArgs(usageOption, args)
      val bt = extman.getOrAddExtension(classOf[BuildTarget], key, args) getOrElse {
         throw RegistrationError("build target not found: " + key)
      }
      report.groups += bt.logPrefix
      report.groups += "archives"
      if (usageOpts.nonEmpty) {
        val moreOpts = bt match {
          case pbt: BuildTargetArguments => pbt.verbOpts ++ pbt.buildOpts
          case _ => Nil
        }
        AnaArgs.usageMessage(ShellArguments.toplevelArgs ++ usageOption ++
          BuildTargetModifier.optDescrs ++ moreOpts).foreach(println)
      } else {
        inputs foreach {case (root, fp) =>
          addArchive(root) // add the archive
          backend.getArchive(root) match {
            case None =>
              // opening may fail despite resolveAnyPhysical (i.e. for a MANIFEST.MF without id)
              logError("not an archive: " + root)
            case Some(archive) =>
              val inPath = fp.segments match {
                case dim :: path =>
                  bt match {
                    case tbt: TraversingBuildTarget if dim != tbt.inDim.toString =>
                      logError("wrong in-dimension \"" + dim + "\"")
                    case _ =>
                  }
                  FilePath(path)
                case Nil => EmptyPath
              }
              bt(mod, archive, inPath)
          }
        }
      }
    }
  }

  /** auxiliary function of makeAction: guess which files/folders the users wants to build
    *
    * @return archive root and relative path in it
    */
  private def collectInputs(f: File): List[(File, FilePath)] = {
    backend.resolveAnyPhysical(f) match {
      case Some(ff) =>
        // f is a file in an archive
        List(ff)
      case None =>
        // not in archive, treat f as directory containing archives
        if (f.isDirectory) f.subdirs.flatMap(collectInputs)
        else {
          logError("not a file within an archive: " + f)
          Nil
        }
    }
  }
  
  /** handles [[ArchiveBuild]] */ 
  def archiveBuildAction(ids: List[String], key: String, mod: BuildTargetModifier, in: FilePath) {
    ids.foreach { id =>
      val arch = backend.getArchive(id) getOrElse (throw GetError("archive not found: " + id))
      key match {
        case "check" => arch.check(in, this)
        case "validate" => arch.validate(in, this)
        case "relational" =>
          arch.readRelational(in, this, "rel")
          arch.readRelational(in, this, "occ")
          log("done reading relational index")
        case "integrate" => arch.integrateScala(this, in)
        case "test" => // TODO misuse of filepath parameter
          if (in.segments.length != 1)
            logError("exactly 1 parameter required, found " + in)
          else
            arch.loadJava(this, in.segments.head)
        case "close" =>
          val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
          backend.closeArchive(id)
          notifyListeners.onArchiveClose(arch)
        case _ =>
          val bt = extman.getOrAddExtension(classOf[BuildTarget], key) getOrElse {
             throw RegistrationError("build target not found: " + key)
          }
          bt(mod, arch, in)
      }
    }
  }

  /** handles [[ExecFile]] */
  def execFileAction(f: File, nameOpt: Option[String]) {
    val folder = f.getParentFile
    // store old state, and initialize fresh state
    val oldHome = state.home
    val oldCAD = state.currentActionDefinition
    state.home = folder
    state.currentActionDefinition = None
    // execute the file
    File.read(f).split("\\n").foreach(f => handleLine(f))
    if (state.currentActionDefinition.isDefined)
      throw ParseError("end of definition expected")
    // restore old state
    state.home = oldHome
    state.currentActionDefinition = oldCAD
    // run the actionDefinition, if given
    nameOpt foreach { name =>
      doAction(Some(folder), name)
    }
  }

  /** clone an archive using [[OAF]] and also clone its dependencies */
  def cloneRecursively(p: String) {
    val oaf = getOAFOrError
    report("user", "trying to clone " + p)
    val lc = oaf.clone(p) getOrElse {
      logError("cloning failed, trying to download")
      oaf.download(p) 
    }.getOrElse {
      logError("downloading failed, giving up")
      return
    }
    val archs = backend.openArchive(lc)
    archs foreach {a =>
      val depS = a.properties.getOrElse("dependencies", "")
      // TODO lmh falsely uses , as separator instead of space
      val deps = if (depS.contains(",")) stringToList(depS, ",").map(_.trim) else stringToList(depS)
      deps foreach {d => cloneRecursively(URI(d).pathAsString)}
    }
  }

  /** add an archive plus its optional classpath and notify listeners */
  def addArchive(root: File) {
    val archs = backend.openArchive(root)
    archs.foreach { a =>
      a.properties.get("classpath").foreach { cp =>
        backend.openRealizationArchive(a.root / cp)
      }
      notifyListeners.onArchiveOpen(a)
    }
  }

  def doAction(file: Option[File], name: String) {
    state.actionDefinitions.find { a => (file.isEmpty || a.file == file.get) && a.name == name } match {
      case Some(Defined(_, _, actions)) =>
        actions foreach (f => handle(f))
      case None =>
        logError("not defined")
    }
  }

  def checkAction(p: Path, id: String) {
    val checker = extman.get(classOf[Checker], id).getOrElse {
      throw GeneralError(s"no checker $id found")
    }
    checker(p)(new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore))
  }


  // ******************************** handling messages
  /** processes a message, see [[web.MessageHandler]] */
  def handle(message: Message): Response = try {
     message match {
       case EvaluateMessage(contOpt, in, text, out) =>
         val interpreter = extman.get(classOf[checking.Interpreter], in).getOrElse {
           return ErrorResponse("no parser found")
         }
         val presenter = extman.get(classOf[presentation.Presenter], out).getOrElse {
           return ErrorResponse("no parser found")
         }
         val context = contOpt.getOrElse(Context.empty)
         val pu = ParsingUnit(SourceRef.anonymous(text), context, text, getNamespaceMap)
         val checked = interpreter(pu)(ErrorThrower)
         val simplified = simplifier(checked, context)
         val presented = presenter.asString(simplified)
         ObjectResponse(presented, "html")
       
       case _ =>
         ErrorResponse("not implemented yet: " + message.toString)
     }
  } catch {
    case e: Error => ErrorResponse(e.toString)
  }
}
