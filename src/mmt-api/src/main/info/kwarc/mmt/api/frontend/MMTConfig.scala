package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import archives._
import utils._

/** an entry in an MMT configuration file (see [[MMTConfig]]) */
abstract class ConfEntry {
   /** archive id, build target key, etc. */
   val id : String
}

/**
 * registers [[BuildTarget]]s with their arguments
 *
 * @param key the key of the target
 * @param cls the qualified Java class name of this target's implementation
 *  (must only be on the class path if this target is actually used)
 * @param args the arguments to be used if this target is instantiated
 */
case class ExtensionConf(key : String, cls : String, args : List[String]) extends ConfEntry {
   val id = key
}

/**
 * sets an MMT-specific environment variable
 */
case class EnvVarConf(id : String, value: String) extends ConfEntry

abstract class BackendConf extends ConfEntry

/**
 * registers an archive with its formats and a flag of whether it is readonly (pregenerated) or should be built
 */
case class ArchiveConf(id : String, formats : List[String], readonly : Boolean) extends BackendConf

/**
 * Registers a profile as a subset of active archives
 */
case class ProfileConf(id : String, archives : List[String]) extends BackendConf

/**
 * declares an abbreviation (CURIE-style namespace prefix) for a URI
 */
case class NamespaceConf(id: String, uri: URI) extends ConfEntry

/**
 * registers a remote database
 */
case class DatabaseConf(url: URI, uri: URI) extends BackendConf {
   val id = url.toString
}

/**
 * registers a set of LMH working copies
 */
case class LMHConf(local: File, https: Boolean, remote: Option[URI]) extends BackendConf {
   val id = remote.toString
}

/**
  * registers a folder on which to find archives
  */
case class MathPathConf(local: File) extends BackendConf {
  val id = local.toString
}

/** defines an archive format
 *  @param id the format name
 *  @param importers the importers to be used for archives of this format
 *  @param exporters the exporters to be used for archives of this format
 */
case class FormatConf(id : String, importers : List[String], exporters : List[String]) extends ConfEntry


/** registers an extension providing opaque semantics for a theory
  * @param theory the theory
  * @param cls the implementing extension
  */
case class SemanticsConf(theory: MPath, cls: String, args: List[String]) extends ConfEntry {
  val id = theory.toPath
}

/** ignored by MMT but available to plugins */
case class ForeignConf(section: String, key: String, values: List[String]) extends ConfEntry {
   val id = section + "_" + key
}

/**
 * an MMT configuration stores catalogs for finding extensions, archives, etc.
 * It is a list of [[ConfEntry]] that can be read from a .cfg file
 */
class MMTConfig {
    private var base : String = ""
    private var entries: List[ConfEntry] = Nil

    override def toString = {
       entries.reverseMap {e => e.toString}.mkString("\n")
    }

    def addEntry(e: ConfEntry): Unit = {entries = entries ::: List(e)}
    def setBase(b : String): Unit = {
      base = b
    }

    def add(that: MMTConfig): Unit = {
       entries = that.getEntries ::: entries
       setBase(that.getBase)
    }

    def getBase = base

    def getEntries = entries
    def getEntries[E <: ConfEntry](cls: Class[E]): List[E] = entries.collect {
       case e: E@unchecked if cls.isInstance(e) => e
    }
    def getEntry[E <: ConfEntry](cls: Class[E], id: String): Option[E] = getEntries(cls).find {e =>
       e.id == id
    }

    /** all foreign conf entries of a certain section */
    def getForeignEntries(section: String): List[ForeignConf] = getEntries(classOf[ForeignConf]).filter(_.section == section)

    def getArchive(aid : String) = getEntry(classOf[ArchiveConf], aid) getOrElse {
       throw ConfigurationError("archive not registered: " + aid)
    }
    def getArchives = getEntries(classOf[ArchiveConf])
    def getWritableArchives = getEntries(classOf[ArchiveConf]).filter(_.readonly == false)

    def getFormat(id: String) = getEntry(classOf[FormatConf], id) getOrElse {
       throw ConfigurationError("format not registered: " + id)
    }
    def getProfile(id : String) = getEntry(classOf[ProfileConf], id) getOrElse {
       throw ConfigurationError("profile not registered: " + id)
    }

    def getImporters(format : String) = getFormat(format).importers
    def getExporters(format : String) = getFormat(format).exporters
    def getImportersForArchive(archive : String) = getArchive(archive).formats.flatMap(getImporters).distinct
    def getExportersForArchive(archive : String) = getArchive(archive).formats.flatMap(getExporters).distinct
}


/** helper functions for configurations */
object MMTConfig {
  /** split at whitespace (allowing for quoted segments to contain whitespace) */
  private def split(line: String): List[String] = splitAtWhitespace(line)
  /**
   * parses a configuration file
   *
   * syntax:
   *  configuration includes: #include PATH/TO/CONF/FILE
   *  section headers: #targets | #formats | #archives
   *  all other lines are configuration entries of the respective section
   */
  def parse(f: File): MMTConfig = {
    parse(File.read(f), Some(f.up))
  }
  def parse(s: String, home: Option[File]) : MMTConfig = {
    def relFile(f: String) = home match {
       case Some(h) => h.resolve(f)
       case None => File(s)
    }
    val config = new MMTConfig
    var section = ""
    s.split('\n').foreach {l =>
      val line = l.trim
      lazy val fail = println(s"invalid $section line: " + line)
      if (line.startsWith("//") || line.isEmpty) {
        //ignore
      } else if (line.startsWith("#include")) {
         val inc = line.substring("#include".length).trim
         val incConf = parse(relFile(inc))
         config.add(incConf)
      } else if (line.startsWith("#")) {
        section = line.substring(1)
      } else section match {
        // TODO "importers" and "exporters" are deprecated but still used by Mihnea
        case "importers" | "exporters" | "extensions" => split(line) match {
          case key :: cls :: args =>
            config.addEntry(ExtensionConf(key, cls, args))
          case _ => fail
        }
        case "archives" => split(line) match {
          case id :: fmtsS :: Nil =>
            val fmts = fmtsS.split(",").toList
            config.addEntry(ArchiveConf(id, fmts, readonly = false))
          case _ => fail
        }
        case "generated_archives" => split(line) match {
          case id :: fmtsS :: Nil =>
            val fmts = fmtsS.split(",").toList
            config.addEntry(ArchiveConf(id, fmts, readonly = true))
          case _ => fail
        }
        case "namespaces" => split(line) match {
          case id :: uri :: Nil =>
            config.addEntry(NamespaceConf(id, URI(uri)))
          case _ => fail
        }
        case "profiles" => split(line) match {
          case id :: archsS :: Nil =>
            val archs = archsS.split(",").toList
            config.addEntry(ProfileConf(id, archs))
          case _ => fail
        }
        case "formats" => split(line) match {
          case id :: impsS :: expsS :: Nil =>
            val imps = impsS.split(",").toList
            val exps = expsS.split(",").toList
            config.addEntry(FormatConf(id, imps, exps))
          case _ => fail
        }
        case "semantics" => split(line) match {
          case thy :: cls :: args =>
            val thyP = Path.parseM(thy, NamespaceMap.empty)
            config.addEntry(SemanticsConf(thyP, cls, args))
          case _ => fail
        }
        case "backends" => split(line) match {
           case ("lmh" | "oaf") :: local :: args if args.length <= 2 =>
              val (https, argsRest) = args match {
                case "ssh" :: tl => (false, tl)
                case "https" :: tl => (true, tl)
                case _ => (true, args)
              }
              val remote = argsRest.headOption.map(URI(_))
              config.addEntry(LMHConf(relFile(local), https, remote))
           case "mathpath" :: local :: Nil =>
              config.addEntry(MathPathConf(File(local)))
           case _ => fail
        }
        case "envvars" => split(line) match {
          case name :: value :: Nil =>
            config.addEntry(EnvVarConf(name, value))
          case _ => fail
        }
        case "base" => config.setBase(line)
        case _ => split(line) match {
           case key::values =>
              config.addEntry(ForeignConf(section, key, values))
           case _ => fail
        }
      }
    }
    config
  }
}
