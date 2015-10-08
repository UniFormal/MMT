package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import utils._
import scala.collection.immutable.{ListMap}

abstract class CompilerConf(val uri : String, val key : String, val args : List[String]) 
case class ImporterConf(override val uri : String, override val key : String, override val args : List[String]) extends CompilerConf(uri, key, args)
case class ExporterConf(override val uri : String, override val key : String, override val args : List[String]) extends CompilerConf(uri, key, args)
case class ArchiveConf(id : String, formats : List[String])
case class FormatConf(id : String, importers : List[String], exporters : List[String])

class MMTConfig(controller : Controller, autoload : Boolean = true) {
    private var _base : String = ""
    private var _importers : List[ImporterConf] = Nil
    private var _exporters : List[ExporterConf] = Nil
    private var _archives : List[ArchiveConf] = Nil
    private var _formats : List[FormatConf] = Nil
    
    def base = _base
    def importers = _importers
    def exporters = _exporters
    def archives = _archives
    def formats = _formats
    
    def addImporter(imp : ImporterConf) = {
      _importers ::= imp
      if (autoload) loadExtension(imp.uri, imp.args)
    }
    def addExporter(exp : ExporterConf) = {
      _exporters ::= exp
      if (autoload) loadExtension(exp.uri, exp.args)
    }
    
    def addArchive(a : ArchiveConf) {
      _archives ::= a
      if (autoload) loadArchive(base + a.id)
    }
    
    def addFormat(f : FormatConf) {
      _formats ::= f
    }
    
    def setBase(b : String) {
      _base = b
    }
    
    def getArchive(aid : String) = archives.find(_.id == aid).getOrElse(throw new Exception("Unknown archive id: " + aid))
    def getImporters(formatId : String) = formats.find(_.id == formatId).getOrElse(throw new Exception("Unknown format id: " + formatId)).importers
    def getExporters(formatId : String) = formats.find(_.id == formatId).getOrElse(throw new Exception("Unknown format id: " + formatId)).exporters
    

    def loadActiveExtensions() { 
      val activeFormats = archives.flatMap(_.formats).toSet[String] map {id => 
        formats.find(_.id == id).getOrElse(throw new Exception("Unknown format id: " + id))
      }
      
      val activeImporters = activeFormats.flatMap(_.importers).toSet[String] map {key => 
        importers.find(_.key == key).getOrElse(throw new Exception("Unknown importer key: " + key))
      }
      val activeExporters = activeFormats.flatMap(_.exporters).toSet[String] map {key => 
        exporters.find(_.key == key).getOrElse(throw new Exception("Unknown exporter key: " + key))
      }
      
      (activeImporters ++ activeExporters) foreach {comp => 
        println("loading " + comp.uri)
        loadExtension(comp.uri, comp.args)
      }
    }
    
    //Utility
    def loadExtension(uri : String, args : List[String] = Nil) = controller.handle(AddExtension(uri, args))
    def loadArchive(location : String) = controller.handle(AddArchive(File(location)))

    def loadAllArchives() {
      archives foreach { arch => 
        loadArchive(base + arch.id)
      }
    }
 
  }
