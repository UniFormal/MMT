package info.kwarc.mmt.mathhub

import java.net.URLDecoder

import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives.lmh.{LMHHub, LMHHubArchiveEntry, LMHHubGroupEntry}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.{GeneralError, MPath, ParseError, Path}

trait MathHubAPIImpl {
  protected var controller: Controller

  //
  // API Methods
  //

  protected def getURI(uri: String) : List[IReferencable] = {
    ???
  }

  protected def getGroups : List[IGroupRef] = {
    ???
  }

  protected def getGroup(group: String) : IGroup  = {
    ???
  }

  protected def getArchive(group: String, name: String) : IArchive = {
    ???
  }

  protected def getModule(group: String, archive: String, name: String): IModule = {
    ???
  }

  protected def getDocument(group: String, archive: String, name: String): IDocument = {
    ???
  }
}