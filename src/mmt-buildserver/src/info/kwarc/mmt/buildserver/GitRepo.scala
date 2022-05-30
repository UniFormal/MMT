package info.kwarc.mmt.buildserver

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.backend.LocalSystem
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.{File, FilePath, URI}
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.GitAPIException
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.CanonicalTreeParser

sealed abstract class GitChange {
  val path : FilePath
}
case class Add(path : FilePath) extends GitChange
case class Delete(path: FilePath) extends GitChange
//case class Rename(oldpath: FilePath, newpath: FilePath) extends GitChange
case class Change(path: FilePath) extends GitChange


case class GitCommit(previous : Option[RevCommit],latest:RevCommit,diffs: List[GitChange]) {
  def getMessage = latest.getFullMessage
  def getEmail = latest.getAuthorIdent.getEmailAddress
  def getAuthor = latest.getAuthorIdent.getName
}

object GitRepo{
  def apply(url: String,dir : File)(implicit controller: Controller) : Option[GitRepo] = {
    val git = Git.cloneRepository().setGitDir(dir / ".git").setURI(url).call()
    val b = URI.fromJava(dir.toURI)
    controller.backend.addStore(LocalSystem(b))
    controller.backend.getArchive(dir).map(GitRepo(_))
  }
}

case class GitRepo(archive : Archive) {
  import scala.jdk.CollectionConverters._
  private lazy val repo = new FileRepositoryBuilder().setGitDir(archive.root / ".git").build()
  private lazy val git = new Git(repo)
  def call_pull = {
    git.pull().call().getMergeResult.getMergedCommits.collect {
      case cmt: RevCommit => cmt
    }.lastOption
  }
  def pullDivs : Option[GitCommit] ={
    val oldc = currentCommit
    val newc = try { call_pull } catch {
      case _ : GitAPIException => return None
    }
    if (newc == oldc) return None
    newc.map { c =>
      val diff = diffs(oldc,Some(c))
      GitCommit(oldc,c,diff)
    }
  }
  def currentCommit = {
    git.log().call().asScala.headOption
  }
  private def getTreeParser(id : Option[ObjectId]) = {
    val walk = new RevWalk(repo)
    val commit = walk.parseCommit(id.orNull)
    val treeId = commit.getTree.getId
    val reader = repo.newObjectReader()
    new CanonicalTreeParser(null,reader,treeId)
  }
  def diffs(oldc:Option[ObjectId],newc:Option[ObjectId]) : List[GitChange] = {
    val oldwalk = getTreeParser(oldc)
    val newwalk = getTreeParser(newc)
    git.diff().setOldTree(oldwalk).setNewTree(newwalk).call().asScala.toList.map {diffentry =>
      diffentry.getChangeType.name() match {
        case "ADD" => Add(FilePath(diffentry.getNewPath))
        case "DELETE" => Delete(FilePath(diffentry.getOldPath))
        case "MODIFY" if diffentry.getOldPath == diffentry.getNewPath =>
          Change(FilePath(diffentry.getOldPath))
        case _ =>
          ???
      }
    }
  }
}
