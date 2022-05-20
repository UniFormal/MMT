package info.kwarc.mmt.buildserver

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.utils.{File, FilePath}
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.CanonicalTreeParser

sealed class GitChange
case class Add(path : FilePath) extends GitChange
case class Delete(path: FilePath) extends GitChange
//case class Rename(oldpath: FilePath, newpath: FilePath) extends GitChange
case class Change(path: FilePath) extends GitChange

case class GitCommit(previous : Option[RevCommit],latest:RevCommit,diffs: List[GitChange]) {
  def getMessage = latest.getFullMessage
  def getEmail = latest.getAuthorIdent.getEmailAddress
  def getAuthor = latest.getAuthorIdent.getName
}

case class GitRepo(a : Archive) {
  import scala.jdk.CollectionConverters._
  private lazy val repo = new FileRepositoryBuilder().setGitDir(a.root / ".git").build()
  private lazy val git = new Git(repo)
  def call_pull = {
    git.pull().call().getMergeResult.getMergedCommits.collect {
      case cmt: RevCommit => cmt
    }.lastOption
  }
  def pullDivs ={
    val oldc = currentCommit
    val newc = call_pull
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
