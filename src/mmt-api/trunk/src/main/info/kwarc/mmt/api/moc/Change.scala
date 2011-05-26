package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._

import scala.collection.mutable.HashSet


abstract class Change

class Add(e: StructuralElement) extends Change
class Update(e: StructuralElement) extends Change
class Delete(p: Path) extends Change
class Rename(p: Path, name: LocalName) extends Change

class Diff {
   val changes = new HashSet[Change]
}