package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import parser._

import Theory._

class DiffReader(controller : Controller) {
  private val xmlReader = new XMLReader(controller)
  def apply(n : scala.xml.Node) : Diff = n.label match {
    case "omdoc-diff" =>
      val changes = n.child.map(parseChange).toList
      new Diff(changes.flatten)
  }

  def parseChange(n : scala.xml.Node) : List[Change] = n.label match {
    case "add" | "delete" =>
      var changes : List[Change] = Nil
      val cont = new StructureParserContinuations(ErrorThrower) {
        override def onElement(s : StructuralElement): Unit = {
          val change = n.label match {
              case "add" => Add(s)
              case "delete" => Delete(s)
          }
          changes ::= change
        }
      }
      val level = (n \ "@level").text
      val base = Path.parse((n \ "@base").text)
      val nsMap = NamespaceMap(base)
      level match {
        case "module" =>
          val path = Path.parseM((n \ "@path").text, nsMap)
          n.child.foreach {c => xmlReader.readInDocument(NamespaceMap(path.doc), None, c)(cont)}
        case "declaration" =>
          val path = Path.parseS((n \ "@path").text, nsMap)
          // TODO xmlReader.readInModule uses side-effects for a few minor features that we do not care about yet
          // so we pass a dummy arguments that absorbs the side effects
          val dummy = Theory.empty(path.doc, path.module.name, noMeta)
          n.child foreach {c => xmlReader.readInModule(path.module, NamespaceMap(base), dummy, c)(cont)}
      }
      changes = changes.reverse
      changes
    case "update" =>
      val base = Path.parse((n \ "@base").text)
      val nsMap = NamespaceMap(base)
      val path = Path.parse((n \ "@path").text) match {
        case c : ContentPath => c
        case p => throw ImplementationError("Expected content path found : " + p.toPath)
      }
      val compName = ComponentKey.parse((n \ "@name").text)
      val oldXMLO = n.child.find(x => x.label == "old")
      val newXMLO = n.child.find(x => x.label == "new")
      val old = oldXMLO.map(Obj.parseTerm(_, nsMap))
      val nw  = newXMLO.map(Obj.parseTerm(_, nsMap))
      List(UpdateComponent(path, compName, old, nw))
  }
}
