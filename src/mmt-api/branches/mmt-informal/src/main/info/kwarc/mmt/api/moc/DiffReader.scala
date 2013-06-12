package info.kwarc.mmt.api.moc

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._


class DiffReader(controller : Controller) {
  def apply(n : scala.xml.Node) : Diff = n.label match {
    case "omdoc-diff" => 
      val changes = n.child.map(parseChange).toList
      new Diff(changes.flatten)
  }
  
  def parseChange(n : scala.xml.Node) : List[Change] = n.label match {
    case "add" | "delete" => 
      var changes : List[Change] = Nil
      def cont(s : StructuralElement) {
        val change = n.label match {
            case "add" => Add(s)
            case "delete" => Delete(s)
        }
        changes ::= change
      }
      val level = (n \ "@level").text
      val base = Path.parse((n \ "@base").text)    
      level match {
        case "module" =>  
          val path = Path.parseM((n \ "@path").text, base)
          controller.xmlReader.readModules(path.doc, None, n.child)(cont)
        case "declaration" => 
          val path = Path.parseS((n \ "@path").text, base)
          controller.xmlReader.readSymbols(path.module.toMPath, base, n.child)(cont)
      }
      changes = changes.reverse
      changes
    case "update" => 
      val base = Path.parse((n \ "@base").text)
      val path = Path.parse((n \ "@path").text) match {
        case c : ContentPath => c
        case p => throw ImplementationError("Expected content path found : " + p.toPath)
      }      
      val compName = DeclarationComponent((n \ "@name").text)
      val oldXMLO = n.child.find(x => x.label == "old")
      val newXMLO = n.child.find(x => x.label == "new")
      val old = oldXMLO.map(Obj.parseTerm(_, base))
      val nw  = newXMLO.map(Obj.parseTerm(_, base))
      List(UpdateComponent(path, compName, old, nw))
  }
}