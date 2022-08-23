package info.kwarc.mmt.stex.vollki

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.utils.File

import scala.collection.mutable

case class UserModel(f : File) {
  private val map = mutable.HashMap.empty[String,Double]
  File.read(f).split('\n').foreach { line =>
    val i = line.indexWhere(_ == ' ')
    if (i != -1) {
      val dst = line.take(i)
      val pst = line.drop(i + 1)
      map(pst) = dst.toDouble
    }
  }
  private def save() : Unit = {
    File.write(f,{
      map.map{
        case (node,value) => value.toString + " " + node
      }.mkString("\n")
    })
  }
  def getValue(n : FullsTeXGraph.sTeXNode) = map.getOrElse(n.id,0.0)
  def setValue(n : FullsTeXGraph.sTeXNode,value : Double) = {
    map(n.id) = value
    save()
  }
  def setValueDeps(n : FullsTeXGraph.sTeXNode,value : Double) = {
    n.topologicalSort.nodes().foreach(in => map(in.id) = value)
    save()
  }
}

class UserModels extends Extension {
  private val map = mutable.HashMap.empty[String,UserModel]

  def getUser(s : String) = map.get(s)
  def getAllUsers = map.values.toList

  override def start(args: List[String]): Unit = {
    super.start(args)
    args match {
      case List(f) if File(f).exists() && File(f).isDirectory =>
        val dir = File(f)
        dir.children.foreach{c =>
          map(c.name) = UserModel(c)
        }
      case _ => ???
    }
  }
}
