package info.kwarc.mmt.stex.vollki

import info.kwarc.mmt.api.{GlobalName, Path}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.utils.File

import scala.collection.mutable

class CongitiveDimension extends Enumeration {
  val REMEMBER, UNDERSTAND,ANALYZE, EVALUATE = Value
}

class SupplementaryDimension extends Enumeration {
  val APPLY, CREATE, NONE = Value
}
case class CognitiveValue(dim: CongitiveDimension, supp : SupplementaryDimension, learning_object:GlobalName)

case class UserModel(f : File) {
  private val map = mutable.HashMap.empty[String,Double]
  def getValue(cog:CognitiveValue): Double = ???
  protected def read() = File.read(f).split('\n').foreach { line =>
    val i = line.indexWhere(_ == ' ')
    if (i != -1) {
      val dst = line.take(i)
      val pst = line.drop(i + 1)
      map(pst) = dst.toDouble
    }
  }
  protected def save() : Unit = {
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
  read()
}

class UserModels extends Extension {
  private val map = mutable.HashMap.empty[String,UserModel]

  def getUser(s : String) = map.get(s)
  def getAllUsers = map.values.toList
  def simulate_user = new UserModel(File("/dummy/file")) {
    override protected def read(): Unit = {}
    override protected def save(): Unit = {}
  }

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
