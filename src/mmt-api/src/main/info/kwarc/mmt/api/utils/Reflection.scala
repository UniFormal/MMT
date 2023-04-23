package info.kwarc.mmt.api.utils

import java.lang.reflect.{Executable, InvocationTargetException}
import scala.concurrent.{Await, Future}
import java.net.URLClassLoader
import scala.concurrent.duration.Duration
import scala.util.Try

class Reflection(jarfile:File,parentClassLoader:Option[ClassLoader] = None) {
  private val classLoader = new URLClassLoader(Array(jarfile.toURI.toURL),parentClassLoader.getOrElse(this.getClass.getClassLoader))

  def safely[A](f: => A): A = {
    val result = Future {
      Thread.currentThread().setContextClassLoader(classLoader)
      f
    }(scala.concurrent.ExecutionContext.global)
    Await.result(result,Duration.Inf)
  }

  def getRefClass(fqcp : String): ReflectedClass = new ThisReflectedClass(fqcp)

  private class ThisReflectedClass(val fqcp : String) extends ReflectedClass {
    private[utils] val cls = classLoader.loadClass(fqcp)
    def getInstance(args:Any*) = {
      val const = findM(cls.getConstructors,args:_*)
      new ThisReflectedInstance(const.get.newInstance(args:_*),this)
    }
    def findM[A <: Executable](ls:Iterable[A],args:Any*) = {
      ls.find(c =>
        c.getParameterCount ==  args.length &&
          c.getParameterTypes.zip(args.map(_.getClass)).forall {
            case (decl, actual) => decl.isAssignableFrom(actual)
          }
      )
    }

    def asInstance(a: Any): ReflectedInstance = {
      new ThisReflectedInstance(a,this)
    }
  }
  private class ThisReflectedInstance(private[Reflection] val instance : Any,val reflectedClass : ThisReflectedClass) extends ReflectedInstance {
    def field[A](name: String,tp: Reflection.ReturnType[A]): A = {
      val value = reflectedClass.cls.getField(name).get(instance)
      tp.resolve(value)
    }
    def method[A](name : String,tp : Reflection.ReturnType[A], args : Any*) : A = {
      val rargs = args.map {
        case r:ThisReflectedInstance => r.instance
        case a => a
      }
      val method = reflectedClass.findM(reflectedClass.cls.getMethods.filter(_.getName == name),args:_*)
      val res = try {
        method.get.invoke(instance,rargs:_*)
      } catch {
        case e : InvocationTargetException =>
          throw e.getCause
      }
      tp.resolve(res)
    }
  }
}

trait ReflectedClass {
  val fqcp : String
  def getInstance(args:Any*) : ReflectedInstance
  def asInstance(a:Any) : ReflectedInstance
}
trait ReflectedInstance {
  val reflectedClass : ReflectedClass

  def field[A](name: String, tp: Reflection.ReturnType[A]): A
  def method[A](name : String,tp : Reflection.ReturnType[A], args : Any*) : A
}
object Reflection {
  sealed trait ReturnType[T] {
    type A = T
    def resolve(o:Any):T
  }
  final case class Atomic[T](cls: Class[T]) extends ReturnType[T] {
    override def resolve(o: Any): T = o.asInstanceOf[T]
  }
  final case class RList[T](cls: ReturnType[T]) extends ReturnType[List[T]] {
    override def resolve(o: Any): List[T] = o.asInstanceOf[List[Any]].map {
      io => cls.resolve(io)
    }
  }
  final case class JLinkedList[T](cls:ReturnType[T]) extends ReturnType[List[T]] {
    import scala.jdk.CollectionConverters._
    override def resolve(o: Any): List[T] = o.asInstanceOf[java.util.LinkedList[Any]].asScala.toList.map(io =>
      cls.resolve(io))
  }
  case class JList[T](cls: ReturnType[T]) extends ReturnType[List[T]] {
    import scala.jdk.CollectionConverters._
    override def resolve(o: Any): List[T] = o.asInstanceOf[java.util.List[Any]].asScala.toList.map(io =>
      cls.resolve(io))
  }
  case class RPair[T1, T2](l: ReturnType[T1], r: ReturnType[T2]) extends ReturnType[(T1, T2)] {
    override def resolve(o: Any): (T1, T2) = {
      val p = o.asInstanceOf[{
        def _1: l.A
        def _2: r.A
      }]
      (l.resolve(p._1),r.resolve(p._2))
    }
  }
  case class RTriple[T1, T2, T3](l: ReturnType[T1], m: ReturnType[T2], r: ReturnType[T3]) extends ReturnType[(T1, T2, T3)] {
    override def resolve(o: Any): (T1, T2, T3) = {
      val p = o.asInstanceOf[{
        def _1: l.A
        def _2: m.A
        def _3: r.A
      }]
      (l.resolve(p._1), m.resolve(p._2), r.resolve(p._3))
    }
  }
  case class Reflected(cls : ReflectedClass) extends ReturnType[ReflectedInstance] {
    override def resolve(o: Any): ReflectedInstance = cls.asInstance(o)
  }
  case class ROption[T](cls: ReturnType[T]) extends ReturnType[Option[T]] {
    override def resolve(o: Any): Option[T] = o match {
      case None => None
      case Some(a:Any) => Some(cls.resolve(a))
    }
  }
  val string = Atomic(classOf[String])
  val float = Atomic(classOf[Float])
  val int= Atomic(classOf[Int])
  val unit = Atomic(classOf[Unit])
}