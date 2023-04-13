package info.kwarc.mmt.api.utils

import java.lang.reflect.InvocationTargetException
import scala.concurrent.{Await, Future}
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.reflect.runtime.universe
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration

class Reflection(jarfile:File,parentClassLoader:Option[ClassLoader] = None) {
  private val classLoader = new URLClassLoader(Array(jarfile.toURI.toURL),parentClassLoader.getOrElse(this.getClass.getClassLoader))
  private val mirror = universe.runtimeMirror(classLoader)

  def safely[A](f: => A): A = {
    val result = Future {
      Thread.currentThread().setContextClassLoader(classLoader)
      f
    }
    Await.result(result,Duration.Inf)
  }

  def getRefClass(fqcp : String): ReflectedClass = new ThisReflectedClass(fqcp)

  private class ThisReflectedClass(val fqcp : String) extends ReflectedClass {
    private val cls =  mirror.reflectClass(mirror.staticClass(fqcp))
    private val constructor = cls.symbol.toType.decl(universe.termNames.CONSTRUCTOR).asMethod
    def getInstance(args:Any*) = {
      new ThisReflectedInstance(cls.reflectConstructor(constructor).apply(args:_*),this)
    }

    def asInstance(a: Any): ReflectedInstance = {
      new ThisReflectedInstance(a,this)
    }
  }
  private class ThisReflectedInstance(private[Reflection] val instance : Any,val reflectedClass : ReflectedClass) extends ReflectedInstance {
    private val symbol = mirror.classSymbol(instance.getClass)
    private val reflected = mirror.reflect(instance)
    def field[A](name: String,tp: Reflection.ReturnType[A]): A = {
      val decl = symbol.typeSignature.decl(universe.TermName(name)).asTerm
      tp.resolve(reflected.reflectField(decl).get)
    }
    def method[A](name : String,tp : Reflection.ReturnType[A], args : Any*) : A = {
      val rargs = args.map {
        case r:ThisReflectedInstance => r.instance
        case a => a
      }
      val decl = symbol.toType.decl(universe.TermName(name)).asMethod//.member(universe.TermName(name)).asInstanceOf[universe.MethodSymbol]
      val t = decl.asMethod.returnType
      val method = reflected.reflectMethod(decl)
      val res = try {
        method.apply(rargs:_*)
      } catch {
        case e : InvocationTargetException =>
          throw e.getCause
      }
      val ret = mirror.reflect(res).instance
      tp.resolve(ret)
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