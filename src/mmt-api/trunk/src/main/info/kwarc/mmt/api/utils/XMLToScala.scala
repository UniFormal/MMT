package info.kwarc.mmt.api.utils

import scala.xml._
import scala.reflect.runtime.universe._

class XMLToScala(pkg: String) {
   private val m = runtimeMirror(getClass.getClassLoader)
   private val StringType = typeOf[String]
   // workaround because makes the Int arguments into a TypeRef
   // private val IntType = typeOf[Int]
   private case class Dummy(a: Int, b: Boolean)
   private val dummyTypes = typeOf[Dummy].companion.member(TermName("apply")).asMethod.paramLists.flatten.toList.map(_.asTerm.info) 
   private val IntType = dummyTypes(0)
   private val BoolType = dummyTypes(1)
   
   /** matches names _i for integers i */
   private object NumberedChildName {
      def unapply(s: String): Option[Int] = if (s.startsWith("_")) Some(s.substring(1).toInt) else None 
   }
   
   case class ExtractError(msg: String) extends Exception(msg)
   
   /**
    * @tparam A the case class of the return type
    * @param file the xml file
    * @return the content of file parsed into type A
    */
   //def load[A](file: File): A = apply(xml.readFile(file), implicitly[TypeTag[A]].tpe).asInstanceOf[A]
   
   /** remove non-Scala-id characters */
   private def xmlName(arg: Symbol) = arg.name.decodedName.toString
   private def xmlName(s: String) = s.replace("_", "-")
   private def scalaName(s: String) = s.replace("-", "_")
   
   def apply(file: File): Any = apply(xml.readFile(file))
   def apply(node: Node, tp: Type): Any = {
      // module = companion object
      val moduleSymbol = tp.typeSymbol.asClass.companion.asModule
      val applyMethodSymbol = tp.companion.member(TermName("apply")).asMethod
      val args = applyMethodSymbol.paramLists.flatten.map {arg =>
         (arg.name, xmlName(arg), arg.asTerm.info)
      }
      var children = node.child.toList.filterNot(_.isInstanceOf[Comment]).zipWithIndex
      /** returns the attribute value, or gets a text node using getChild, empty by default */
      def getAttributeOrChild(scalaKey: String): String = {
         val key = xmlName(scalaKey)
         val att = xml.attr(node, key)
         if (att != "")
            att
         else {
            val keyChild = getKeyedChild(key)
            keyChild match {
               case Some(Text(s)) => s
               case Some(n) => throw ExtractError(s"text node expected in child $key: $n")
               case None => ""
            }
         }
      }
      /** returns the first child with a given label and removes it from children */
      def getKeyedChild(scalaKey: String): Option[Node] = {
         val label = xmlName(scalaKey)
         val (child, i) = children.find {case (c,_) => c.label == label}.getOrElse {
            return None
         }
         children = children.filter(_._2 != i)
         child.child.toList match {
            case l if l.forall(_.isInstanceOf[SpecialNode]) => Some(Text(l.text)) // turn Text, EntityRef, Atom etc. into a single Text node
            case cn :: Nil => Some(cn)
            case _ => throw ExtractError(s"node fields must contain a single node: $child")
         }
      }
      /** returns the first child with a given label */
      def getNumberedChild(pos: Int): Node = {
         val (child, _) = children(pos)
         child
      }
      
      val values = args.map {
         case (n, nS, StringType) => getAttributeOrChild(nS)
         case (n, nS, IntType) =>
            val s = getAttributeOrChild(nS)
            try {s.toInt}
            catch {case _: Exception => throw ExtractError(s"integer expected at key $nS: $s")}
         case (n, nS, BoolType) =>
            val s = getAttributeOrChild(nS)
            s.toLowerCase match {
               case "true" => true
               case "false" | "" => false
               case b => throw ExtractError(s"boolean expected at key $nS: $b")
            }
         case (_, "children", _) =>
            children.map {c => apply(c._1)}
         case (_, NumberedChildName(pos), _) =>
            val childNode = getNumberedChild(pos)
            apply(childNode)
         case (n, nS, argTp) =>
            val childNode = getKeyedChild(nS).getOrElse {
               throw ExtractError(s"no attribute/child with key/label $nS (of type $argTp) found in $node")
            }
            apply(childNode)
      }
      val module = m.reflectModule(moduleSymbol).instance
      val applyMethod = m.reflect(module).reflectMethod(applyMethodSymbol)
      val result = applyMethod(values:_*)
      println("found: " + result)
      result
   }
   
   def apply(node: Node): Any = {
      val c = Class.forName(pkg + "." + scalaName(node.label))
      apply(node, m.classSymbol(c).toType)
   }
}

case class A(a: String, b: Int, c: Boolean, d: B, children: List[C])
case class B(a: String, children: List[B])
abstract class C
case class Ca(a: String) extends C
case class Cb(_0: C, _1: C) extends C

object Test {
   val n1 = <A a="a" b="1" c="true"><d><B><a>a</a></B></d><Ca a="a"/><Cb><Ca a="a0"/><Ca a="a1"/></Cb></A>
   def main(args: Array[String]) {
      val a = new XMLToScala("info.kwarc.mmt.api.utils").apply(n1, typeOf[A])
      println(a)
      assert(a == A("a", 1, true, B("a", Nil), List(Ca("a"), Cb(Ca("a0"), Ca("a1")))))
   }
}