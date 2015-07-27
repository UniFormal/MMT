package info.kwarc.mmt.api.utils

import scala.xml._
import scala.reflect.runtime.universe._

/** a marker for classes representing groups of attributes/children, used by [[XMLToScala]] */
trait Group

/** thrown on all errors, e.g., when expected classes are not present or XML nodes are not present */
case class ExtractError(msg: String) extends Exception(msg)
   

/**
 * This class uses Scala reflection to parse XML into user-defined case classes.
 * 
 * The effect is that the classes encode the XML grammar, and the parser picks the corresponding class for each XML tag.
 * This is kind of the opposite of generating a parser, where the grammar is fixed and the classes are generated.  
 * 
 * @param pkg the full name of the package in which the case classes are declared
 * 
 * An XML node is parsed into an case class instance as follows
 *  * the case class whose name is the tag name is used (except that XML - is treated as Scala _)
 *  * the arguments to the class (technically: the single apply method of the companion object) are computed as follows:
 *   * k: String: the value of the attribute with key s, or the content of the child with tag k, ("" if neither present)
 *   * k: Int: accordingly
 *   * k: Boolean: accordingly (false if not present)
 *   * t: A where A is one the case classes: recursive call on the single child of the child with tag t
 *   * t: Option[A]: accordingly, but the child may be absent
 *   * t: List[A]: accordingly but the child may have any number of children
 *   * _a: A, Option[A], List[A]: like the t-cases above but taking the next available child(ren)
 *      These must come last.
 *   The type A does not have to be a case class, e.g., the elements in a list can have the tags of subclasses of A.
 */
class XMLToScala(pkg: String) {
   /** mirrors are used to evaluated reflected objects */
   private val m = runtimeMirror(getClass.getClassLoader)

   /** It's non-trivial to construct Type programmatically. So we take them by reflecting Dummy */
   private case class Dummy(a: Int, b: Boolean, c: List[Int], d: Option[Int], e: Group, f: String)
   /** the argument types of Dummy */
   private val dummyTypes = typeOf[Dummy].companion.member(TermName("apply")).asMethod.paramLists.flatten.toList.map(_.asTerm.info)
   /** the Type of Int (strangely != typeOf[Int]) */
   private val IntType = dummyTypes(0)
   /** the Type of Boolean */
   private val BoolType = dummyTypes(1)
   /** the Type of String */
   private val StringType = dummyTypes(5)
   /** matches the Type of a unary type operator */
   private class TypeRefMatcher(sym: Symbol) {
      def unapply(tp: Type): Option[Type] = {
         tp match {
            case tr: TypeRef if tr.sym == sym => Some(tr.args.head)
            case _ => None
         }
      }
   }
   /** matches the Type of List[A] */
   private object ListType extends TypeRefMatcher(dummyTypes(2).asInstanceOf[TypeRef].sym)
   /** matches the Type of List[A] */
   private object OptionType extends TypeRefMatcher(dummyTypes(3).asInstanceOf[TypeRef].sym)
   /** the Type of Group */
   private val GroupType = dummyTypes(4)
      
   /** convert Scala id names to xml tag/key names */
   private def xmlName(s: String) = s.replace("_", "-")
   /** convert xml tag/key names to Scala id names */
   private def scalaName(s: String) = s.replace("-", "_")
   /** (non-recursively) remove comments and whitespace-only text nodes */
   private def cleanNodes(nodes: List[Node]) = nodes.filter {
      case _:Comment => false
      case Text(s) if s.trim == "" => false
      case _ => true
   }
   
   /** read and parse a file */
   def apply(file: File) : Any = apply(xml.readFile(file))
   /** parse a Node */
   def apply(node: Node): Any = {
      val c = try {
         Class.forName(pkg + "." + scalaName(node.label))
      } catch {
         case e: java.lang.ClassNotFoundException => throw ExtractError("no class for " + node)
      }
      val foundType = m.classSymbol(c).toType
      apply(node, foundType)
   }
   
   private case class Argument(scalaName: Name, userName: String, scalaType: Type)

   /** 
    *  abstracts away Scala's reflection boilerplate
    *  @param tp the case class to instantiate
    *  @param obtain computes the argument value for each argument declaration of that case class
    *  @return the instance of that case class using the obtained argument values
    */
   private def makeInstance(tp: Type)(obtain: Argument => Any): Any = {
      /* Type = Scala type of reflected Scala types
       * Symbol = reflection of a declaration
       * module = companion object
       */
      // the symbol of the companion object of tp
      val moduleSymbol = tp.typeSymbol.asClass.companion.asModule
      // the symbol of its apply method
      val applyMethodSymbol = tp.companion.member(TermName("apply")).asMethod
      // the arguments Name:Type of the apply methods, and the string representation of the Name
      val arguments: List[Argument] = applyMethodSymbol.paramLists.flatten.map {arg =>
         Argument(arg.name, arg.name.decodedName.toString, arg.asTerm.info)
      }
      val values = arguments map obtain
      // evaluate moduleSymbol (to a singleton class) and get its runtime instance 
      val module = m.reflectModule(moduleSymbol).instance
      // evaluate the apply method of the instance
      val applyMethod = m.reflect(module).reflectMethod(applyMethodSymbol)
      // call the apply method on the values computed above
      val result = try {
         applyMethod(values:_*)
      } catch {
         case e: java.lang.IllegalArgumentException =>
            // the values don't conform to the expected types
            // this should never happen if apply is implemented correctly
            val msg = println(s"error creating value of type $tp")
            val exp = arguments.map(a => a.scalaType.toString)
            val found = values.map(_.toString)
            throw ExtractError(s"$msg\nexpected: $exp\nfound$found")
         case e: java.lang.reflect.InvocationTargetException =>
            // reflection succeeded, but errors during object initialization
            throw e.getCause
      }
      result
   }

   /** parse a Node of expected Type expType */
   private def apply(node: Node, expType: Type): Any = {
      if (node.isInstanceOf[Text])
         // treat text nodes as Strings
         return node.text
      val c = try {
         Class.forName(pkg + "." + scalaName(node.label))
      } catch {
         case e: java.lang.ClassNotFoundException => throw ExtractError("no class for " + node)
      }
      val foundType = m.classSymbol(c).toType
      if (! (foundType <:< expType))
         throw ExtractError(s"expected $expType\nfound $node")
      // the remaining children of node (removed once processed)
      var children = cleanNodes(node.child.toList).zipWithIndex
      // the used attributes of node (added once processed)
      var attributesTaken: List[String] = Nil
      /** finds the string V by looking at (i) key="V" (ii) <key>V</key> (iii) "" */
      def getAttributeOrChild(scalaKey: String): String = {
         // special case: _key yields the body of the node, which must be text
         if (scalaKey.startsWith("_")) {
            children.map(_._1) match {
               case nodes if nodes.forall(_.isInstanceOf[SpecialNode]) =>
                  // TODO sure about that?
                  children = Nil
                  return nodes.text
               case nodes => throw ExtractError(s"text node expected in child $scalaKey: $nodes")
            }
         }
         val key = xmlName(scalaKey)
         val att = xml.attr(node, key)
         if (att != "" && !attributesTaken.contains(key)) {
            attributesTaken ::= key
            att
         } else {
            val keyChildren = getKeyedChild(key, false)
            keyChildren match {
               case Some(Text(s) :: Nil) => s
               case Some(nodes) if nodes.forall(_.isInstanceOf[SpecialNode]) => nodes.text // turn Text, EntityRef, Atom etc. into a single Text node
               case Some(nodes) => throw ExtractError(s"text node expected in child $key: $nodes")
               case None => ""
            }
         }
      }
      /** finds the nodes NODES by looking at (i) <label>NODES</label> (ii) None */
      def getKeyedChild(scalaKey: String, keepLabel: Boolean): Option[List[Node]] = {
         val label = xmlName(scalaKey)
         val (child, i) = children.find {case (c,_) => c.label == label}.getOrElse {
            return None
         }
         children = children.filter(_._2 != i)
         if (keepLabel)
            Some(List(child))
         else
            Some(cleanNodes(child.child.toList))
      }
      
      /** compute argument values one by one depending on the needed type */
      def getArgumentValue(arg: Argument): Any = arg match {
         // base type: use getAttributeOrChild
         case Argument(n, nS, _) if showRaw(arg.scalaType) == showRaw(StringType) =>
            // in jEdit, arg.scalaType == StringType is false; no idea why
            getAttributeOrChild(nS)
         case Argument(n, nS, IntType) =>
            val s = getAttributeOrChild(nS)
            if (s == "") 0 else
               try {s.toInt}
               catch {case _: Exception => throw ExtractError(s"integer expected at key $nS: $s")}
         case Argument(n, nS, BoolType) =>
            val s = getAttributeOrChild(nS)
            s.toLowerCase match {
               case "true" => true
               case "false" | "" => false
               case b => throw ExtractError(s"boolean expected at key $nS: $b")
            }
         // special argument _name: first remaining node
         case Argument(_, nS, argTp) if nS.startsWith("_") =>
            argTp match {
               case ListType(elemType) =>
                  val vs = children.map {c => apply(c._1, elemType)}
                  children = Nil
                  vs
               case OptionType(elemType) =>
                  if (children.isEmpty) None
                  else {
                     val (child,_) = children.head
                     children = children.tail
                     Some(apply(child, elemType))
                  }
               case _ =>
                  if (children == Nil) {
                     throw ExtractError(s"no child left for $nS (of type $argTp) in " + node)
                  }
                  val (child, _) = children.head
                  children = children.tail
                  apply(child,argTp)
            }
         // default case: use getKeyedChild
         case Argument(n, nS, argTp) =>
            lazy val noNode      = ExtractError(s"no child with label $nS (of type ${showRaw(argTp)} ${showRaw(StringType)} found in $node")
            lazy val wrongLength = ExtractError(s"$nS does not have exactly one child (of type $argTp) in $node")
            var omitted = false
            val (keepLabel, nS2) = if (nS.endsWith("_")) (true, nS.substring(0,nS.length-1)) else (false, nS)
            val childNodes = getKeyedChild(nS2, keepLabel).getOrElse {
               omitted = true
               Nil
            }
            // sub-split to handle special argument types
            argTp match {
               // List[A]
               case ListType(elemType) =>
                  if (omitted)
                     Nil
                  else
                     childNodes.map(apply(_, elemType))
               // Option[A]
               case OptionType(elemType) =>
                  if (omitted)
                     None
                  else if (childNodes.length == 1)
                     Some(apply(childNodes.head, elemType))
                  else
                     throw wrongLength
               // group A
               case _ if argTp <:< GroupType =>
                  makeInstance(argTp)(getArgumentValue)
               // A
               case _ =>
                  if (omitted)
                     throw noNode
                  else if (childNodes.length != 1)
                     throw wrongLength
                  else
                     apply(childNodes.head, argTp)
            }
      }
      val res = makeInstance(foundType)(getArgumentValue)
      if (!children.isEmpty)
         throw ExtractError(s"children left after constructing $res: " + children.map{case (c,i) => s"\n$i:$c"}.mkString(","))
      res
   }
}

object XMLToScala {
   def checkString(value: String, allowed: String*) {
     if (! (allowed contains value))
         throw ExtractError(s"illegal string value: $value; expected: ${allowed.mkString(",")}")
   }
}

// case classes for testing

case class A(a: String, b: Int, c: Boolean, d: B, e: List[B], f: Option[B], g: Option[C], _cs: List[C])
case class B(a: String, _bs: List[B])
abstract class C
case class Ca(a: String) extends C
case class Cb(_c1: C, _c2: C) extends C
case class Cc(a: String, bc: G, d: String) extends C
case class G(b: String, h: H) extends Group
case class H(c: String) extends Group

// test cases
object Test {
   val n1 = <A a="a" b="1" c="true">
              <d><B><a>a</a></B></d>
              <e><B a="a1"/> <B a="a2"/></e>
              <g><Ca a="a" b="b" c="c"/></g>
              <Ca a="a"/>
              <Cb><Ca a="a0"/> <Ca a="a1"/></Cb>
              <Cc a="a" b="b" c="c" d="d"/>
            </A>
   val r1 = A("a", 1, true,
               B("a", Nil),
               List(B("a1",Nil), B("a2",Nil)),
               None,
               Some(Ca("a")),
               List(Ca("a"), Cb(Ca("a0"), Ca("a1")), Cc("a", G("b", H("c")), "d"))
             )
   def main(args: Array[String]) {
      val a1 = new XMLToScala("info.kwarc.mmt.api.utils").apply(n1)
      println(a1)
      assert(a1 == r1)
   }
}