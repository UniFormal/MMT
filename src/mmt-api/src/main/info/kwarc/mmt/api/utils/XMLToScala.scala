package info.kwarc.mmt.api.utils

import scala.util.Success
import scala.xml._
import scala.reflect.runtime.universe._

/** a marker for classes representing groups of attributes/children, used by [[XMLToScala]] */
trait Group

/** thrown on all errors, e.g., when expected classes are not present or XML nodes are not present */
abstract class ExtractError(val msg: String) extends Exception(msg)

/** default case */
case class FatalExtractError(msg: String) extends Exception(msg)

/** thrown on all errors where extraction succeeded but the wrong type was found; these can be backtracked, e.g., if they follow a list */
case class BacktrackableExtractError(token: Int, msg: String) extends Exception(msg)


/**
 * This class uses Scala reflection to parse XML into user-defined case classes.
 *
 * The effect is that the classes encode the XML grammar, and the parser picks the corresponding class for each XML tag.
 * This is kind of the opposite of generating a parser, where the grammar is fixed and the classes are generated.
 *
 * @param pkg the full name of the package in which the case classes are declared
 *
 * An XML node is parsed into a case class instance as follows
 *  * the case class whose name is the tag name is used except that
 *    * XML - is treated as Scala _
 *    * XML names that are reserved words in Scala are prefixed by 'XML' in Scala
 *    * if a Scala class with name N does not exist, N_ is tried as a fallback (e.g., useful if N cannot be a Scala class name)
 *  * the arguments to the class (technically: the single apply method of the companion object) are computed as follows:
 *   * k: String: the value of the attribute with key k, or the content of the child with tag k, ("" if neither present)
 *   * k: Int: accordingly
 *   * k: Boolean: accordingly (false if not present)
 *   * t: A where A is one the case classes: recursive call on the single child of the child with tag t
 *   * t: Option[A]: accordingly, but the child may be absent
 *   * t: List[A]: accordingly but the child may have any number of children
 *   * t: G <: Group:  All arguments of G are computed and an instance of G constructed.
  *        Effectively, all fields of G are imported into the current case class.
 *   * _a: A, Option[A], List[A] (where A may be a subtype of Group): like the t-cases above but taking the next available child(ren) of type A
 *   The type A does not have to be a case class, e.g., the elements in a list can have the tags of subclasses of A.
 */
class XMLToScala(pkg: String) {
   /** mirrors are used to evaluated reflected objects */
   private val m = runtimeMirror(getClass.getClassLoader)

   // definitions for giving names to reflected Scala types

   /** It's non-trivial to construct Type programmatically. So we take them by reflecting Dummy */
   private case class Dummy(a: Int, b: Boolean, c: List[Int], d: Option[Int], e: Group, f: String, g:BigInt, h: scala.xml.Node)
   /** the argument types of Dummy */
   private val dummyTypes = typeOf[Dummy].companion.member(TermName("apply")).asMethod.paramLists.flatten.toList.map(_.asTerm.info)
   /** the Type of Int (strangely != typeOf[Int]) */
   private val IntType = dummyTypes(0)
   /** the Type of Boolean */
   private val BoolType = dummyTypes(1)
   /** the Type of String */
   private val StringType = dummyTypes(5)
   private val BigIntType = dummyTypes(6)
   private val NodeType = dummyTypes(7)
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

   // functions for mapping between XML and Scala names

   private val scalaReserved = List("var", "val", "def", "type", "class", "object")
   private val scalaEscapePrefix = "XML"
   /** convert Scala id names to xml tag/key names */
   def xmlName(s: String) = {
     val s2 = if (s.startsWith(scalaEscapePrefix)) s.substring(scalaEscapePrefix.length) else s
     s2.replace("_", "-")
   }
   /** convert xml tag/key names to Scala id names */
   def scalaName(s: String) = {
     val s2 = s.replace("-", "_")
     if (scalaReserved contains s2) scalaEscapePrefix + s2 else s2
   }

   /** (non-recursively) remove comments and whitespace-only text nodes */
   private def cleanNodes(nodes: List[Node]) = nodes.filter {
      case _:Comment => false
      case Text(s) if s.trim == "" => false
      case _ => true
   }

   // main functions

   /** read and parse a file */
   def apply(file: File) : Any = apply(xml.readFile(file))
   /** parse a Node */
   def apply(node: Node): Any = {
      val c = try {
         Class.forName(pkg + "." + scalaName(node.label))
      } catch {
         case e: java.lang.ClassNotFoundException => throw FatalExtractError("no class for " + node)
      }
      val foundType = m.classSymbol(c).toType
      apply(node, foundType)
   }

   // abstraction from Scala reflection

   /** represents an argument of a reflected method
    *  @param scalaName Scala name of the argument
    *  @param xmlKey corresponding xml name, possibly with leading/trailing underscores
    *  @param scalaType Scala's internal representation of the type
    */
   private case class Argument(scalaName: String, xmlKey: String, scalaType: Type)

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
         val n = arg.name.decodedName.toString
         // convert to xmlName except for initial/terminal _
         val init = n.takeWhile(_ == '_')
         val term = n.reverse.takeWhile(_ == '_')
         val n2 = n.substring(init.length, n.length-term.length)
         Argument(n, init+xmlName(n2)+term, arg.asTerm.info)
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
            throw FatalExtractError(s"$msg\nexpected: $exp\nfound$found")
         case e: java.lang.reflect.InvocationTargetException =>
            // reflection succeeded, but errors during object initialization
            throw e.getCause
      }
      result
   }

   // the method doing the actual work

   /** a value that distinguishes different invocations of apply, used for backtracking */
   private var token = -1
   private def newToken = {token += 1; token}

   /** parse a Node of expected Type expType */
   private def apply(node: Node, expType: Type, backtrackingToken: Int = -1): Any = {
      //println(node.toString+"\n - "+node.child.map(x => x.isInstanceOf[SpecialNode].toString+":"+x.toString).mkString("\n - "))
      //println(" - - "+node.attributes.map(a =>
      //   a.key + " = " + node.attributes.collectFirst{case p if p.key==a.key || p.key.endsWith(":"+a.key) => p.value.toString}))
      if (node.isInstanceOf[Text]) {
         // treat text nodes as Strings
         return node.text
      }
      val name = pkg + "." + scalaName(node.label)
      val c = try {
         Class.forName(name)
      } catch {
         case _: java.lang.ClassNotFoundException =>
           try {
             // try again with a variant name, this allows for otherwise impossible Scala names (e.g., if there XML labels that only differ in capitalization)
             Class.forName(name + "_")
           } catch {
             case _: java.lang.ClassNotFoundException =>
               throw FatalExtractError("no class for " + node)
           }
      }
      val foundType = m.classSymbol(c).toType
      if (! (foundType <:< expType)) {
         val msg = s"expected $expType\nfound $foundType in $node"
         throw BacktrackableExtractError(backtrackingToken, msg)
      }

      // state
      // the remaining children of node (removed once processed)
      var children = cleanNodes(node.child.toList).zipWithIndex
      def childrenString = children.map{case (c,i) => s"\n$i:$c"}.mkString(",")
      // the used attributes of node (added once processed)
      var attributesTaken: List[String] = Nil
      // wrapping this around some code that affects the state, restores the state if a BacktrackableExtractError is encountered
      def backtrackable[A](body: => A): A = {
        val oldChildren = children
        val oldAttributesTaken = attributesTaken
        try {body}
        catch {case e: BacktrackableExtractError =>
           children = oldChildren
           attributesTaken = oldAttributesTaken
           throw e
        }
      }

      /** finds the string V by looking at (i) key="V" (ii) <key>V</key> (iii) "" */
      def getAttributeOrChild(key: String): String = {
         // special case: _key yields the body of the node, which must be text
         if (key.startsWith("_")) {
            children.map(_._1) match {
               case nodes if nodes.forall(_.isInstanceOf[SpecialNode]) =>
                  // TODO sure about that?
                  children = Nil
                  return nodes.text
               case nodes => throw FatalExtractError(s"text node expected in child $key: $nodes")
            }
         }
         val att = node.attributes.collectFirst{case p if p.key == key || p.key.endsWith(":"+key) => p.value.toString}.getOrElse("")
         if (att != "" && !attributesTaken.contains(key)) {
            attributesTaken ::= key
            att
         } else {
            val keyChildren = getKeyedChild(key, false)
            keyChildren match {
               case Some(Text(s) :: Nil) => s
               case Some(nodes) if nodes.forall(_.isInstanceOf[SpecialNode]) => nodes.text // turn Text, EntityRef, Atom etc. into a single Text node
               case Some(nodes) => throw FatalExtractError(s"text node expected in child $key: $nodes")
               case None => ""
            }
         }
      }
      /** finds the nodes NODES by looking at <label>NODES</label> */
      def getKeyedChild(label: String, keepLabel: Boolean): Option[List[Node]] = {
         val (child, i) = children.find {case (c,_) => c.label == label}.getOrElse {
            return None
         }
         children = children.filter(_._2 != i)
         if (keepLabel)
            Some(List(child))
         else
            Some(cleanNodes(child.child.toList))
      }

      /**
       * gets either the next child or the next group of children
       * if a group fails, the state is restored
       */
      def getSingleOrGroupChild(expType: Type, token: Int): Any = {
        if (expType <:< GroupType) {
          backtrackable {
            makeInstance(expType)(getArgumentValue)
          }
        } else {
          val v = apply(children.head._1, expType, token)
          children = children.tail
          v
        }
      }

      /** compute argument values one by one depending on the needed type */
      def getArgumentValue(arg: Argument): Any = arg match {
         // base type: use getAttributeOrChild
         case Argument(_, nS, _) if showRaw(arg.scalaType) == showRaw(StringType) =>
            // in jEdit, arg.scalaType == StringType is false; no idea why
            getAttributeOrChild(nS)
         case Argument(_, nS, IntType) =>
            val s = getAttributeOrChild(nS)
            if (s == "") 0 else
               try {s.toInt}
               catch {case _: Exception => throw FatalExtractError(s"integer expected at key $nS: $s")}
         case Argument(_, nS, BigIntType) =>
            val s = getAttributeOrChild(nS)
            if (s == "") 0 else
               try {BigInt(s)}
               catch {case _: Exception => throw FatalExtractError(s"BigInt expected at key $nS: $s")}
         case Argument(_, nS, BoolType) =>
            val s = getAttributeOrChild(nS)
            s.toLowerCase match {
               case "true" => true
               case "false" | "" => false
               case b => throw FatalExtractError(s"boolean expected at key $nS: $b")
            }
         case Argument(_, nS, NodeType) =>
            getKeyedChild(nS, true) match {
              case Some(List(node)) => node
              case None => scala.xml.Text("")
              case r => throw FatalExtractError(s"single element expected at key $nS: $childrenString")
            }
         // special argument _name: first remaining node(s)
         case Argument(_, nS, argTp) if nS.startsWith("_") =>
            val token = newToken
            argTp match {
               case ListType(elemType) =>
                  var vs: List[Any] = Nil
                  // take as many children as type-check
                  While (children.nonEmpty) {
                     try {
                      vs ::= getSingleOrGroupChild(elemType, token)
                    } catch {case e: BacktrackableExtractError if e.token == token =>
                      While.break
                    }
                  }
                  vs.reverse
               case OptionType(elemType) =>
                  if (children.isEmpty)
                    None
                  else {
                     // take the next child if it type-checks, otherwise None
                     try {
                       val s = getSingleOrGroupChild(elemType, token)
                       Some(s)
                     } catch {case e:BacktrackableExtractError if e.token == token =>
                        None
                     }
                  }
               case _ =>
                  if (children == Nil) {
                     throw FatalExtractError(s"no child left for $nS (of type $argTp) in " + node)
                  }
                  getSingleOrGroupChild(argTp, token)
            }
         // default case: use getKeyedChild
         case Argument(_, nS, argTp) =>
            lazy val noNode      = FatalExtractError(s"no child with label $nS (of type ${show(argTp)} found in $node")
            lazy val wrongLength = FatalExtractError(s"$nS does not have exactly one child (of type $argTp) in $node")
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
                  else childNodes.map(apply(_, elemType))
               // Option[A]
               case OptionType(elemType) =>
                  if (omitted) {
                    if (showRaw(elemType) == showRaw(StringType)) {
                      val s = try {getAttributeOrChild(nS)} catch {case e: FatalExtractError => ""}
                      if (s.nonEmpty) Some(s) else None
                    } else None
                  } else if (childNodes.length == 1)
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
      if (children.nonEmpty)
         throw FatalExtractError(s"children left after constructing $res: " + childrenString)
      res
   }
}

object XMLToScala {
   def checkString(value: String, allowed: String*): Unit = {
     if (! (allowed contains value))
         throw FatalExtractError(s"illegal string value: $value; expected: ${allowed.mkString(",")}")
   }
}

// case classes for testing

case class A(a: String, b: Int, c: Boolean, d: B, e: List[B], f: Option[B], g: Option[C], _cs: List[C])
case class B(a: String, _bs: List[B])

abstract class C
case class Ca(a: String) extends C
case class Cb(_c1: C, _c2: C) extends C
case class Cc(a: String, bc: G, d: String) extends C
case class Cd(_bs: List[B], _c: Option[C]) extends C
case class Ce(_js: List[J], _c: Option[C]) extends C
case class G(b: String, h: H) extends Group
case class H(c: String) extends Group
case class J(_b: B, _c: C) extends Group
// test cases
object Test {
   val n1 = <A a="a" b="1" c="true">
              <d><B><a>a</a></B></d>
              <e><B a="a1"/> <B a="a2"/></e>
              <g><Ca a="a" b="b" c="c"/></g>
              <Ca a="a"/>
              <Cb><Ca a="a0"/> <Ca a="a1"/></Cb>
              <Cc a="a" b="b" c="c" d="d"/>
              <Cd><B a="a1"/><B a="a2"/><Ca a="Ca"/></Cd>
              <Ce><B a="a1"/><Ca a="Ca1"/><B a="a2"/><Ca a="Ca2"/><Ca a="Ca"/></Ce>
            </A>
   val r1 = A("a", 1, true,
               B("a", Nil),
               List(B("a1",Nil), B("a2",Nil)),
               None,
               Some(Ca("a")),
               List(Ca("a"), Cb(Ca("a0"), Ca("a1")), Cc("a", G("b", H("c")), "d"),
                    Cd(List(B("a1",Nil), B("a2", Nil)), Some(Ca("Ca"))),
                    Ce(List(J(B("a1",Nil), Ca("Ca1")), J(B("a2",Nil), Ca("Ca2"))), Some(Ca("Ca")))
               )
             )
   def main(args: Array[String]): Unit = {
      val a1 = new XMLToScala("info.kwarc.mmt.api.utils").apply(n1)
      println(a1)
      assert(a1 == r1)
   }
}
