package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import scala.xml._

/** A RenderingHandler collects output generated during presentation */
abstract class RenderingHandler {
   /** output a string */
   def apply(s : String)
   /** output an XML node */
   def apply(N : Node)
   /** output a literal */
   def apply(l : Literal) : Unit = l match {
      case StringLiteral(s) => apply(s)
      case XMLLiteral(n) => apply(n)
      case ValueLiteral(v) => apply(v.toString)
      case Omitted => ()
   }
   /** start an XML element, use empty prefix for unprefixed elements, content is provided by succeeding calls */
   def elementStart(prefix : String, label : String)
   /** start an XML attribute, use empty prefix for unprefixed attributes, value is provided by succeeding calls */
   def attributeStart(prefix : String, name : String)
/*   def procinstr(target : String, text : String)
   def xmldecl(version : String, encoding : String) */
   /** end the current attribute */
   def attributeEnd()
   /** end the current element */
   def elementEnd()
}

/** collects the output as text, XML is converted to a string, does not specify what to do with that string */
abstract class TextHandler extends RenderingHandler {
   private var openTag : Boolean = false
   private var openAtt : Boolean = false
   private var openElements : List[String] = Nil
   protected def prefixOpt(s: String) = if (s == "") "" else (s + ":")
   def write(s: String) : Unit 
   def apply(N : Node) {apply(N.toString)}
   def apply(s: String) {
      if (openTag && ! openAtt) {
        write(">\n")
        openTag = false
      }
      val escape = if (openElements != Nil) scala.xml.Utility.escape(s) else s
      write(escape)
   }
/*   def xmldecl(version : String, encoding : String) {
      write("<?xml version=\"" + version + "\" encoding=\"" + encoding + "\"?>")
   }
   def procinstr(target : String, text : String) {
      write("<?" + target + " " + text + "?>")
   } */
   def elementStart(prefix : String, label : String) {
      if (openAtt) attributeEnd
      if (openTag) write(">\n")
      val pl = prefixOpt(prefix) + label
      write("<" + pl + "\n")
      openTag = true
      openElements = pl :: openElements
   }
   def elementEnd() {
      if (openElements == Nil) return
      if (openTag) write("\n/>\n")
      else write("\n</" + openElements.head + ">\n")
      openTag = false
      openElements = openElements.tail
   }
   def attributeStart(prefix : String, name : String) {
      if (openAtt) attributeEnd
      write(prefixOpt(prefix) + name + "=\"")
      openAtt = true
   }
   def attributeEnd() {
      write("\"\n")
      openAtt = false
   }
}

/** writes text output to the console */
object ConsoleWriter extends TextHandler {
   def write(s : String) {print(s)}
}

/** writes text output to a file */
class FileWriter(val filename : java.io.File) extends TextHandler with utils.FileWriter {
	def write(s : String) {
	  file.print(s)
	  }
}

/** excpetion thrown by XML builder if methods are called that would lead to ill-formed XML */
case object XMLError extends java.lang.Throwable

/** collects the output as XML and stores it in memory */
class XMLBuilder extends RenderingHandler {
   private var preamble = ""
   private var state : List[Elem] = Nil
   private var inAttribute : Option[(String,String)] = None
   private var attribute = ""
   protected def prefixOpt(s: String) = if (s == "") null else s
   def apply(str : String) =
      inAttribute match {
         case None => state match {
            case Nil => preamble = preamble + str
            case Elem(p,l,a,s,c @ _*) :: rest => state = Elem(p,l,a,s, c ++ List(scala.xml.Text(str)) : _*) :: rest
         }
         case Some(_) => attribute = attribute + str
      }
   def apply(N : Node) =
      inAttribute match {
         case None => state match {
            case Nil => N match {
               case e : Elem => state = List(e)
               case _ => preamble = preamble + N.toString
            }
            case Elem(p,l,a,s,c @ _*) :: rest => state = Elem(p,l,a,s, c ++ List(N) : _*) :: rest
         }
      case Some(_) => throw XMLError
   }
   
/*   def xmldecl(version : String, encoding : String) {
      document.version = Some(version)
      document.encoding = Some(encoding)
   }
   def procinstr(target : String, text : String) {
      if (state.length > 0) throw XMLError
      document.children = document.children ++ scala.xml.ProcInstr(target, text) 
   } */
   def elementStart(prefix : String, label : String) {
      inAttribute match {
         case None => state = Elem(prefixOpt(prefix), label, Null, TopScope) :: state
         case Some(_) => throw XMLError
      }
      
   }
   def elementEnd() {
      inAttribute match {
         case None =>
           state match {
             case Nil => ()
             case _ :: Nil => ()
             case e :: Elem(p,l,a,s,c @ _*) :: rest => state = Elem(p,l,a,s, c ++ List(e) : _*) :: rest
           }
         case _ => throw XMLError
      }
   }
   def attributeStart(prefix : String, name : String) {
      inAttribute match {
         case None => 
            inAttribute = Some(prefix, name)
            attribute = ""
         case Some(_) => throw XMLError
      }
   }
   def attributeEnd() {
      inAttribute match {
         case None => throw XMLError
         case Some((prefix, name)) => state match {
            case Nil => throw XMLError
            case e :: rest =>
               val att = if (prefix == "") new UnprefixedAttribute(name, attribute, Null)
                         else new PrefixedAttribute(prefix, name, attribute, Null)
               state = (e % att) :: rest
         }
      }
      inAttribute = None
   }
   /** returns the output XML node */
   def get() : Node = {
     if (inAttribute != None || state.length > 1) throw XMLError
     if (state == Nil) return scala.xml.Text(preamble)
     state(0) //TO DO: print preamble
   }
}