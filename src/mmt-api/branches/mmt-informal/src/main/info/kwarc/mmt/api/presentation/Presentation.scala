package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import scala.xml.{Node,NodeSeq}

/** This is the type of a simple language of presentation expressions that may be used in Notation and are evaluated by Presenter */
sealed abstract class Presentation {
   /** concatenation */
   def +(that : Presentation) = that match {
      case PList(l) => PList(this :: l)
      case p => PList(List(this,p)) 
   }
   /** fill the Hole s of an expression */
   def fill(plug : Presentation*) : Presentation = this
}

/** produces a string */
case class Text(text : String) extends Presentation

/** produces an XML element */
case class Element(prefix : String, label : String, attributes : List[Attribute], children : List[Presentation])
           extends Presentation {
   override def fill(plug : Presentation*) =
      Element(prefix, label, attributes.map(_.fill(plug : _*)), children.map(_.fill(plug : _*)))
}

/*
case class XMLDecl(version : String, encoding : String) extends Presentation
case class ProcInstr(target : String, text : String) extends Presentation
*/
/** produces an XML attribute */
case class Attribute(prefix : String, name : String, value : Presentation) {
   def fill(plug : Presentation*) = Attribute(prefix, name, value.fill(plug : _*))
}

/** groups a list of presentation expressions that are rendered in order into a single expressions */
case class PList(items : List[Presentation]) extends Presentation {
   //override concatenation to avoid nested PLists
   override def +(that : Presentation) = that match {
      case PList(l) => PList(items ::: l)
      case _ => PList(items ::: List(that))
   }
   override def fill(plug : Presentation*) = PList(items.map(_.fill(plug : _*)))
}

/** test where component pos is equal to Omitted and branch accordingly */
case class If(pos : CIndex, test: String, yes : Presentation, no : Presentation) extends Presentation {
   override def fill(plug : Presentation*) = If(pos, test, yes.fill(plug : _*), no.fill(plug : _*))
}

/** test whether pos is Obj with head Some(path) and branch accordingly */
case class IfHead(pos : CIndex, path : Path, yes : Presentation, no : Presentation) extends Presentation {
   override def fill(plug : Presentation*) = IfHead(pos, path, yes.fill(plug : _*), no.fill(plug : _*))
}

/** loop over a presentation expression, Recurse Neighbor may be used to recurse into individual components 
 * @param begin the first component
 * @param pre output produced at the beginning (only if iteration is non-empty)
 * @param end the last component
 * @param post output produced at the end (only if iteration is non-empty)
 * @param step the increment (may be negative)
 * @param sep output produced between two iterations
*/
case class Components(begin : CIndex, pre : Presentation, end : CIndex, post : Presentation, step : Int, sep : Presentation, body : Presentation)
           extends Presentation {
   override def fill(plug : Presentation*) = {
      val List(pre2, post2, sep2, body2) = List(pre, post, sep, body).map(_.fill(plug : _*))
      Components(begin, pre2, end, post2, step, sep2, body2)
   }
}

/** short hand to iterate over a list of components */
object Iterate {
	def apply(b : CIndex, e : CIndex, s : Presentation, p : Option[Precedence]) =
		Components(b, Presentation.Empty, e, Presentation.Empty, 1, s, Recurse(p))
}
/** short hand to recurse into a single component */
object Component {
	def apply(index : CIndex, q : Option[Precedence]) = Iterate(index, index, Presentation.Empty, q)
}

/** produces the current index in a loop */
case object Index extends Presentation

/** recurses into a component 
 * @param offset the index of the component relative to the current index in the loop
 * @param prec the input precedence to be used (no brackets if None)
 */
case class Neighbor(offset : Int, prec : Option[Precedence]) extends Presentation
/** short hand to recurse into the current component */
object Recurse {
	def apply(p : Option[Precedence]) = Neighbor(0, p)
}

/** like Components but nests instead of concatenating, step should contain Hole to indicate nesting */
case class Nest(begin : CIndex, end : CIndex, step : Presentation, base : Presentation) extends Presentation {
   override def fill(plug : Presentation*) = Nest(begin, end, step.fill(plug : _*), base.fill(plug : _*))
}

/** produces the position of the current expression as a string, can be used for unique ids */
case object Id extends Presentation

/** produces the name of the used style */
case object TheNotationSet extends Presentation

/** used to indicate a hole that can be filled later, used with Nest and Fragment
 * @param index the index in the list of filling expressions that determines which expression is used to fill this hole
 * @param default the expression used to fill the hole if not enough filling expressions are provided 
 */
case class Hole(index : Int, default : Presentation) extends Presentation {
   override def fill(plug : Presentation*) = if (plug.length > index) plug(index) else default 
}

/** generates a unique ID that is available during the recursion */
case class GenerateID(name: String, scope: Presentation) extends Presentation {
   override def fill(plug : Presentation*) = GenerateID(name, scope.fill(plug : _*))
}

/** refers to a previously generated ID */
case class UseID(name: String) extends Presentation

case class Compute(index : Option[CIndex], function : String) extends Presentation

/** a call to a macro that is defined as fragment notation 
 * @param name the used notation will have role "fragment:name"
 * @param args the list of arguments that fill the holes in that notation
 */
case class Fragment(name : String, args : Presentation*) extends Presentation {
   override def fill(plug : Presentation*) = 
      Fragment(name, args.map(_.fill(plug : _*)) : _*) 
}

object ArgSep {
	def apply() = Fragment("argsep")
}
object OpSep {
	def apply() = Fragment("opsep")
}
object ImpArgs {
	def apply(arg : Presentation*) = Fragment("impargs", arg : _*)
}
object MathArgs {
	def apply(arg : Presentation*) = Fragment("mathargs", arg : _*)
	def unapply(p : Presentation) : Option[Seq[Presentation]] = p match {
		case Fragment("mathargs", a @ _*) => Some(a)
		case _ => None
	}
}

object Brackets {
	def apply(arg : Presentation) = Fragment("brackets", arg)
}
object EBrackets {
	def apply(arg : Presentation, elevel : Int) = Fragment("ebrackets", arg, Text(elevel.toString))
}
object NoBrackets {
	def apply(arg : Presentation) = Fragment("nobrackets", arg)
}

/** Helper object for presentation s*/
object Presentation {
   def Empty = PList(Nil)
   private def cindex(s : String) : CIndex = {
      try {NumberedIndex(s.toInt)}
      catch {case _ => NamedIndex(s)}//throw ParseError("illegal index: " + s)}
   }
   private def int(s : String) : Int = {
      try {s.toInt}
      catch {case _ => throw ParseError("illegal index: " + s)}
   }
   private def precOpt(s : String) = s match {
      case "" => None
      case s => Some(Precedence.parse(s))
   }
   private def parseTest(n: Node) = {
      val t = xml.attr(n, "test")
      if (List("present", "atomic") contains t) t
      else throw ParseError("invalid test " + t)
   }
   /** parses presentation from XML */
   def parse(N : NodeSeq)(implicit inAttr: Boolean = false) : Presentation = {
      val ps = for (n <- N) yield n match {
         case <text/> => Text(xml.attr(n, "value"))
/*         case <procinstr/> => ProcInstr(xml.attr(n, "target"), xml.attr(n, "text")) */
         case <element>{pres @ _*}</element> =>
            if (inAttr) throw ParseError("element not permitted within attribute")
            var atts : List[Attribute] = Nil
            var elems : List[Presentation] = Nil
            for (c <- n.child) c match {
               case <attribute/> =>
                  atts ::= Attribute(xml.attr(c, "prefix"), xml.attr(c, "name"), Text(xml.attr(c, "value")))
               case <attribute>{value @ _*}</attribute> =>
                  atts ::= Attribute(xml.attr(c, "prefix"), xml.attr(c, "name"), parse(value)(true))
               case p => elems ::= parse(p)
            }
            Element(xml.attr(n,"prefix"), xml.attr(n,"name"), atts.reverse, elems.reverse)
         case <attribute>{pres @ _*}</attribute> => throw ParseError("attribute only permitted within element")
         case <recurse/> =>
            Neighbor(int(xml.attr(n, "offset", "0")), precOpt(xml.attr(n, "precedence")))
         case <hole>{child @ _*}</hole> => Hole(int(xml.attr(n, "index", "0")), parse(child))
         case <id/> => Id 
         case <index/> => Index
         case <nset/> => TheNotationSet 
         case <component/> =>
            val index = cindex(xml.attr(n, "index"))
            Component(index, precOpt(xml.attr(n, "precedence")))
         case <components>{child @ _*}</components> =>
            val begin = cindex(xml.attr(n, "begin", "0"))
            val end = cindex(xml.attr(n, "end", "-1"))
            val step = int(xml.attr(n, "step", "1"))
            var (pre : Presentation, post : Presentation, sep : Presentation, body : Presentation) = (Empty, Empty, Empty, Recurse(None))
            for (c <- child) c match {
               case <separator>{s @ _*}</separator> => sep = parse(s)
               case <body>{b @ _*}</body> => body = parse(b)
               case <pre>{p @ _*}</pre> => pre = parse(p)
               case <post>{p @ _*}</post> => post = parse(p)
               case _ => throw ParseError("illegal child of components: " + c) 
            }
            Components(begin, pre, end, post, step, sep, body)
         case <nest><base>{base @ _*}</base><step>{step @ _*}</step></nest> =>
            Nest(cindex(xml.attr(n, "begin")), cindex(xml.attr(n, "end")), parse(step), parse(base))
         case <if><then>{yes @ _*}</then><else>{no @ _*}</else></if> =>
            val test = parseTest(n)
         	If(cindex(xml.attr(n,"index")), test, parse(yes), parse(no))
         case <if><then>{yes @ _*}</then></if> =>
            val test = parseTest(n)
            If(cindex(xml.attr(n,"index")), test, parse(yes), Empty)
         case <ifhead><then>{yes @ _*}</then><else>{no @ _*}</else></ifhead> =>
         	IfHead(cindex(xml.attr(n,"index")), Path.parse(xml.attr(n,"path"), mmt.mmtbase), parse(yes), parse(no))
         case <ifhead><then>{yes @ _*}</then></ifhead> =>
            IfHead(cindex(xml.attr(n,"index")), Path.parse(xml.attr(n,"path"), mmt.mmtbase), parse(yes), Empty)
         //case <ifpresent>{yes @ _*}</ifpresent> =>
            //IfPresent(int(xml.attr(n,"index")), parse(yes), Empty)
         case <generateid>{scope @ _*}</generateid> => GenerateID(xml.attr(n, "name"), parse(scope))
         case <useid/> => UseID(xml.attr(n, "name"))
         case <compute/> => 
            val i = xml.attr(n, "index")
            val ci = if (i == "") None else Some(cindex(i))
            Compute(ci, xml.attr(n, "function"))
         case <fragment>{child @ _*}</fragment> =>
            val args = if (! child.isEmpty && child(0).label != "arg")
               Seq(parse(child))
            else {
               child map {
                  case <arg>{arg @ _*}</arg> => parse(arg)
                  case a => throw ParseError("illegal argument: " + a)
               }
            }
            Fragment(xml.attr(n,"name"), args : _*)
         case e @ scala.xml.Elem(prefix, label, atts, scope, _*) if prefix != null =>
            val pAtts = atts map {
               case scala.xml.PrefixedAttribute(p, k, v, _) => Attribute(p, k, Text(v.text)) 
               case scala.xml.UnprefixedAttribute(k, v, _) => Attribute("", k, Text(v.text))
            }
            Element(prefix, label, pAtts.toList, (e.child map parse).toList)
         case scala.xml.Comment(_) => Empty
         case _ => throw ParseError("illegal presentation item: " + n)
      }
      if (ps.length == 1)
         ps(0)
      else
         PList(ps.toList)
   }
}