package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.MyList.{fromList}
import info.kwarc.mmt.api.utils._
import scala.xml.{Null, Node, NodeSeq, UnprefixedAttribute}

//TODO: merge with content import
/** import between two styles */
case class NotationImport(from : MPath, to : MPath) extends PresentationElement {
   val path = to
   val parent = to
   val role = Role_Include
   val components = List(objects.OMMOD(from),objects.OMMOD(to))
   def toNode = <include from={from.toPath}/>
}

/**
 * Abstract representation of notations
 */
abstract class Notation extends PresentationElement {
   val nset : MPath
   val key : NotationKey
   val role = Role_Notation
   val components = Nil //TODO
   val path = nset
   val parent = nset
   def maintoString = "notation for " + key
   def toNode : Node
}

/** an interface for notations that can be used to present complex objects */ 
trait ComplexNotation extends Notation {
   def precedence: Precedence
   def presentation(args: Int, vars: Int, scopes: Int): Presentation
}

/**
 * A notation in a style, used for rendering
 * @param nset the containing style
 * @param key the notation key
 * @param presentation the presentation to be used
 * @param wrap a flag indicating whether this notation should be wrapped more with a more specific one if applicable
 */
// TODO abolish wrap in favor of fragment:constantwrapper
// TODO NotationKey should only be role; no merging in NotationStore
// TODO fragment:superscript, fragment:fraction, follow MathML
case class StyleNotation(nset : MPath, key : NotationKey, presentation : Presentation, wrap : Boolean) extends Notation {
   override def toString = maintoString + " " + presentation.toString
   def toNode = <notation wrap={if (wrap) "true" else null}>{presentation.toNode}</notation>
}

object StyleNotation {
   /**
    * parses a StyleNotation from XML
    */
   def parse(N : Node, nset : MPath, key : NotationKey) : Notation = {
       val wrap = xml.attr(N, "wrap") match {
          case "true" | "1" => true
          case "" | "false" | "0" => false
          case s => throw ParseError("illegal boolean value: " + s)
       }
       StyleNotation(nset, key, Presentation.parse(N.child), wrap)
   }
}

/*
sealed abstract class NotationProperties {
   val impl : Int
   val oPrec : Precedence
   def toNode : scala.xml.Elem
}
case class Infix(pos: Int, impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix={pos.toString} />
   override def toString = pos.toString + " " + impl.toString + " " + oPrec.toString
}
case class Prefix(appstyle : AppStyle, impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix="pre" />
   override def toString = "pre " + appstyle.toString + " " + impl.toString + " " + oPrec.toString
}
case class Postfix(appstyle : AppStyle, impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix="post" />
   override def toString = "post " + appstyle.toString + " " + impl.toString + " " + oPrec.toString
}
case class Interfix(assoc : Associativity, impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix="inter" />
   override def toString = "inter " + assoc.toString + " " + impl.toString + " " + oPrec.toString
}
case class Treefix(impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix="tree" />
   override def toString = "tree " + impl.toString + " " + oPrec.toString
}
case class Bindfix(impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix="bind" />
   override def toString = "bind " + impl.toString + " " + oPrec.toString
}

object NotationProperties {
   def apply(fix: Fixity, ass: Associativity, app: AppStyle, impl: Int, oPrec: Precedence) = fix match {
      case Pre => Prefix(app, impl, oPrec) 
      case Post => Postfix(app, impl, oPrec)
      case In(i) => Infix(i, impl, oPrec)
      case Inter => Interfix(ass, impl, oPrec)
      case Bind => Bindfix(impl, oPrec)
      case Tree => Treefix(impl, oPrec)
   }
}

/**
 * Representation of a declarative notation where the presentation is computed from parameters 
 * @param nset the containing style
 * @param key notation key
 * @param fix fixity
 * @param assoc associativity
 * @param appstyle application style
 * @param impl number of implicit arguments
 * @param oPrec the output precedence (used for bracket generation)
 */
case class PropertyNotation(nset : MPath, key : NotationKey, props : NotationProperties) extends ComplexNotation {
   def precedence = props.oPrec
   private implicit def int2CInxed(i: Int) = NumberedIndex(i)
   def presentation(args: Int, vars: Int, scopes: Int) = {
      val impl = props.impl
      val oPrec = Some(props.oPrec)
      val oper = Component(0, oPrec)
      val impargs = Iterate(1, impl, ArgSep(), oPrec)
      val args = Iterate(impl + 1, -1, ArgSep(), oPrec)
      val operimp = if (impl == 0) oper else Fragment("operimp", oper, impargs)
      props match {
         case Prefix(_,_,_) => Fragment("pre", operimp, args)
         case Postfix(_,_,_) => Fragment("post", operimp, args)
         case Infix(i,_,_) => Iterate(props.impl + 1, props.impl + i, ArgSep(), oPrec) + OpSep() + operimp + OpSep() +
                       Iterate(props.impl + i + 1, -1, ArgSep(), oPrec)
         case Interfix(assoc ,_,_) => assoc match {
            case AssocNone => Iterate(props.impl + 1, -1, OpSep() + operimp + OpSep(), oPrec.map(_.weaken))
            case Right => Nest(props.impl + 1, -1, Recurse(oPrec) + operimp + Hole(0,Presentation.Empty), Recurse(oPrec.map(_.weaken)))
            case Left => Nest(-1, props.impl + 1, Hole(0,Presentation.Empty) + operimp + Recurse(oPrec), Recurse(oPrec.map(_.weaken)))
         }
         case _: Bindfix => operimp + Iterate(props.impl + 1, -2, ArgSep(), oPrec) + OpSep() + Component(-1, oPrec)
         case _: Treefix => Fragment("tree", operimp, Iterate(props.impl + 1, -1, ArgSep() + ArgSep() + ArgSep(), Some(Precedence.neginfinite)))
      }
   }
   val wrap = false
   override def toString = key.path.getOrElse("[nopath]") + " " + key.role.toString + " " + props.toString
   def toNode = props.toNode % new UnprefixedAttribute("for", key.path.map(_.toPath).getOrElse(null), new UnprefixedAttribute("role", key.role.toString, scala.xml.Null))
}
/**
 * Helper object for notations
 */
object Notation {
   /**
    * parses a notation from XML
    */
   def parse(N : Node, nset : MPath, key : NotationKey) : Notation = {
       val fx = xml.attr(N,"fixity")
       val ap = xml.attr(N,"application-style")
       val as = xml.attr(N,"associativity")
       val im = xml.attr(N,"implicit")
       // simple notation if no attribute present
       if (fx == "" && ap == "" && as == "" && im == "") {
          val wrap = xml.attr(N, "wrap") match {
             case "true" | "1" => true
             case "" | "false" | "0" => false
             case s => throw ParseError("illegal boolean value: " + s)
          }
          SimpleNotation(nset, key, Presentation.parse(N.child), wrap)
       } else {
          val oPrec = xml.attr(N, "precedence") match {
             case "" => Precedence.integer(0)
             case op => Precedence.parse(op)
          }
          val fix = parseFix(fx)
          val app = parseAppSt(ap)
          val ass = parseAss(as)
          val imp = parseImp(im)
          PropertyNotation(nset, key, NotationProperties(fix, ass, app, imp, oPrec))
      }
   }
   /** parses the output of ComplexNotation.toString */
   def parseString(s: String, nset: MPath) : ComplexNotation = {
      val tokens = s.split("\\s+").iterator
      val path = Path.parse(tokens.next, nset)
      val role = Role.parse(tokens.next)
      val props = parseInlineNotation(tokens)
      PropertyNotation(nset, NotationKey(Some(path), role), props)
   }
   def parseInlineNotation(tokens: Iterator[String]) : NotationProperties = {
      val fix = parseFix(tokens.next)
      var ass: Associativity = null
      var app: AppStyle = null
      fix match {
        case Pre | Post => app = parseAppSt(tokens.next)
        case Inter => ass = parseAss(tokens.next)
        case _ =>
      }
      val imp = parseImp(tokens.next) 
      val prec = Precedence.parse(tokens.next)
      NotationProperties(fix, ass, app, imp, prec)
   }
   def parseFix(s: String) = s match { 
       case "" | "pre" => Pre
       case "post" => Post
       case "in" => In(1)
       case "inter" => Inter
       case "bind" => Bind
       case "tree" => Tree
       case s => try {In(s.toInt)}
                 catch {case _ : Throwable => throw ParseError("illegal fixity value: " + s)}
    }
   def parseAppSt(s: String) = s match {
       case "math" => Math
       case "lc" => LC
       case "" => LC
       case v => throw ParseError("illegal associativity value: " + v)
   }
   def parseAss(s: String) = s match {
       case "left" => Left
       case "right" => Right
       case "none" => AssocNone
       case "" => AssocNone
       case v => throw ParseError("illegal associativity value: " + v)
    }
   def parseImp(s: String) = s match {
       case "" => 0
       case s => try {s.toInt}
                 catch {case _ : Throwable => throw ParseError("illegal number of implicit arguments: " + s)}
    }
}

*/