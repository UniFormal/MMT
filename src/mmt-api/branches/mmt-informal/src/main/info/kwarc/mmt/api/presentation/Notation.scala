package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.MyList.{fromList}
import info.kwarc.mmt.api.utils._
import scala.xml.{Node,NodeSeq}

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
   val pres : Presentation
   val oPrec : Option[Precedence]
   val wrap : Boolean
   val role = Role_Notation
   val components = Nil //TODO
   val path = nset
   val parent = nset
   def maintoString = "notation for " + key
   def toNode = <notation/>
}

/**
 * Main representation of a notation
 * @param nset the containing style
 * @param key the notation key
 * @param pres the presentation to be used
 * @param oPrec the output precedence (used for bracket generation)
 * @param wrap a flag indicating whether this notation should be wrapped more with a more specific one if applicable
 */
case class SimpleNotation(nset : MPath, key : NotationKey, pres : Presentation, oPrec : Option[Precedence], wrap : Boolean) extends Notation {
   override def toString = maintoString + " " + pres.toString
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
case class ComplexNotation(nset : MPath, key : NotationKey, 
                           fix : Fixity, assoc : Associativity, appstyle : AppStyle, impl : Int,
                           oPrec: Option[Precedence]) extends Notation {
   private implicit def int2CInxed(i: Int) = NumberedIndex(i)
   lazy val pres : Presentation = {
      val oper = Component(0, oPrec)
      val impargs = Iterate(1, impl, ArgSep(), oPrec)
      val args = Iterate(impl + 1, -1, ArgSep(), oPrec)
      val operimp = if (impl == 0) oper else Fragment("operimp", oper, impargs)
      fix match {
         case Pre => Fragment("pre", operimp, args)
         case Post => Fragment("post", operimp, args)
         case In(i) => Iterate(impl + 1, impl + i, ArgSep(), oPrec) + OpSep() + operimp + OpSep() + 
                       Iterate(impl + i + 1, -1, ArgSep(), oPrec)
         case Inter => assoc match {
            case AssocNone => Iterate(impl + 1, -1, OpSep() + operimp + OpSep(), oPrec.map(_.weaken))
            case Right => Nest(impl + 1, -1, Recurse(oPrec) + operimp + Hole(0,Presentation.Empty), Recurse(oPrec.map(_.weaken)))
            case Left => Nest(-1, impl + 1, Hole(0,Presentation.Empty) + operimp + Recurse(oPrec), Recurse(oPrec.map(_.weaken)))
         }
         case Bind => operimp + Iterate(impl + 1, -2, ArgSep(), oPrec) + OpSep() + Component(-1, oPrec)
         case Tree => Fragment("tree", operimp, Iterate(impl + 1, -1, ArgSep() + ArgSep() + ArgSep(), Some(Precedence.neginfinite)))
      }
   }
   val wrap = false
   override def toString = key.path.getOrElse("[nopath]") + " " + key.role.toString + " " +
                           fix.toString + " " + assoc.toString + " " + appstyle.toString + " " +
                           impl.toString + " " + oPrec.map(_.toString).getOrElse("none") 
}
/**
 * Helper object for notations
 */
object Notation {
   /**
    * parses a notation from XML
    */
   def parse(N : Node, nset : MPath, key : NotationKey) : Notation = {
       val oPrec = (key.role.bracketable, xml.attr(N, "precedence")) match {
          case (false, "") => None
          case (false, _) => throw ParseError("precedence given for non-bracketable role: " + N)
          case (true, "") => Some(Precedence.integer(0))
          case (true, op) => Some(Precedence.parse(op))
       }
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
          SimpleNotation(nset, key, Presentation.parse(N.child), oPrec, wrap)
       } else {
          val fix = parseFix(fx)
          val app = parseAppSt(ap)
          val ass = parseAss(as)
          val imp = parseImp(im)
          ComplexNotation(nset, key, fix, ass, app, imp, oPrec)
      }
   }
   /** parses the output of ComplexNotation.toString */
   def parseString(s: String, nset: MPath) : ComplexNotation = {
      val tokens = s.split(" ").iterator
      val path = Path.parse(tokens.next, nset)
      val role = Role.parse(tokens.next)
      val fix = parseFix(tokens.next)
      val ass = parseAss(tokens.next)
      val app = parseAppSt(tokens.next)
      val imp = parseImp(tokens.next) 
      val prec = Precedence.parseOpt(tokens.next)
      ComplexNotation(nset, NotationKey(Some(path), role), fix, ass, app, imp, prec)
   }
   def parseFix(s: String) = s match { 
       case "" | "pre" => Pre
       case "post" => Post
       case "in" => In(1)
       case "inter" => Inter
       case "bind" => Bind
       case "tree" => Tree
       case s => try {In(s.toInt)}
                 catch {case _ => throw ParseError("illegal fixity value: " + s)}
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
                 catch {case _ => throw ParseError("illegal number of implicit arguments: " + s)}
    }
}