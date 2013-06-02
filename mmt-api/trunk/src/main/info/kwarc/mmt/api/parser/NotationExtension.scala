package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import presentation.Presentation

/** A FixityParser handles advanced notations, e.g., infix, prefix in order to avoid giving the cumbersome mixfix notations
 *  
 *  FixityParsers can be added flexibly to extend MMT with additional notation forms
 *  
 *  Every notation form is given by a fixityString and list of string arguments
 */
abstract class FixityParser {
   /** true if this parser is applicable to a given fixityString */
   def isApplicable(fixityString: String): Boolean
   /** parses a notation for name with a given list of arguments 
    *  @return the fixity that will occur in the TextNotation
    */
   def parse(name: GlobalName, fixityString: String, args: List[String]): Fixity
}

/** maintains the available fixity parsers
 *  
 *  currently hardcoded, should be moved to ExtensionManager
 */
object FixityParser {
   /** the available parsers */
   val parsers = List(MixfixParser, BasicFixityParser, RuleFixityParser)
   /** parses using an applicable FixityParser */
   def parse(name: GlobalName, fixityString: String, arguments: List[String]) = {
      val parser = parsers.find(_.isApplicable(fixityString)).getOrElse(throw ParseError("unknown fixity: " + fixityString))
      parser.parse(name, fixityString, arguments)
   }
}

/** A Fixity is a high-level description of a list of markers that can be used for parsing or presentation
 *  
 *  It is returned by a FixityParser and used in a TextNotation
 */
abstract class Fixity(val fixityString: String) {
   /** @return the arguments for serialization */
   def argumentStrings: List[String]
   def argumentString: String = argumentStrings.mkString(" ")
   override def toString = "%%" + fixityString + " " + argumentString
   /** @return a complete list of Marker's */
   def allMarkers : List[Marker]
   /** @return the list of only those markers that are relevant for parsing */
   def markers = allMarkers.filter {
      case _:ImplicitArg => false // remove the implicit markers
      case _ => true
   }
}

/** the standard FixityParser for a list of Marker's */
object MixfixParser extends FixityParser {
   /** applicable to "mixfix" */
   def isApplicable(s: String) = s == "mixfix"
   def parse(name: GlobalName, fix: String, args: List[String]) = {
      val markers = args.map(Marker.parse(name, _))
      Mixfix(markers)
   }
}

/** the simplest Fixity consisting of a list of Marker's */
case class Mixfix(val allMarkers: List[Marker]) extends Fixity("mixfix") {
   def argumentStrings = allMarkers.map(_.toString)
}

/**
 * A FixityParser that provides infix, prefix, etc., each with some implicit arguments
 *  
 * see BasicFixity
 */
object BasicFixityParser extends FixityParser {
   /** infix, infix-right, infix-left, prefix, postfix */
   def isApplicable(s: String) = List("infix", "infix-right", "infix-left", "prefix", "postfix") contains s
   def parse(name: GlobalName, f: String, args: List[String]) = {
      val impl = args match {
         case hd :: Nil => 
            try {hd.toInt}
            catch {
               case e: Throwable => throw ParseError("number expected")
            }
            
         case _ => throw ParseError("one argument expected")
      }
      BasicFixity(name, f, impl)
   }
}

/**
 * @param fixity can be infix, infix-right, infix-left, prefix, postfix
 * @param impl the number of implicit arguments; all operators skip the first impl arguments
 */ 
case class BasicFixity(name: GlobalName, fixity: String, impl: Int) extends Fixity(fixity) {
   def argumentStrings = List(impl.toString)
   /** infix: a1 f a2
    *  prefix: f a1 ... an
    *  postfix: a1 ... an f
    *  
    *  infix-left and infix-right are currently the same as infix, will be used for left/right-associative operators later
    *  but generally its better to avoid them in favor of sequence arguments
    */
   def allMarkers = fixity match {
      case "infix"       => List(Arg(impl+1),SymbolName(name),Arg(impl+2))
      case "infix-left"  => List(Arg(impl+1),SymbolName(name),Arg(impl+2))
      case "infix-right" => List(Arg(impl+1),SymbolName(name),Arg(impl+2))
      case "prefix"      => List(SymbolName(name), SeqArg(impl+1, Delim("%w")))
      case "postfix"     => List(SeqArg(impl+1, Delim("%w")), SymbolName(name))
   }
}

object RuleFixityParser extends FixityParser {
   def isApplicable(s: String) = s == "rule"
   def parse(name: GlobalName, fix: String, args: List[String]) = {
      val ints = try {
         args.map(_.toInt)
      } catch {case e: Throwable => throw ParseError("illegal arguments: " + args)}
      val (i, e, h) = ints.length match {
         case 0 => (0,0,0)
         case 1 => (ints(0), 0, 0)
         case 2 => (ints(0),0,ints(1))
         case 3 => (ints(0),ints(1),ints(2))
         case _ => throw ParseError("too many arguments: " + args)
      }
      RuleFixity(name, i, e, h)
   }
}

/** A Fixity for inference rules: it takes some implicit and explicit parameters, then some hypotheses */
case class RuleFixity(name: GlobalName, implParams: Int, explParams: Int, hypos: Int) extends Fixity("rule") {
   def argumentStrings = List(implParams.toString, explParams.toString, hypos.toString)
   def allMarkers =
      SymbolName(name) ::
      Range(0,implParams).map {i => ImplicitArg(i+1)}.toList :::
      Range(0,explParams).map {i => Arg(implParams + i +1 )}.toList :::
      Range(0,hypos).map {i => Arg(implParams + explParams + i + 1)}.toList
} 

/*
case class Treefix(impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix="tree" />
   override def toString = "tree " + impl.toString + " " + oPrec.toString
}
case class Bindfix(impl : Int, oPrec: Precedence) extends NotationProperties {
   def toNode = <notation fix="bind" />
   override def toString = "bind " + impl.toString + " " + oPrec.toString
}


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
*/