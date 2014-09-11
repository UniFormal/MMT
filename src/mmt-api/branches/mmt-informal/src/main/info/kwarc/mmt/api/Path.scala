package info.kwarc.mmt.api
import libraries._
import modules._
import objects._
import utils._

import scala.collection.mutable.Map

/**
 * A Path represents an MMT path.
 * 
 * An MMT path refers to a document (doc), a module (doc?mod), or a symbol (M % sym).
 * Use the objects ?, %, /, \, and ! for pattern matching paths.
 */
sealed abstract class Path extends ontology.BaseType {
   /** the document part of the path */
   def doc : DPath
   /** goes one step up, identity if URI-path already empty */
   def ^! : Path
   /** the list of ancestors paths starting with this path */
   def ancestors : List[Path] = if (this.^! == this) List(this) else this :: (this ^!).ancestors
   /** checks whether this is a prefix of that */
   def <=(that : Path) : Boolean = this == that || (that != that.^! && this <= that.^!)
   /** string representation of a Path
    *  @return the path as an MMT URI
    *  @param long add trailing ?
    */
   def toPath(long : Boolean) : String = this match {
      case DPath(uri) => uri.toString + (if (long) "??" else "")
      case doc ? name => doc.toPath + "?" + name.toPath + (if (long) "?" else "")
      case mod % name => mod.toMPath.toPath + "?" + name.toPath
      case CPath(p, c) => p.toPathLong + "?" + c
   }
   /** as toPath(false) */
   def toPath : String = toPath(false)
   /** as toPath(true) */
   def toPathLong : String = toPath(true)
   /** as toPath, but escapes XML-illegal characters */
   def toPathEscaped = scala.xml.Utility.escape(toPath)
   /** the last components of the path, useful for short displays */
   def last : String
   /** breaks an MMT URI reference into its components, all of which are optional */
   def toTriple : (Option[DPath], Option[LocalName], Option[LocalName]) = this match {
      case mod % name =>
         val mp = mod.toMPath
         (Some(mp.parent), Some(mp.name), Some(name))
      case doc ? mod => (Some(doc), Some(mod), None)
      case doc : DPath => (Some(doc), None, None)
      case c: CPath => c.parent.toTriple
   }
   /** currently same as toPath, only toPath guarantees official string representation */
   override def toString = toPath
}

/** auxiliary trait to mixin convenience methods into [[Path]] classes */
trait SlashFunctions[A] {
   def /(n: LocalName): A
   def /(n: String): A = this / LocalName(n)
   def /(n: LNStep): A = this / LocalName(n)
}

/**
 * A DPath represents an MMT document level path.
 * @param uri the URI of the document (may not contain query or fragment)
 */
case class DPath(uri : URI) extends Path with SlashFunctions[DPath] {
   def doc = this
   def last = uri.path match {case Nil | List("") => uri.authority.getOrElse("") case l => l.last}
   def /(n : LocalName) = DPath(uri / n.steps.map(_.toPath))
   def ^! = DPath(uri ^)
   def ?(n : String) : MPath = this ? LocalName(n)
   def ?(n : LocalName) = MPath(this, n)
   def version : Option[String] = uri.path match {
       case Nil => None
       case l => l.last.indexOf(";") match {
          case -1 => None
          case i => Some(l.last.substring(i+1))
       }
   }
}

/**
 * A path to a module or symbol
 */
sealed trait ContentPath extends Path {
   /** checks if the path is a generic MMT path */
   def isGeneric : Boolean
   def $(comp: DeclarationComponent) = CPath(this, comp)
   def module : Term
   def name : LocalName
}

/**
 * An MPath represents an MMT module level path.
 * @param parent the path of the parent document
 * @param name the name of the module
 */
case class MPath(parent : DPath, name : LocalName) extends ContentPath with SlashFunctions[MPath] {
   def doc = parent
   def last = name.steps.last.toPath
   /** go up to containing document */
   def ^ : MPath = parent ? name.init
   def ^^ : DPath = parent
   def ^! = if (name.length <= 1) ^^ else ^
   /** go down to a submodule */
   def /(n : LocalName) = MPath(parent, name / n)
   /** go down to a symbol */
   def ?(n : LocalName) : GlobalName = OMID(this) % n
   def ?(n : String) : GlobalName = this ? LocalName(n)
   def isGeneric = (this == mmt.mmtcd)
   def module = OMMOD(this)
   def toGlobalName = ^ ? LocalName(name.last)
}

/** A GlobalName represents the MMT URI of a symbol-level declaration.
 * This includes virtual declarations and declarations within complex module expressions.
 */
case class GlobalName(module: Term, name: LocalName) extends ContentPath with SlashFunctions[GlobalName] {
   def doc = module.toMPath.doc
   def ^! = if (name.length == 1) module.toMPath else GlobalName(module, name.init)
   def /(n : LocalName) = GlobalName(module, name / n)
   def last = name.last.toPath
   def apply(args: List[Term]) : Term = OMA(OMS(this), args)
   def apply(args: Term*) : Term = apply(args.toList)
   def apply(con: Context, args: List[Term]) : Term = OMBINDC(OMS(this), con, args)
   def apply(subs: Substitution, con: Context, args: List[Term]) : Term = ComplexTerm(this, subs, con, args)
   /** true iff the parent is a named module and each include step is simple */
   def isSimple : Boolean = module.isInstanceOf[OMID] && name.steps.forall(_.isInstanceOf[SimpleStep])
   def isGeneric = (module.toMPath == mmt.mmtcd)
}

/**
 * A LocalName represents a local MMT symbol-level declarations (relative to a module).
 * @param steps the list of (in MMT: slash-separated) components
 */
case class LocalName(steps: List[LNStep]) extends SlashFunctions[LocalName] {
   def /(n: LocalName) : LocalName = LocalName(steps ::: n.steps)
   def init = LocalName(steps.init)
   def tail = LocalName(steps.tail)
   def head = steps.head
   def last = steps.last
   def length = steps.length
   /**
    * @return if this == p / l, then Some(p), else None
    */
   def hasPrefix(l: LocalName): Option[LocalName] =
      if (steps.startsWith(l.steps)) Some(LocalName(steps.drop(l.length)))
      else None
   /** removes repeated complex steps, keeping the later one */
   def simplify: LocalName = {
      var complexBefore = false
      val stepsRS = steps.reverse filter {
         case s: SimpleStep => true
         case c: ComplexStep =>
            val res = ! complexBefore 
            complexBefore = true
            res
      }
      LocalName(stepsRS.reverse)
   }
   /** returns the list of all names contained in this one, starting with the shortest */
   def prefixes : List[LocalName] = if (length <= 1) List(this) else init.prefixes ::: List(this)
   /** machine-oriented string representation of this name, parsable and official */
   def toPath : String = steps.map(_.toPath).mkString("", "/", "")
  /** human-oriented string representation of this name, no encoding, possibly shortened */
   override def toString : String = steps.map(_.toString).mkString("", "/", "")
}

object LocalName {
   def apply(step: LNStep) : LocalName = LocalName(List(step))
   def apply(step: String) : LocalName = LocalName(SimpleStep(step))
   def apply(p: MPath) : LocalName = LocalName(ComplexStep(p))
   /** parses a LocalName, complex segments are parsed relative to base */
   def parse(s: String, base: Path): LocalName = LocalRef.parse(s).toLocalName(base)
   def parse(s:String): LocalName = parse(s, utils.mmt.mmtbase)
}

/** a step in a LocalName */
abstract class LNStep {
   def toPath : String
   def unary_! = LocalName(this)
   def /(n: LocalName) = LocalName(this) / n
   def /(n: LNStep) = LocalName(this) / n
}
/** constant or structure declaration */
case class SimpleStep(name: String) extends LNStep {
   def toPath = xml.encodeURI(name)
   override def toString = name
}
/** an include declaration; ComplexStep(fromPath) acts as the name of an unnamed structure */
case class ComplexStep(path: MPath) extends LNStep {
   def toPath = "[" + path.toPath + "]"
   override def toString = toPath
}

case class CPath(parent: ContentPath, component: DeclarationComponent) extends Path {
   def doc = parent.doc
   def ^! = parent
   def last = component.toString
}

/*
object LNEmpty {
   def apply() = LocalName(Nil)
   def unapply(n: LocalName) : Option[Unit] = n match {
      case LocalName(Nil) => Some(())
      case _ => None
   }
}
*/

/**
 * A LocalRef represents a possibly relative LocalName
 * @param segments the list of (slash-separated) components
 * @param absolute a flag whether the reference is absolute (i.e., starts with a slash)
 */
case class LocalRef(segments : List[String], absolute : Boolean) {
   def toLocalName(base: Path) = {
      val steps = segments map {s =>
         if (s.startsWith("["))
            ComplexStep(Path.parseM(s.substring(1,s.length - 1), base))
         else
            SimpleStep(s)
      }
      LocalName(steps)
   }
   override def toString = segments.mkString(if (absolute) "/" else "","/","")
}

object LocalRef {
   /** splits /-separated sequence of (String | "[" String "]") into its components
    * []-wrappers are preserved
    * []-wrapped components may contain /
    * components may be empty, initial or final / causes empty component
    * empty string is parsed as Nil
    */
   private def splitName(s: String): List[String] = {
      var left : String = s            //string that is left to parse
      var seen : List[String] = Nil    //segments that have been parsed
      var current : String = ""        //the part of the current segment that has been parsed
      //called when the end of the current segment has been detected
      def segmentDone {seen ::= current; current = ""}
      //called when the next character is appended to the current segment
      def charDone {current = current + left(0); left = left.substring(1)}
      //parses s segment-wise; if a segment starts with [, pass control to complex
      def start {   if (left == "")            {if (current != "" || ! seen.isEmpty) segmentDone}
               else if (left.startsWith("[") && current == "")
                                               {complex}
               else if (left.startsWith("/"))  {segmentDone; left = left.substring(1); start}
               else                            {charDone; start}
      } //TODO accept only balanced nestings of []?
      //parses a complex segment of the form [URI] (assumes [ has been parsed already)
      def complex {if (left.isEmpty)           {throw ParseError("unclosed '[' in " + s)}
               else if (left == "]")           {charDone; segmentDone}
               else if (left.startsWith("]/")) {charDone; segmentDone; left = left.substring(1); start}
               else                            {charDone; complex}
      }
      start
      return seen.reverse
   }
   def parse(n : String) : LocalRef = {
      val relative = n.startsWith("/")
      var l = splitName(n)
      if (relative)
         l = l.drop(1)
      l = try {l map xml.decodeURI} catch {case xml.XMLError(s) => throw ParseError(s)}
      LocalRef(l, ! relative)
   }
}

/** helper object for paths */
object Path {
   /**
    * [[Path]]s consume a lot of memory because there are so many
    * because they are stateless, we can secretly introduce structure sharing
    */ 
   private val pathCache = new ValueCache[Path](50)
   private val uriParseCache = new ResultCache[String,URI](URI(_), 5)

   /** the empty path */
   lazy val empty : Path = Path.parse("")

   def parse(s : String) : Path = parse(s, utils.mmt.mmtbase)
   /** parses an MMT-URI reference into a triple and then makes it absolute */
   def parse(s : String, base : Path) : Path = {
      val (d,m,n,c) = split(s)
      parse(d,m,n,c,base)
   }
   // merge(x,y) merges the URI or LocalPath x with the relative or absolute reference y
   private def mergeD(bd : Option[DPath], d : DPath) : DPath = if (bd.isEmpty) d else DPath(bd.get.uri.resolve(d.uri)) 
   private def mergeN(base: Path, bl : Option[LocalName], l : LocalRef) : LocalName =
      if (bl.isEmpty || l.absolute)
         l.toLocalName(base)
      else
         bl.get / l.toLocalName(base)
   /** turns an MMT-URI reference (d,m,n) into an MMT-URI relative to base (omitting a component is possible by making it empty) */
   def parse(dS : String, m : String, n : String, comp: String, base : Path) : Path = {
      val d = uriParseCache(dS)
      //to make the case distinctions simpler, all omitted (= empty) components become None
      val doc = if (d.scheme == None && d.authority == None && d.path == Nil) None else Some(DPath(d))
      def wrap(l : LocalRef) = if (l.segments.isEmpty) None else Some(l)
      val mod = wrap(LocalRef.parse(m))
      val name = wrap(LocalRef.parse(n))
      //get the base as a triple of three Options (first component will never be None)
      val (bdoc, bmod, bname) = base.toTriple
      //now explicit case distinctions to be sure that all cases are covered
      val path = (bdoc, bmod, bname, doc, mod, name) match {
         case (Some(bd), Some(bm), _, None,    None,    Some(n)) => bd ? bm ? mergeN(base, bname, n)
         case (Some(bd), _       , _, None,    Some(m), None   ) => bd ? mergeN(base, bmod, m)
         case (Some(bd), _       , _, None,    Some(m), Some(n)) => bd ? mergeN(base, bmod, m) ? n.toLocalName(base)
         case (_       , _       , _, None,    None,    None   ) => base
         case (_       , _       , _, Some(d), None,    None   ) => mergeD(bdoc, d)
         case (_       , _       , _, Some(d), Some(m), None   ) => mergeD(bdoc, d) ? m.toLocalName(base)
         case (_       , _       , _, Some(d), Some(m), Some(n)) => mergeD(bdoc, d) ? m.toLocalName(base) ? n.toLocalName(base)
         case _ => throw ParseError("(" + doc + ", " + mod + ", " + name + ") cannot be resolved against " + base) 
      }
      val pathR = pathCache.get(path)
      if (comp == "") pathR else pathR match {
         case cp: ContentPath =>
            val compP = TermComponent.parse(comp)
            CPath(cp, compP)
         case p => throw ParseError("cannot take component " + comp + " of path " + p)
      }
   }
   /** splits uri?mod?name?component into (uri, mod, name, component) */
   private def split(s : String) : (String, String, String, String) = {
      var left = s
      val comp = Array("", "", "", "") // Array(uri, mod, name, component)
      var current = 0
      while (left != "") {
         left(0) match {
            case '?' =>
               if (current == 3)
                  throw ParseError("MMT-URI may have at most three ?s: " + s)
               else
                  current += 1
            case '#' => throw ParseError("MMT-URI may not have fragment: " + s)
            case '[' if current == 2 =>
               val pos = left.indexOf("]")
               if (pos == -1)
                  comp(current) += '[' //unclosed [ not treated specially
               else {
                  comp(current) += left.substring(0,pos+1)
                  left = left.substring(pos) //one more character chopped below
               }
            case c =>
               comp(current) += c
         }
         left = left.substring(1)
      }
      (comp(0), comp(1), comp(2), comp(3))
   }
   /** as parse but fails if the result is not a component level URI */
   def parseC(s : String, base : Path) : CPath = parse(s,base) match {
      case p : CPath => p
      case p => throw ParseError("component path expected: " + p) 
   }
   /** as parse but fails if the result is not a symbol level URI */
   def parseS(s : String, base : Path) : GlobalName = parse(s,base) match {
      case p : GlobalName => p
      case p => throw ParseError("symbol path expected: " + p) 
   }
   /** as parse but fails if the result is not a module level URI */
   def parseM(s : String, base : Path) : MPath = parse(s,base) match {
      case p : MPath => p
      case p => throw ParseError("module path expected: " + p) 
   }
   /** as parse but fails if the result is not a document level URI */
   def parseD(s : String, base : Path) : DPath = parse(s,base) match {
      case p : DPath => p
      case p => throw ParseError("document path expected: " + p) 
   }
}

/** 
 * This permits the syntax doc ? mod in patterns. 
 */
object ? {
   def unapply(p : Path) : Option[(DPath,LocalName)] = p match {
      case MPath(doc, name) => Some((doc, name))
      case _ => None
   }
}

/** 
 * This permits the syntax mod ?? name in patterns. 
 */
object ?? {
   def unapply(p : Path) : Option[(MPath,LocalName)] = p match {
      case GlobalName(OMMOD(p), n) => Some((p,n))
      case _ => None
   }
}

/** 
 * This permits the syntax mod % sym in patterns.
 */
object % {
   def unapply(p : Path) : Option[(Term,LocalName)] = p match {
      case GlobalName(term,n) => Some((term,n))
      case _ => None
   }
}

/** 
 * This permits the syntax head / tail in patterns. 
 */
object / {
   def unapply(l : LocalName) : Option[(LNStep,LocalName)] = if (l.length <= 1) None else Some((l.head,l.tail))
}


/** 
 * This permits the syntax init \ last in patterns. 
 */
object \ {
   def unapply(l : LocalName) : Option[(LocalName,LNStep)] = if (l.length <= 1) None else Some((l.init,l.last))
}

/** 
 * This permits the syntax !(n) in patterns to match atomic local names. 
 */
object ! {
   def unapply(l : LocalName) : Option[LNStep] = if (l.length == 1) Some(l.head) else None
}