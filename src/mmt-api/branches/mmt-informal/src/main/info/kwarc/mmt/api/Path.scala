package info.kwarc.mmt.api
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.presentation._
import scala.collection.mutable.Map


/**
 * A Path represents an MMT path. <p>
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
   def toTriple : (Option[DPath], Option[LocalPath], Option[LocalName]) = this match {
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

/**
 * A DPath represents an MMT document level path.
 * @param uri the URI of the document (may not contain query or fragment)
 */
case class DPath(uri : URI) extends Path {
   //go down to a module
   def doc = this
   def last = uri.path match {case Nil | List("") => uri.authority.getOrElse("") case l => l.last}
   def /(n : String) = DPath(uri / n)
   def /(n : LocalPath) = DPath(n.fragments.foldLeft(uri)( _ / _))
   def ^! = DPath(uri ^)
   def ?(n : String) : MPath = this ? new LocalPath(n)
   def ?(n : LocalPath) = MPath(this, n)
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
}

/**
 * An MPath represents an MMT module level path.
 * @param parent the path of the parent document
 * @param name the name of the module
 */
case class MPath(parent : DPath, name : LocalPath) extends ContentPath {
   def doc = parent
   def last = name.fragments.last
   /** go up to containing document */
   def ^ : MPath = parent ? name.init
   def ^^ : DPath = parent
   def ^! = if (name.isNative) ^^ else ^
   def /(n : String) : MPath = MPath(parent, name / n)
   /** go down to a submodule */
   def /(n : LocalPath) = MPath(parent, name / n)
   /** go down to a symbol */
   def ?(n : LocalName) : GlobalName = OMID(this) % n
   def ?(n : String) : GlobalName = this ? LocalName(n)
   def components : List[Content] = List(StringLiteral(doc.uri.toString), StringLiteral(name.flat),Omitted, StringLiteral(toPathEscaped))
   def isGeneric = (this == mmt.mmtcd)
}

/** A GlobalName represents the MMT URI of a symbol-level declaration.
 * This includes virtual declarations and declarations within complex module expressions.
 */
case class GlobalName(module: Term, name: LocalName) extends ContentPath {
   def doc = utils.mmt.mmtbase
   def ^! = if (name.length == 1) module.toMPath else GlobalName(module, name.init)
   def last = if (name.isAnonymous) "_" else name.last.toPath
   def apply(args: List[Term]) : Term = OMA(OMID(this), args)
   def apply(args: Term*) : Term = apply(args.toList)
   /** true iff the parent is a named module and each include step is simple */
   def isSimple : Boolean = module.isInstanceOf[OMID] && name.steps.forall(_.isInstanceOf[SimpleStep])
   def isGeneric = (module.toMPath == mmt.mmtcd)
}

/**
 * A LocalPath represents a local MMT module (relative to a document).
 * @param fragments the list of (in MMT: slash-separated) components
 */
case class LocalPath(fragments : List[String]) {
   def this(n : String) = {this(List(n))}
   def /(n: String) = LocalPath(fragments ::: List(n)) 
   def /(that: LocalPath) = LocalPath(fragments ::: that.fragments)
   def isNative = fragments.length == 1
   def init = LocalPath(fragments.init)
   def tail = LocalPath(fragments.tail)
   def head = fragments.head
   def last = fragments.last
   def length = fragments.length
   def prefixes : List[LocalPath] = if (length <= 1) List(this) else this :: tail.prefixes
   implicit def toList : List[String] = fragments
   def flat : String = fragments.mkString("", "/","")
   def toPath : String = fragments.map(xml.encodeURI).mkString("", "/", "")
   override def toString = flat
}
object LocalPath {
  lazy val Anon = LocalPath(List())
}

/**
 * A LocalName represents a local MMT symbol-level declarations (relative to a module).
 * @param steps the list of (in MMT: slash-separated) components
 */
case class LocalName(steps: List[LNStep]) {
   def /(n: LocalName) : LocalName = LocalName(steps ::: n.steps)
   def /(n: LNStep) : LocalName = this / LocalName(List(n))
   def /(n: String) : LocalName = this / LocalName(n)
   def init = LocalName(steps.init)
   def tail = LocalName(steps.tail)
   def head = steps.head
   def last = steps.last
   def length = steps.length
   /** machine-oriented string representation of this name, parsable and official */
   def toPath : String = steps.map(s => xml.encodeURI(s.toString)).mkString("", "/", "")
   /** human-oriented string representation of this name, no encoding, possibly shortened */
   override def toString : String = steps.map(_.toPath).mkString("", "/", "")
   def isAnonymous = this == LocalName.Anon
}
object LocalName {
   def apply(step: LNStep) : LocalName = LocalName(List(step))
   def apply(step: String) : LocalName = LocalName(SimpleStep(step))
   /** parses a LocalName that has no []-wrapped segments */
   def parse(s:String) = {
      val ref = Path.parseLocal(s)
      LocalName(ref.segments.map(SimpleStep(_)))
   }
   /** parses a LocalName, complex segments are parsed relative to base */
   def parse(base: Path, s: String) = Path.parseLocal(s).toLocalName(base)
   val Anon = LocalName(List())
}
/** a step in a LocalName */
abstract class LNStep {
   def toPath : String
   override def toString = toPath
   def unary_! = LocalName(this)
   def /(n: LocalName) = LocalName(this) / n
   def /(n: LNStep) = LocalName(this) / n
}
/** constant or structure declaration */
case class SimpleStep(name: String) extends LNStep {
   def toPath = name
}
/** an include declaration; these are unnamed and identified by the imported theory */
case class ComplexStep(path: MPath) extends LNStep {
   def toPath = "[" + path.toPath + "]"
   /** true iff the IncludeStep is a named module */
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
 * A LocalRef represents a possibly relative LocalName or LocalPath
 * @param segments the list of (in MMT: slash-separated) components
 * @param absolute a flag whether the reference is absolute (in MMT: starts with a slash)
 */
case class LocalRef(segments : List[String], absolute : Boolean) {
   def toLocalName(base: Path) = {
      val steps = segments map {s => if (s.startsWith("[")) ComplexStep(Path.parseM(s.substring(1,s.length - 1), base))
                                     else SimpleStep(s)
                               }
      LocalName(steps)
   }
   def toLocalPath = LocalPath(segments)
   override def toString = segments.mkString(if (absolute) "/" else "","/","")
}

/** helper object for paths */
object Path {
   /** the empty path */
   val empty : Path = Path.parse("")

   def parse(s : String) : Path = parse(s, utils.mmt.mmtbase)
   /** parses an MMT-URI reference into a triple and then makes it absolute */
   def parse(s : String, base : Path) : Path = {
      val (d,m,n,c) = split(s)
      parse(d,m,n,c,base)
   }
   // merge(x,y) merges the URI or LocalPath x with the relative or absolute reference y
   private def mergeD(bd : Option[DPath], d : DPath) : DPath = if (bd.isEmpty) d else DPath(bd.get.uri.resolve(d.uri)) 
   private def mergeM(bl : Option[LocalPath], l : LocalRef) : LocalPath =
      if (bl.isEmpty || l.absolute)
         l.toLocalPath
      else
         bl.get / l.toLocalPath
   private def mergeS(base: Path, bl : Option[LocalName], l : LocalRef) : LocalName =
      if (bl.isEmpty || l.absolute)
         l.toLocalName(base)
      else
         bl.get / l.toLocalName(base)
   /** turns an MMT-URI reference (d,m,n) into an MMT-URI relative to base (omitting a component is possible by making it empty) */
   def parse(d : URI, m : String, n : String, comp: String, base : Path) : Path = {
      //to make the case distinctions simpler, all omitted (= empty) components become None
      val doc = if (d.scheme == None && d.authority == None && d.path == Nil) None else Some(DPath(d))
      def wrap(l : LocalRef) = if (l.segments.isEmpty) None else Some(l)
      val mod = wrap(parseLocal(m))
      val name = wrap(parseLocal(n))
      //get the base as a triple of three Options (first component will never be None)
      val (bdoc, bmod, bname) = base.toTriple
      //now explicit case distinctions to be sure that all cases are covered
      val path = (bdoc, bmod, bname, doc, mod, name) match {
         case (Some(bd), Some(bm), _, None,    None,    Some(n)) => bd ? bm ? mergeS(base, bname, n)
         case (Some(bd), _       , _, None,    Some(m), None   ) => bd ? mergeM(bmod, m)
         case (Some(bd), _       , _, None,    Some(m), Some(n)) => bd ? mergeM(bmod, m) ? n.toLocalName(base)
         case (_       , _       , _, None,    None,    None   ) => base
         case (_       , _       , _, Some(d), None,    None   ) => mergeD(bdoc, d)
         case (_       , _       , _, Some(d), Some(m), None   ) => mergeD(bdoc, d) ? m.toLocalPath
         case (_       , _       , _, Some(d), Some(m), Some(n)) => mergeD(bdoc, d) ? m.toLocalPath ? n.toLocalName(base)
         case _ => throw ParseError("(" + doc + ", " + mod + ", " + name + ") cannot be resolved against " + base) 
      }
      if (comp == "") path else path match {
         case cp: ContentPath =>
            val compP = TermComponent.parse(comp)
            CPath(cp, compP)
         case p => throw ParseError("cannot take component " + comp + " of path " + p)
      }
   }
   /** splits uri?mod?name?component into (uri, mod, name, component) */
   private def split(s : String) : (URI, String, String, String) = {
      if (s.indexOf("#") != -1)
         throw new ParseError("MMT-URI may not have fragment: " + s)
      val comps = s.split("\\?",-1)
      val doc = URI(comps(0))  //note: split returns at least List(""), never Nil
      comps.length match {
         case 1 => (doc, "", "", "")
         case 2 => (doc, comps(1), "", "")
         case 3 => (doc, comps(1), comps(2), "")
         case 4 => (doc, comps(1), comps(2), comps(3))
         case _ => throw ParseError("MMT-URI may have at most three ?s: " + s)
      }
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
   
   /** splits /-separated sequence of (String | "[" String "]") into its components
    * []-wrappers are preserved
    * []-wrapped components may contain /
    * components may be empty
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
      def start {   if (left == "")            {if (current != "") segmentDone}
               else if (left.startsWith("/[")) {segmentDone; left = left.substring(1); complex}
               else if (left.startsWith("/"))  {segmentDone; left = left.substring(1); start}
               else                            {charDone; start}
      } //TODO accept only balanced pairs of []?
      //parses a complex segment of the form [URI] (assumes [ has been parsed already)
      def complex {if (left.isEmpty)           {throw ParseError("unclosed '[' in " + s)}
               else if (left == "]")           {segmentDone}
               else if (left.startsWith("]/")) {charDone; segmentDone; left = left.substring(1); start}
               else                            {charDone; complex}
      }
      start
      return seen.reverse
   }
   
   /** parses a possibly relative LocalPath or LocalName */
   def parseLocal(n : String) : LocalRef = {
      val relative = n.startsWith("/")
      var l = splitName(n)
      if (relative)
         l = l.drop(1)
      if (l.exists(_ == ""))
         throw ParseError("cannot parse " + n + " (local path may not have empty component or end in slash)")
      l = l map xml.decodeURI
      LocalRef(l, ! relative)
   }
   /** as parseLocal but fails on relative results */
   def parseName(n : String) : LocalRef = {
      val r = parseLocal(n)
      if (! r.absolute)
         throw ParseError("cannot parse " + n + " (local path may not start with slash)")
      r
   }
}

/** 
 * This permits the syntax doc ? mod in patterns. 
 */
object ? {
   def unapply(p : Path) : Option[(DPath,LocalPath)] = p match {
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
   def unapply(l : LocalPath) : Option[(String,LocalPath)] = if (l.isNative) None else Some((l.head,l.tail))
   def unapply(l : LocalName) : Option[(LNStep,LocalName)] = if (l.length <= 1) None else Some((l.head,l.tail))
}


/** 
 * This permits the syntax init \ last in patterns. 
 */
object \ {
   def unapply(l : LocalPath) : Option[(LocalPath,String)] = if (l.isNative) None else Some((l.init,l.last))
   def unapply(l : LocalName) : Option[(LocalName,LNStep)] = if (l.length <= 1) None else Some((l.init,l.last))
}

/** 
 * This permits the syntax !(n) in patterns to match atomic local names. 
 */
object ! {
   def unapply(l : LocalPath) : Option[String] = if (l.isNative) Some(l.head) else None
   def unapply(l : LocalName) : Option[LNStep] = if (l.length == 1) Some(l.head) else None
}
