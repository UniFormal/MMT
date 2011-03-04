package jomdoc
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.utils._
import jomdoc.presentation._
import scala.collection.mutable.Map

object Path {
   //parses an MMT-URI reference into a triple and then makes it absolute
   def parse(s : String, base : Path) : Path = {
      val (d,m,n) = toTriple(s)
      parse(d,m,n,base)
   }
   // merge(x,y) merges the URI or LocalPath x with the relative or absolute reference y
   private def merge(bd : Option[DPath], d : DPath) = if (bd.isEmpty) d else DPath(bd.get.uri.resolve(d.uri)) 
   private def merge(bl : Option[LocalPath], l : LocalRef) =
      if (bl.isEmpty || l.absolute)
         LocalPath(l.fragments)
      else
         bl.get / LocalPath(l.fragments)
   /** turns an MMT-URI reference (d,m,n) into an MMT-URI relative to base (omitting a component is possible by making it empty) */
   def parse(d : xml.URI, m : String, n : String, base : Path) : Path = {
      //to make the case distinctions simpler, all omitted (= empty) components become None
      val doc = if (d.getScheme == null && d.getAuthority == null && d.getPath == "") None else Some(DPath(d))
      def wrap(l : LocalRef) = if (l.fragments.isEmpty) None else Some(l)
      val mod = wrap(parseLocal(m))
      val name = wrap(parseLocal(n))
      //get the base as a triple of three Options (first component will never be None)
      val (bdoc, bmod, bname) = base.toTriple
      //now explicit case distinctions to be sure that all cases are covered
      (bdoc, bmod, bname, doc, mod, name) match {
         case (Some(bd), Some(bm), _, None,    None,    Some(n)) => bd ? bm ? merge(bname, n)
         case (Some(bd), _       , _, None,    Some(m), None   ) => bd ? merge(bmod, m)
         case (Some(bd), _       , _, None,    Some(m), Some(n)) => bd ? merge(bmod, m) ? n 
         case (_       , _       , _, None,    None,    None   ) => base
         case (_       , _       , _, Some(d), None,    None   ) => merge(bdoc, d)
         case (_       , _       , _, Some(d), Some(m), None   ) => merge(bdoc, d) ? m
         case (_       , _       , _, Some(d), Some(m), Some(n)) => merge(bdoc, d) ? m ? n
         case _ => throw ParseError("(" + doc + ", " + mod + ", " + name + ") cannot be resolved against " + base) 
      }
   }
   //splits uri?mod?name into (uri, mod, name)
   def toTriple(s : String) : (xml.URI, String, String) = {
      if (s.indexOf("#") != -1)
         throw new ParseError("MMT-URI may not have fragment")
      val comps = s.split("\\?",-1)
      val doc = new xml.URI(comps(0))  //note: split returns at least List(""), never Nil
      comps.length match {
         case 1 => (doc, "", "")
         case 2 => (doc, comps(1), "")
         case 3 => (doc, comps(1), comps(2))
         case _ => throw ParseError("MMT-URI may have at most two ?s: " + s)
      }
   }
   def parseS(s : String, base : Path) : SPath = parse(s,base) match {
      case p : SPath => p
      case p => throw ParseError("symbol path expected: " + p) 
   }
   def parseM(s : String, base : Path) : MPath = parse(s,base) match {
      case p : MPath => p
      case p => throw ParseError("module path expected: " + p) 
   }
   def parseD(s : String, base : Path) : DPath = parse(s,base) match {
      case p : DPath => p
      case p => throw ParseError("document path expected: " + p) 
   }
   def parseLocal(n : String) : LocalRef = {
      val relative = n.startsWith("/")
      var l = n.split("/",-1).toList
      if (l == List(""))  //note: split returns at least List(""), never Nil
         l = Nil
      if (relative)
      	l = l.drop(1)
      if (l.exists(_ == ""))
         throw ParseError("cannot parse " + n + " (local path may not have empty component or end in slash)")
      LocalRef(l, ! relative)
   }
   def parseName(n : String) : LocalPath = {
      val r = parseLocal(n)
      if (! r.absolute)
         throw ParseError("cannot parse " + n + " (local path may not start with slash)")
      r.toLocalPath
   }
}
/**
 * A Path represents an MMT path. <p>
 * An MMT path refers to a document (doc), a module (doc?mod), or a symbol (doc?mod?sym).<p>
 * See the objects ?, ??, /, \, and ! for pattern matching paths.
 */
abstract class Path {
   /** the document part of the path */
   def doc : DPath
   /** goes one step up, identity if URI-path already empty */
   def ^! : Path
   /** the list of ancestors paths starting with this path */
   def ancestors : List[Path] = if (this.^! == this) List(this) else this :: (this ^!).ancestors
   /** the last component */
   def last : String
   /** checks whether this is a prefix of that */
   def <=(that : Path) : Boolean = this == that || (that != that.^! && this <= that.^!)
   /** string representation of a Path
    *  @return the path as an MMT URI
    *  @param long add trailing ?
    */
   def toPath(long : Boolean) : String = this match {
      case DPath(uri) => uri.toString + (if (long) "??" else "")
      case doc ? name => doc.toPath + "?" + name.flat + (if (long) "?" else "")
      case mod ?? name => mod.toPath + "?" + name.flat
   }
   def toPath : String = toPath(false)
   def toPathLong : String = toPath(true)
   def toPathEscaped = scala.xml.Utility.escape(toPath)
   def toTriple : (Option[DPath], Option[LocalPath], Option[LocalPath]) = this match {
      case doc ? mod ?? name => (Some(doc), Some(mod), Some(name))
      case doc ? mod => (Some(doc), Some(mod), None)
      case doc : DPath => (Some(doc), None, None)
   }
   /** currently same as toPath, only toPath guarantees official string representation */
   override def toString = toPath
}

/**
 * A DPath represents an MMT document level path.
 * @param doc the URI of the document (may not contain query or fragment)
 */
case class DPath(uri : xml.URI) extends Path {
   //go down to a module
   def doc = this
   def last = uri.path match {case Nil | List("") => uri.uri .getAuthority case l => l.last}
   def /(n : String) = DPath(uri / n)
   def /(n : LocalPath) = DPath(n.fragments.foldLeft(uri)( _ / _))
   def ^! = DPath(uri ^)
   def ?(n : String) : MPath = this ? new LocalPath(n)
   def ?(n : LocalPath) = MPath(this, n)
   def ?(r : LocalRef) : MPath = this ? r.toLocalPath
}

/**
 * An MPath represents an MMT module level path.
 * @param parent the path of the parent document
 * @param name the name of the module
 */
case class MPath(parent : DPath, name : LocalPath) extends Path {
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
   def ?(n : LocalPath) = SPath(this,n)
   def ?(r : LocalRef) : SPath = this ? r.toLocalPath
   def ?(n : String) : SPath = this ? LocalPath(List(n))
   def components : List[Content] = List(StringLiteral(doc.uri.toString), StringLiteral(name.flat),Omitted, StringLiteral(toPathEscaped))
}

/**
 * An SPath represents an MMT symbol level path.
 * @param parent the path of the parent module
 * @param name the name of the symbol
 */
case class SPath(parent : MPath, name : LocalPath) extends Path {
   def this(doc : DPath, module : LocalPath, name : LocalPath) = this(MPath(doc, module), name)
   def doc = parent.doc
   def module = parent.name
   def last = name.fragments.last
   def /(n : LocalPath) = SPath(parent, name / n)
   /** go up to to next higher structure, stay if none */
   def ^ : SPath = if (name.isNative) this else parent ? name.init
   /** go up to containing module */
   def ^^ : MPath = parent
   /** go up to containing document */
   def ^^^ : DPath = ^^.^^
   def ^! = if (name.isNative) ^^ else ^
   def components : List[Content] = List(StringLiteral(doc.toString), StringLiteral(module.flat), StringLiteral(name.flat), StringLiteral(toPathEscaped))
}

/**
 * A LocalPath represents a local MMT module (relative to a document) or symbol (relative to a module) name.
 * @param fragments the list of (in MMT: slash-separated) components
 */
case class LocalPath(fragments : List[String]) {
   def this(n : String) = {this(List(n))}
   def flat : String = fragments.mkString("", "/","")
   def /(n: String) = LocalPath(fragments ::: List(n)) 
   def /(that: LocalPath) = LocalPath(fragments ::: that.fragments)
   def isNative = fragments.length == 1
   def init = LocalPath(fragments.init)
   def tail = LocalPath(fragments.tail)
   def head = LocalPath(List(fragments.head))
   def last = LocalPath(List(fragments.last))
   def length = fragments.length
   def prefixes : List[LocalPath] = if (length <= 1) List(this) else this :: tail.prefixes
   implicit def toList : List[String] = fragments
   override def toString = flat
}

/**
 * A LocalRef represents a qualified local possibly relative reference to an MMT module or symbol.
 * @param fragments the list of (in MMT: slash-separated) components
 * @param absolute a flag whether the reference is absolute (in MMT: starts with a slash)
 */
case class LocalRef(fragments : List[String], absolute : Boolean) {
   def toLocalPath = LocalPath(fragments)
   override def toString = fragments.mkString(if (absolute) "/" else "","/","")
}

/** 
 * This permits the syntax doc ? mod in patterns. 
 */
object ? {
   def unapply(p : Path) : Option[(DPath,LocalPath)] = p match {
      case p : MPath => Some((p ^^, p.name))
      case _ => None
   }
}

/** 
 * This permits the syntax doc ? mod ?? sym in patterns. 
 * Scala's pattern matching could also recognize a pattern "doc ? mod ? name". But then doc would have type Path, not DPath.
 */
object ?? {
   def unapply(p : Path) : Option[(MPath,LocalPath)] = p match {
      case p : SPath => Some((p ^^, p.name))
      case _ => None
   }
}

/** 
 * This permits the syntax head / tail in patterns. 
 */
object / {
   def unapply(l : LocalPath) = if (l.isNative) None else Some((l.head,l.tail))
}


/** 
 * This permits the syntax init \ last in patterns. 
 */
object \ {
   def unapply(l : LocalPath) = if (l.isNative) None else Some((l.init,l.last))
}

/** 
 * This permits the syntax !(n) to match atomic local names in patterns. 
 */
object ! {
   def unapply(l : LocalPath) = if (l.isNative) Some(l.head) else None
}

/*
object test {
def t(p : Path) = p match {
   case a ? b / c => println("?/" + a ? (b / c))
   case a ? !(b) => println("?!: " + a ? b)
   case a ? b ?? c \ d => println("???\\: " + a ? b ? (c / d))
   case a ? b ?? c => println("???: " + a ? b ? c)
   case a => println(a)
}
}
*/


