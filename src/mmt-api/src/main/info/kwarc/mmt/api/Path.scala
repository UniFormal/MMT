package info.kwarc.mmt.api
import libraries._
import modules._
import objects._
import utils._

import scala.collection.mutable.Map
import scala.util.Random

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
      // TODO URI.toString escapes DPaths containing ComplexSteps falsely
      case DPath(uri) => uri.toString + (if (long) "??" else "")
      case doc ? name => doc.toPath + "?" + name.toPath + (if (long) "?" else "")
      case mod ?? name => mod.toPath + "?" + name.toPath
      case CPath(p, c) => p.toPathLong + "?" + c
   }
   /** as toPath(false) */
   def toPath : String = toPath(false)
   /** as toPath(true) */
   def toPathLong : String = toPath(true)
   /** as toPath, but escapes XML-illegal characters */
   def toPathEscaped = scala.xml.Utility.escape(toPath)
   /** the last components of the path, human-oriented (i.e., no escaping), useful for short displays */
   def last : String
   /** breaks an MMT URI reference into its components, all of which are optional */
   def toTriple : (Option[DPath], Option[LocalName], Option[LocalName]) = this match {
      case mp ?? name =>
         (Some(mp.parent), Some(mp.name), Some(name))
      case doc ? mod => (Some(doc), Some(mod), None)
      case doc : DPath => (Some(doc), None, None)
      case c: CPath => c.parent.toTriple
   }
   /** currently same as toPath, only toPath guarantees official string representation */
   override def toString = toPath
   /** remove the component, if any */
   def dropComp: ComponentParent = this match {
      case p: ComponentParent => p
      case CPath(p,_) => p
   }
}

/** auxiliary trait to mixin convenience methods into [[Path]] classes */
trait SlashFunctions[A] {
   def /(n: LocalName): A
   def /(n: String): A = this / LocalName(n)
   def /(n: LNStep): A = this / LocalName(n)
}

/** auxiliary trait to mixin convenience methods into [[Path]] classes */
trait QuestionMarkFunctions[A] {
   def ?(n: LocalName): A
   def ?(n: String): A = this ? LocalName(n)
   def ?(ns: List[String]): A = this ? LocalName(ns:_*)
   def ?(n: LNStep): A = this ? LocalName(n)
}

/**
 * A DPath represents an MMT document level path.
 * @param uri the URI of the document (may not contain query or fragment)
 */
case class DPath(uri : URI) extends Path with ComponentParent with SlashFunctions[DPath] with QuestionMarkFunctions[MPath] {
   if (uri.query.isDefined || uri.fragment.isDefined) throw ImplementationError("MMT namespace URI may not have query or fragment")

   /** two paths are equal if their URIs are except that
     *  the scheme is ignored
     *  empty AUTH == absent AUTH
     *  AUTH/ == AUTH
     */
   override def equals(that: Any) = that match {
     case that: DPath =>
       this.uri.authority.getOrElse("") == that.uri.authority.getOrElse("") &&
       ((this.uri.authority.isDefined && this.uri.path.isEmpty) || this.uri.absolute == that.uri.absolute) &&
       this.uri.pathNoTrailingSlash == that.uri.pathNoTrailingSlash
       // query and fragment must be empty anyway
     case _ => false
   }
   /** overridden to ensure equal elements have same hash code */
   override val hashCode = (uri.authority, uri.pathNoTrailingSlash).hashCode
   
   def ^^ = DPath(uri ^!)
   /** the path of this document, this == ^^ / name */
   def name = LocalName(uri.path map {s => LNStep.parse(s, NamespaceMap.empty)})
   def doc = this
   def last = uri.path match {case Nil | List("") => uri.authority.getOrElse("") case l => l.last}
   def /(n : LocalName) = DPath(uri / n.steps.map(_.toPath))
   def ?(n : LocalName) = MPath(this, n)
   def ^ = DPath(uri ^)
   def ^! = ^
   /** if this = pref / l then Some(l) */
   def dropPrefix(pref: DPath): Option[LocalName] = {
      if (this.^^ == pref.^^)
         this.name.dropPrefix(pref.name)
      else
         None
   }
   /** if this.name != Nil then this.toMPath.toDPath == this */
   def toMPath = ^ ? LocalName(name.last)
}

/** A path to a module or symbol */
sealed trait ContentPath extends Path with ComponentParent {
   /** for GlobalName's referring to a theory-like [[Declaration]], this yields the URI of the corresponding [[Module]] */
   def toMPath : MPath
   def name: LocalName
   /** the longest MPath prefix */
   def module: MPath
}

/** A path that is not a CPath (and thus may have a component) */
//TODO should be called StructuralPath
sealed trait ComponentParent extends Path {
   def $(comp: ComponentKey) = CPath(this, comp)
   def name: LocalName
}

/**
 * An MPath represents an MMT module level path.
 * @param parent the path of the parent document
 * @param name the name of the module
 */
case class MPath(parent : DPath, name : LocalName) extends ContentPath with SlashFunctions[MPath] with QuestionMarkFunctions[GlobalName]{
   def doc = parent
   def last = name.steps.last.toString
   /** go down to a submodule */
   def /(n : LocalName) = MPath(parent, name / n)
   /** go down to a symbol */
   def ?(n : LocalName) = GlobalName(this, n)
   /** go up to containing module */
   def ^ : MPath = parent ? name.init
   /** go up to containing document */
   def ^^ : DPath = parent
   /** go up one level */
   def ^! = if (name.length <= 1) ^^ else ^
   /** the largest module containing this one */
   def mainModule: MPath = if (name.length <= 1) this else parent ? name.head
   /** if name.length == 1 then this.toDPath.toMPath == this */
   def toDPath = parent / name
   /** this.toMPath == this */
   def toMPath = this
   /** if this.name != Nil then this.toGlobalName.toMPath == this */
   def toGlobalName = ^ ? LocalName(name.last)
   def module = this
   /** the super module if this is a nested module */
   def superModule = if (name.length <= 1) None else Some(this ^)
}

/**
 * A GlobalName represents the MMT URI of a symbol-level declaration.
 * This includes induced declarations.
 */
case class GlobalName(module: MPath, name: LocalName) extends ContentPath with SlashFunctions[GlobalName] {
   def doc = module.doc
   def ^! = if (name.length == 1) module else GlobalName(module, name.init)
   def /(n : LocalName) = GlobalName(module, name / n)
   def last = name.last.toString
   def apply(args: List[Term]) : Term = OMA(OMS(this), args)
   def apply(args: Term*) : Term = apply(args.toList)
   def apply(con: Context, args: List[Term]) : Term = OMBINDC(OMS(this), con, args)
   def apply(subs: Substitution, con: Context, args: List[Term]) : Term = ComplexTerm(this, subs, con, args)
   /** true iff each include step is simple */
   def isSimple : Boolean = name.steps.forall(_.isInstanceOf[SimpleStep])
   /** if name.length == 1 then this.toMPath.toGlobalName == this */
   def toMPath = module / name
   /** turns module into [[ComplexStep]] */
   def toLocalName = ComplexStep(module) / name

   def mapName(op: LocalName => LocalName): GlobalName = this.copy(name = op(name))
}

object LocalName {
   def apply(step: LNStep) : LocalName = LocalName(List(step))
   def apply(steps: String*) : LocalName = LocalName(steps.toList map SimpleStep)
   def apply(p: MPath) : LocalName = LocalName(ComplexStep(p))
   implicit def toList(ln: LocalName): List[LNStep] = ln.steps
   implicit def fromList(l: List[LNStep]): LocalName = LocalName(l)
   /** parses a LocalName, complex segments are parsed relative to base */
   def parse(s: String, nsMap : NamespaceMap): LocalName = LocalRef.parse(s).toLocalName(nsMap)
   def parse(s:String): LocalName = parse(s, NamespaceMap.empty)
   val empty: LocalName = LocalName(Nil)

   /**
     * Generates a random LocalName of the form ''LocalName(prefix_<random alphanumeric string>)''.
     *
     * Of course, the generated name might still clash with some other name pre-existing, but this
     * scenario is highly improbable.
     *
     * @param prefix Some string prefix
     * @param length The length of the random alphanumeric string suffix
     */
   def random(prefix: String = "", length: Int = 15): LocalName = {
      LocalName(s"${prefix}_${Random.alphanumeric.dropWhile(_.isDigit).take(length).mkString}")
   }
}

/**
 * A LocalName represents a local MMT symbol-level declarations (relative to a module).
 * @param steps the list of (in MMT: slash-separated) components
 */
case class LocalName(steps: List[LNStep]) extends SlashFunctions[LocalName] {
   def /(n: LocalName) : LocalName = LocalName(steps ::: n.steps)
   def init: LocalName = LocalName(steps.init)
   /**
    * @return if this == p / l, then Some(l), else None
    */
   def dropPrefix(p: LocalName): Option[LocalName] =
      if (steps.startsWith(p.steps)) Some(this.drop(p.length))
      else None
   /** removes repeated complex steps, keeping the later one */
   def simplify: LocalName = {
      var complexBefore = false
      val stepsRS = steps.reverse filter {
         case s: SimpleStep =>
           complexBefore = false
           true
         case c: ComplexStep =>
            val res = ! complexBefore
            complexBefore = true
            res
      }
      LocalName(stepsRS.reverse)
   }
   /** the identity substitution this->this */
   def id = Sub(this, OMV(this))
   /** returns the list of all prefixes of this name, from atomic to this one */
   def prefixes : List[LocalName] = if (this.length <= 1) List(this) else init.prefixes ::: List(this)
   /** removes all complex steps; possibly ambiguous, but often much nicer for priting/parsing */
   def dropComplex = LocalName(steps.filterNot(_.isInstanceOf[ComplexStep]))
   /** machine-oriented string representation of this name, parsable and official */
   def toPath : String = steps.map(_.toPath).mkString("", "/", "")
  /** human-oriented string representation of this name, no encoding, possibly shortened */
   override def toString : String = toStr(false)
   def toStr(implicit shortURIs: Boolean) = steps.map(_.toStr).mkString("", "/", "")

   def mapLast(f: SimpleStep => SimpleStep): LocalName = {
      val newSteps: List[LNStep] = steps match {
         case beginning :+ (step @ (_: SimpleStep)) => beginning :+ f(step)
         case _ => steps
      }
      LocalName(newSteps)
   }

   def suffixLastSimple(suffix: String): LocalName = mapLast(
      step => SimpleStep(step.name + suffix)
   )

   def prefixOrCreateLastSimpleStep(prefix: String): LocalName = {
      val newSteps: List[LNStep] = steps match {
         case beginning :+ SimpleStep(name) => beginning :+ SimpleStep(prefix + name)
         case List(ComplexStep(mpath)) => List(ComplexStep(mpath.parent ? mpath.name.prefixOrCreateLastSimpleStep(prefix)))
         case _ => steps :+ SimpleStep(prefix)
      }
      LocalName(newSteps)
   }
}

/** a step in a LocalName */
abstract class LNStep {
   def toPath : String
   def unary_! = LocalName(this)
   def /(n: LocalName) = LocalName(this) / n
   def /(n: LNStep) = LocalName(this) / n
   override def toString = toStr(false)
   def toStr(implicit shortURIs: Boolean): String
}

object LNStep {
   def parse(s: String, nsMap: NamespaceMap) : LNStep = {
      if (s.startsWith("[") && s.endsWith("]")) {
         val ns = s.substring(1,s.length - 1)
         if (ns.startsWith("(") && ns.endsWith(")"))
            ComplexStep(Obj.fromPathEncoding(xml.decodeURI(s)).toMPath)
         else ComplexStep(Path.parseM(xml.decodeURI(ns), nsMap))
      } else
         SimpleStep(xml.decodeURI(s))
   }
   val empty = SimpleStep("")
}

/** constant or structure declaration */
case class SimpleStep(name: String) extends LNStep {
   def toPath = xml.encodeURI(name)
   def toStr(implicit shortURIs: Boolean) = name
}
/** an include declaration; ComplexStep(fromPath) acts as the name of an unnamed structure */
case class ComplexStep(path: MPath) extends LNStep {
   def toPath = "[" + path.toPath + "]"
   def toStr(implicit shortURIs: Boolean) = "[" + path.name.toStr + "]"
}

case class CPath(parent: ComponentParent, component: ComponentKey) extends Path {
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
   def toLocalName(nsMap : NamespaceMap) = {
      val steps = segments map {s => LNStep.parse(s, nsMap)}
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
      def segmentDone: Unit = {seen ::= current; current = ""}
      //called when the next character is appended to the current segment
      def charDone: Unit = {current = current + left(0); left = left.substring(1)}
      //parses s segment-wise; if a segment starts with [, pass control to complex
      def start: Unit = {   if (left == "")            {if (current != "" || ! seen.isEmpty) segmentDone}
               else if (left.startsWith("[") && current == "" && {
                  val p = left.indexOf("]")
                  p != -1 && (left.length == p+1 || (left.length > p+1 && left(p+1) == '/'))
               })
                                               {complex}
               else if (left.startsWith("/"))  {segmentDone; left = left.substring(1); start}
               else                            {charDone; start}
      } //TODO accept only balanced nestings of []?
      //parses a complex segment of the form [URI] (assumes [ has been parsed already)
      def complex: Unit = {if (left.isEmpty)           {throw ParseError("unclosed '[' in " + s)}
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

   def parse(s : String) : Path = parse(s, NamespaceMap.empty)
   /** parses an MMT-URI reference into a triple and then makes it absolute */
   def parse(s : String, nsMap : NamespaceMap) : Path = {
      val (d,m,n,c) = split(s)
      parse(d,m,n,c,nsMap)
   }
   // merge(x,y) merges the URI or LocalPath x with the relative or absolute reference y
   private def mergeD(bd : Option[DPath], d : DPath) : DPath = if (bd.isEmpty) d else DPath(bd.get.uri.resolve(d.uri))
   private def mergeN(nsMap : NamespaceMap, bl : Option[LocalName], l : LocalRef) : LocalName =
      if (bl.isEmpty || l.absolute)
         l.toLocalName(nsMap)
      else
         bl.get / l.toLocalName(nsMap)
   /** turns an MMT-URI reference (d,m,n) into an MMT-URI relative to base (omitting a component is possible by making it empty) */
   def parse(dS : String, m : String, n : String, comp: String, nsMap : NamespaceMap) : Path = {
      val base = nsMap.base
      //to make the case distinctions simpler, all omitted (= empty) components become None
      val docOpt = if (dS == "") None else Some(dS)
      val doc = docOpt.map {s =>
         val sExp = nsMap.expand(s)
         val d = uriParseCache(sExp)
         DPath(d)
      }
      def wrap(l : LocalRef) = if (l.segments.isEmpty) None else Some(l)
      val mod = wrap(LocalRef.parse(m))
      val name = wrap(LocalRef.parse(n))
      //get the base as a triple of three Options (first component will never be None)
      val (bdoc, bmod, bname) = base.toTriple
      //now explicit case distinctions to be sure that all cases are covered
      val path = (bdoc, bmod, bname, doc, mod, name) match {
         case (Some(bd), Some(bm), _, None,    None,    Some(n)) => bd ? bm ? mergeN(nsMap, bname, n)
         case (Some(bd), _       , _, None,    Some(m), None   ) => bd ? mergeN(nsMap, bmod, m)
         case (Some(bd), _       , _, None,    Some(m), Some(n)) => bd ? mergeN(nsMap, bmod, m) ? n.toLocalName(nsMap)
         case (_       , _       , _, None,    None,    None   ) => base
         case (_       , _       , _, Some(d), None,    None   ) => mergeD(bdoc, d)
         case (_       , _       , _, Some(d), Some(m), None   ) => mergeD(bdoc, d) ? m.toLocalName(nsMap)
         case (_       , _       , _, Some(d), Some(m), Some(n)) => mergeD(bdoc, d) ? m.toLocalName(nsMap) ? n.toLocalName(nsMap)
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
            case '[' if current == 2 || current == 1 =>
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
   def parseC(s : String, nsMap : NamespaceMap) : CPath = parse(s,nsMap) match {
      case p : CPath => p
      case p => throw ParseError("component path expected: " + p)
   }
   /** as parse but fails if the result is not a symbol level URI */
   def parseS(s : String, nsMap : NamespaceMap = NamespaceMap.empty) : GlobalName = parse(s,nsMap) match {
      case p : GlobalName => p
      case p => throw ParseError("symbol path expected: " + p)
   }
   /** as parse but fails if the result is not a module level URI */
   def parseM(s : String, nsMap : NamespaceMap = NamespaceMap.empty) : MPath = parse(s,nsMap) match {
      case p : MPath => p
      case p => throw ParseError("module path expected: " + p)
   }
   /** as parse but fails if the result is not a document level URI */
   def parseD(s : String, nsMap : NamespaceMap) : DPath = parse(s,nsMap) match {
      case p : DPath => p
      case p => throw ParseError("document path expected: " + p)
   }
   def parseMS(s : String, nsMap : NamespaceMap) : ContentPath = parse(s,nsMap) match {
      case p : ContentPath => p
      case p => throw ParseError("document path expected: " + p)
   }
   def fromURI(s : URI,nsmap : NamespaceMap) : Path = parse(s.toString,nsmap) // TODO properly
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
      case GlobalName(mp, n) => Some((mp,n))
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
